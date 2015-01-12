(module main racket/base
  (require racket/tcp)
  (provide run-server read-all-chars run)

  #|

  IO STUFFS

  |#

  (define-syntax on-multiple
    (syntax-rules ()
      ((_ on thing)
       (on thing))
      ((_ on thing things ...)
       (begin 
	 (on thing) 
	 (on-multiple on things ...)))))

  (define-syntax with-flush
    (syntax-rules ()
      ((_ (ports ...) body ...)
       (begin
	 body ...
	 (on-multiple flush-output ports ...)))))

  (define (safe-read-char port)
    (and (char-ready? port) (read-char port)))

  (define (read-all-chars port)
    (let ([out-string (open-output-string)])
      (let loop ([c (safe-read-char port)])
	(when c
	  (display c out-string)
	  (loop (safe-read-char port))))
      (get-output-string out-string)))

  #|

  USER STUFFS
  
  |#

  (struct session
    ([in-port]
     [out-port]
     [input-fn #:mutable]
     [input-handler-fn #:mutable]
     [id #:mutable]
     [closed? #:mutable]))

  (define (get-input! session)
    ((session-input-fn session) session))

  (define (handle-input! session input-str quit-box)
    ((session-input-handler-fn session) session input-str quit-box))

  (define (get-handle-input! session quit-box)
    (printf "~s~%~%" session)
    (handle-input! session (get-input! session) quit-box))
  
  ; close a connection to the in and out ports of a user
  (define (kill-connection! session)
    (close-output-port (session-out-port session))
    (close-input-port (session-in-port session)))

  ; accept on a tcp listener and ruturn a user that 
  (define (make-accept-session server input-fn input-handler-fn)
    (let-values ([(in out) (tcp-accept server)])
      (session in out input-fn input-handler-fn "" #f)))

  (define (get-event session)
    (handle-evt (session-in-port session) (lambda (evt) session)))

  (define (get-events sessions)
    (let loop ([sessions sessions])
      (if (pair? sessions)
	  (cons (get-event (car sessions)) (loop (cdr sessions)))
	  '())))

  (define (remove-closed sessions)
    (filter (lambda (session) (not (session-closed? session))) sessions))

  #|

  String manipulation stuff
  
  |#
      
  (define (whitespace? char)
    (or (eq? char #\space)
	(eq? char #\newline)))

  (define (remove-whitespace str)
    (list->string (filter (lambda (ch) (not (whitespace? ch))) (string->list str))))

  #|

  SERVER STUFFS

  |#

  (define (make-server port)
    (tcp-listen port 4 #t))

  (define (run-server port default-input-fn 
		      default-input-handler-fn idle-fn)
    (let loop ([server (make-server port)]
	       [sessions (list)]
	       [quit? (box #f)])
      (if (unbox quit?)
	  (values sessions server)
	  (begin
	    (idle-fn sessions quit?)
	    (when (tcp-accept-ready? server)
	      (set! sessions 
		    (cons 
		     (make-accept-session server default-input-fn
					  default-input-handler-fn)
		     sessions)))
	    (and (pair? sessions)
		 (get-handle-input! (apply sync (get-events sessions)) quit?))
	    (set! sessions (remove-closed sessions))
	    (loop server sessions quit?)))))
     
  (define (build-run-close-server port 
		      (default-input-fn (lambda (session)
					  ""))
		      (default-input-handler-fn (lambda (session input quit-box)
						  #t))
		      (idle-fn (lambda (sessions quit-box)
				 (with-flush
				  ((current-output-port))
				  (display "Cycled\n"))
				 (set-box! quit-box #t))))
    (let-values ([(sessions server) 
		  (run-server port 
			      default-input-fn
			      default-input-handler-fn 
			      idle-fn)])
      (for ([s (in-list sessions)])
	(kill-connection! s))
      (tcp-close server)))

  (define (run)
    (build-run-close-server 12345
		(lambda (session)
		  (read-all-chars (session-in-port session)))
		(lambda (session input quit-box)
		  (if (equal? "quit\n" input)
		      (set-box! quit-box #t)
		      (with-flush
		       ((session-out-port session)
			(current-output-port))
             (fprintf (session-out-port session) "~a" input))))
		       ;(if (string->number (remove-whitespace input))
			   ;(fprintf (session-out-port session) "~a" input)
			   ;(fprintf (session-out-port session) "NaN~%")))))
		(lambda (sessions quit-box)
		  (when (pair? sessions)
		    (printf "Cycled ~s~%" sessions))
		  (flush-output (current-output-port)))))
	(run)
  )
