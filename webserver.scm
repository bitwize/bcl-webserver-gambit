
(define CR 13)
(define LF 10)

;;; Read a line up to the first CRLF pair from a port.

(define (read-to-crlf port)
  (let loop ((c (read-u8 port))
	     (v (make-u8vector 1024))
	     (p 0))
    (let* ((vl (u8vector-length v)))
      (cond
       ((eof-object? c)
	c)
       ((>= p vl)
	(let* ((v2 (make-u8vector (* vl 2))))
	  (u8vector-copy! v2 0 v 0 p)
	  (loop c v2 p)))
       ((= c CR)
	(let* ((c2 (peek-u8 port)))
	  (if (= c2 LF)
	      (begin
		(read-u8 port) ; eat the '\n'
		(if (= p vl)
		    v
		    (subu8vector v 0 p)))
	      (begin
		(u8vector-set! v p c)
		(loop (read-u8 port) v (+ p 1))))))
       (else
	(u8vector-set! v p c)
	(loop (read-u8 port)
	      v
	      (+ p 1)))))))

;;; Read a list of HTTP headers from a port. Stops on first blank
;;; line.  Raises an error of the connection closes before the first
;;; blank line is reached.

(define (read-headers port)
  (let loop ((ln (read-to-crlf port))
	     (lns '()))
    (cond
     ((eof-object? ln) (error "Connection terminated before headers read"))
     ((= (u8vector-length ln) 0) (reverse! lns))
     (else
      (loop (read-to-crlf port)
	    (cons ln lns))))))

(define (write-http-response code status type body port)
  (let* ((l (u8vector-length body))
	 (s (with-output-to-string
	      (lambda ()
		(display "HTTP/1.1 ")
		(display code)
		(display " ")
		(display status)
		(display "\r\nContent-Type: ")
		(display type)
		(display "\r\nContent-Length: ")
		(display l)
		(display "\r\n\r\n"))))
	 (h (string->utf8 s)))
    (write-subu8vector h 0 (u8vector-length h) port)
    (write-subu8vector body 0 l port)
    (force-output port)
    (close-port port)))

(define (get-fs-path path dir)
  (if (not
       (and (> (string-length path) 0)
	    (char=? (string-ref path 0) #\/)))
      (error "invalid path"))
  (let* ((file-path (path-normalize (substring path 1 (string-length path)) #f dir)))
    (if (not
	 (string=? (substring file-path 0 (string-length dir))
		   dir))
	(error "invalid path"))
    file-path))

(define (parse-request line)
  (define (split sep str)
      (call-with-input-string
       str
       (lambda (p)
         (read-all p (lambda (p) (read-line p sep))))))
  (let* ((result (filter
		  (lambda (x) (not (zero? (string-length x))))
		  (split #\space line))))
    (cond ((not (= (length result) 3))
	   (error "Invalid request"))
	  (else
	   (values (car result)
		   (cadr result)
		   (caddr result))))))

(define (handle-connection! port dir)
  (with-exception-handler
   (lambda (e)
     (if (error-exception? e)
	 (begin
	   (display
	    (string-append "Error: " (error-exception-message e) "\n")
	    (current-error-port))
	   (write-http-response 500 "Internal Server Error" "text/plain" (string->utf8 "Internal Server Error") port)
	 (error "Caught exception:" e))))
   (lambda ()
     (let* ((line (utf8->string (read-to-crlf port)))
	    (headers (map utf8->string (read-headers port))))
       (call-with-values
	   (lambda () (parse-request line))
	 (lambda (method path version)
	   (display (string-append
		     method
		     " "
		     path
		     " "
		     version
		     "\n")
		    (current-error-port))
	   (cond
	    ((not (string=? (substring version 0 7) "HTTP/1."))
	     (error "Invalid HTTP version"))
	    ((string=? method "GET")
	     (let* ((fn (get-fs-path path dir)))
	       (display (string-append
			 "getting file: "
			 fn
			 "\n")
			(current-error-port))
			
	       (with-exception-catcher
		(lambda (e)
		  (cond
		   ((permission-denied-exception? e)
		    
		    (write-http-response
		     403 "Forbidden" "text/plain" (string->utf8 "Forbidden") port))
		   ((no-such-file-or-directory-exception? e)
		    (write-http-response
		     404 "Not Found" "text/plain" (string->utf8 "Not Found") port))
		   (else (raise e))))
		(lambda ()
		  (let* ((contents (read-file-u8vector fn)))
		    (write-http-response 200 "OK" "text/plain" contents port))))))
	    (else
	     (error "Invalid request"))))))))) 

(define (web-server port dir)
  (let* ((serv-port (open-tcp-server port)))
    (let loop ((p (read serv-port)))
      (display "got connection\n" (current-error-port))
      (thread (lambda () (handle-connection! p dir)))
      (loop (read serv-port)))))
(web-server 8080 "/tmp/webserver/")
