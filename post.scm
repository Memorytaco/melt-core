(library (Flax post)
  (export read-post
          post-ref
		  register-metadata-parser!
		  read-metadata-headers
		  parse-metadata)
  (import (scheme)
          (Flax structure)
          (Flax utils)
          (Flax srfi match)
          (Flax reader reader))
  
  (import type-post)
  (import type-reader)
  
  
  ;; read one post and return post object
  (define (read-post file-name reader-list)
    (let ((reader (reader-available? reader-list file-name)))
      (if reader
          (let-values (((meta-data content) ((reader-proc reader) file-name)))
            (make-post file-name meta-data content))
          (begin
			(display "unsupportet file format\n")
			'()))))
  
  (define (post-ref key post)
	(display "im in post-ref")
    (assq-ref key (post-metadata post)))
  
  
  ;; read meta data
  (define %metadata-parsers
    (make-eq-hashtable))
  
  (define (register-metadata-parser! name parser)
    (hashtable-set! %metadata-parsers name parser))
  
  (define (get-metadata-parser key)
    (hashtable-ref %metadata-parsers key identity))
  
  (define (parse-metadata key value)
    ((get-metadata-parser key) value))
  
  (define (read-metadata-headers port)
    (define (string-trim-both string)
      (string-trim string 'both))
	
	(display "in read metadata headers\n")
	(let ((metadata '()))
	  (do ((line (get-line port) (get-line port))
		   (end? #f))
		  ((or end? (eof-object? line))
		   (if (not end?)
			   (error (port-name port)
					  "end of file while readig metadata: " )))
		(cond
		 [(string=? line "---")
		  (set! end? #t)]
		 [else
		  (let-values (((key value) (values (car (map string-trim-both (string-split-dual line #\:)))
											(car (cdr (map string-trim-both (string-split-dual line #\:)))))))
			(set! metadata (alist-cons (string->symbol key) (parse-metadata (string->symbol key) value) metadata)))]))
	  metadata))
  
  ;; two new parser
  (register-metadata-parser!
   'type
   (lambda (str)
     (string->symbol str)))
  
  (register-metadata-parser!
   'depth
   (lambda (str)
     (string->number str)))
  
  )
