#!chezscheme
(library (melt lib commonmark)
  (export commonmark->sxml
		  split-paragraph)
  (import (scheme)
		  (melt utils))

  ;;-----------------------------------------------define record ###########
  ;; define the media type
  (define-record-type token
	(nongenerative melt-token)
	(fields
	 ;; type is a symbol
	 (immutable type token-type)
	 ;; attr is an list and each element is a list
	 ;; which has two elements
	 (immutable attr token-attr)
	 ;; the content
	 (immutable cont token-cont)))

  ;;-----------------------------------------------define key words ########
  ;; define special characters
  (define &&key-words (alist->hash-table
					   '((#\* . star)
						 (#\_ . under-score)
						 (#\- . minus)
						 (#\+ . plus)
						 (#\! . she)
						 (#\# . sharp)
						 (#\newline . newline)
						 (#\` . back-quote))))

  ;; transform one token to an sxml tag
  (define (transform-token token)
	  (define (attr-expand t-list)
		(if (null? t-list)
			'()
			(cons '@ t-list)))
	  (if (token-attr token)
		  (if (token-cont token)
			  `(,token-name ,(attr-expand (token-attr token)) ,(token-cont token))
			  `(,token-name ,(attr-expand (token-attr token))))
		  (list token-name)))
  
  ;; judge whether a line is a blank line
  ;; the line doesn't contain a newlne character
  (define (blank-line? line)
	(call/cc (lambda (cc)
			   (do ((char-list (string->list line) (cdr char-list)))
				   ((null? char-list) #t)
				 (let ((char (car char-list)))
				   (if (not
						(or (equal? #\tab char)
							(equal? #\space char)))
					   (cc #f)))))))
  
  ;; read one file, generate a list of paragraphs
  (define (split-paragraph path)
	(call-with-port
	 (open-file-input-port path (file-options) (buffer-mode block) (native-transcoder))
	 (lambda (port)
	   (define paragraphs '())
	   (do ((paragraph '() (append paragraph line))			
			(line (get-line port) (get-line port)))
		   ((eof-object? line)
			(set! paragraphs (append paragraphs (list paragraph)))
			paragraphs)
		 (display line)
		 (if (blank-line? line)
			 (begin
			   (set! paragraphs (append paragraphs (list paragraph)))
			   (set! paragraph '())
			   (set! line '()))
			 (set! line (list line)))))))

  (define tokenize
	(lambda (line-list)
	  `(p )))

  (define commonmark->sxml
	(lambda ()
	  '3))

  )
