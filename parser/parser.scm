(library (Flax parser parser)
  (export make-reader-matcher
          reader-available?)
  (import (scheme)
          (Flax structure)
          (Flax utils))
  
  (import type-reader)
  
  ;; make file extension matcher
  ;; return the function which returns #t if the file's
  ;; extension is ext
  (define make-parser-matcher
    (lambda (ext)
      (lambda (path)
        (string=? ext (path-extension path)))))
  
  ;; return the reader if the extension is
  ;; matched or #f when reader is not available
  (define parser-query
    (lambda ())
(lambda (reader-list name)
      (display "in reader-available!\n")
	  (cond
       [(eq? '() reader-list)
        #f]
       [(atom? reader-list)
        (if ((reader-matcher reader-list) name)
            reader-list
            #f)]
       [(list? reader-list)
        (let ((exists? #f))
		  (do ((iterate-list reader-list (cdr iterate-list)))
              ((null? iterate-list) exists?)
			(if ((reader-matcher (car iterate-list)) name)
				(set! exists? (car iterate-list)))))]
       [else #f])))
  
  )
