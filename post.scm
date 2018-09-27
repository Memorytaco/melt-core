(define-module (Flax post)
  #:use-module (Flax utils)
  #:use-module (Flax reader)

  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)


  #:export (make-post
            is-post?
            get-post-file-name
            get-post-metadata
            get-post-sxml
	    set-post-sxml
            
	    read-post
	    post-ref))


;; ~metadata~ is an alist
;; ~sxml~ is a sxml tree
(define-record-type <post>
  (make-post file-name metadata sxml)
  is-post?
  (file-name get-post-file-name)
  (metadata get-post-metadata)
  (sxml get-post-sxml set-post-sxml))

;; read one post and return post object
(define* (read-post file-name #:key (reader-list (list sxml-reader)))
  (if (eq? reader-list '())
      (begin
       (format (current-error-port) "Unmatched file : ~a ~%" file-name)
       '())
      (if ((get-reader-matcher (car reader-list)) file-name)
	  (let-values (((meta-data content) ((get-reader-proc (car reader-list)) file-name)))
	    (make-post (basename file-name) meta-data content))
	  (read-post file-name #:reader-list (cdr reader-list)))))

(define (post-ref post key)
  (assq-ref (get-post-metadata post) key))
