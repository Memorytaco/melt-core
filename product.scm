#!chezscheme
(library (Flax product)
  (export process-layer-module
          reader-module)
  (import (scheme)
          (Flax structure)
          (Flax utils)
		  (Flax process)
		  (Flax post)
		  (Flax asset)
          (Flax reader reader)
          (Flax reader sxml))
  
  
  ;; process-layers
  (module process-layer-module
          (process-layer)
		  
          (import type-process)
          (import type-post)
		  
          ;; return the procedure matches the key
          ;; from the process layer
          (define procedure-layer-assq
            (lambda (key process-layer)
              (process-procedure (cdr (assq key process-layer)))))

		  ;; query post metadata, use assq to get the assoc data match key
		  ;; otherwise return #f
          (define post-ref
            (lambda (key post)
              (cdr (if (assq key (post-metadata post))
                       (assq key (post-metadata post))
                       '(#t . #f)))))
		  
          ;; -------------------the default process layer------------------
          ;; this template require these keywords in post
          ;; img    -- the preview image path, from the index file to that image, pure String
          ;; title  -- the title of the post, pure String
          ;; author -- Your name, pure String
          ;; date   -- the date the post posted, Pure String
          ;; tag    -- The type of the post, Pure String
          ;; this will show later in the end of the file
          (define meta-process
            (make-process 'meta
                          (lambda (process-layer post)
                            ((procedure-ref 'index process-layer)
                             (procedure-ref 'post process-layer)
                             post))))
		  
          (define index-process
            (make-process 'index
                          (lambda (procedure post)
                            `(html (head this is title)
                                   (body ,(procedure post))))))
		  
          (define post-process
            (make-process 'post
                          (lambda (post)
							`(div (@ (style "display:block;"))
                                  (img (@ (src ,(post-ref 'img post))))
                                  (hr)
                                  (h3 ,(post-ref 'title post))
                                  (span ,(if (eq? '() (post-ref 'author post))
                                             "anonymous"
                                             (post-ref 'author post))
                                        ,(if (eq? '() (post-ref 'date post))
                                             "today"
                                             (post-ref 'date post)))
                                  (div ,(if (eq? '() (post-ref 'tag post))
                                            "none"
                                            (let ((span-tag (lambda (tag-list output-list)
                                                              (cons `(span ,(car tag-list))
                                                                    output-list)))
                                                  (current-list '())
                                                  (tag-list (post-ref 'tag post)))
                                              (while (not (eq? '() tag-list))
                                                (set! current-list (span-tag tag-list current-list))
                                                (set! tag-list (cdr tag-list)))
                                              current-list)))
								  (div ,(post-ref 'content post))))))
		  
          ;; require each post has these metadatas
          ;; title if none output "title" -- title is a string
          ;; author if none output "anonymous" -- author is a string
          ;; date if none output "today" -- date is a string
          ;; img if none don't display the image -- img is a src string
          ;; tag if none output "none" -- tag is a !!string list!!
          ;; content -- must have! otherwise you will get one post without content
          (define process-layer
            `((meta . ,meta-process)
              (index . ,index-process)
              (post . ,post-process))))
    
  ;; readers
  (module reader-module
          [sxml-reader
		   commonmark-reader]
		  
          (import type-reader)
          ;; define simple reader  ;;;
          ;; sxml-reader
          ;; take the file as the scheme code and then read it
          (define sxml-reader
            (make-reader (make-reader-matcher "sxml")
                         (lambda (file-name)
                           (let ((contents ((lambda (file-name)
                                              (define fasl (string-append (cd) "/cache" (directory-separator-string) (basename file-name)))
                                              (define port (if (file-exists? fasl)
															   (open-file-input-port fasl)
															   (begin
																 (mkdir-r (string-append (cd) "/cache"))
																 (fasl-file file-name fasl)
																 (open-file-input-port fasl))))
                                              (define content (eval (fasl-read port)))
                                              (close-input-port port)
                                              content) file-name)))
                             (values (alist-delete 'content contents) ;; return the metadata list
                                     (cdr (assq 'content contents)))))))    ;; return the list

		  ;; for the temple commonmark
		  (define (commonmark->sxml port)
			(display "in commonmark->sxml\n\n")
			(do ((line (get-line port) (get-line port))
				 (strings '()))
				((eof-object? line) (display (apply string-append strings))
				 `((content . ,(apply string-append strings))))
			  (set! strings (append strings (list (string-append line "\n"))))))

		  ;; not available now!!
		   (define commonmark-reader
		     (make-reader (make-reader-matcher "md")
		                  (lambda (file)
		                    (call-with-input-file file
							  (lambda (port)
								(values 
								 (commonmark->sxml port)
								 (read-metadata-headers port)))))))
		   
		   )
  )
