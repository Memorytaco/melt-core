#!chezscheme
(library (Flax defaults)
         (export process-layer-module
                 reader-module
                 builder-module)
         (import (scheme)
                 (Flax structure)
                 (Flax utils)
                 (Flax invoke)
                 (Flax reader reader)
                 (Flax reader sxml))


         ;; process-layers
         (module process-layer-module
                 (default-process-layer)

                 (import type-process)
                 (import type-post)

                 ;; return the procedure matches the key
                 ;; from the process layer
                 (define procedure-layer-assq
                   (lambda (key process-layer)
                     (process-procedure (cdr (assq key process-layer)))))

                 (define post-ref
                   (lambda (key post)
                     (cdr (if (assq key (post-metadata post))
                              (assq key (post-metadata post))
                              '(#t . #f)))))

                 ;; -------------------the default process layer-------------------------------
                 ;; this template require these keywords in post
                 ;; img    -- the preview image path, from the index file to that image, pure String
                 ;; title  -- the title of the post, pure String
                 ;; author -- Your name, pure String
                 ;; date   -- the date the post posted, Pure String
                 ;; tag    -- The type of the post, Pure String
                 ;; this will show later in the end of the file
                 (define default-meta-process
                   (make-process 'meta
                                 (lambda (process-layer post)
                                   ((procedure-layer-assq 'index process-layer)
                                    (assq-ref 'post process-layer)
                                    post))))

                 (define default-index-process
                   (make-process 'index
                                 (lambda (process post)
                                   `(html (head)
                                          (body ,((process-procedure process) post))))))

                 (define default-post-process
                   (make-process 'posts
                                 (lambda (post)
                                   `(div (@ (style "display:block;"))
                                         ,(if (eq? '() (post-ref post 'img))
                                              '()
                                              `(img (@ (src ,(post-ref post 'img)))))
                                         (hr)
                                         (h3 ,(if (eq? '() (post-ref post 'title))
                                                  "title"
                                                  (post-ref post 'title)))
                                         (span ,(if (eq? '() (post-ref post 'author))
                                                    "anonymous"
                                                    (post-ref post 'author))
                                               ,(if (eq? '() (post-ref post 'date))
                                                    "today"
                                                    (post-ref post 'date)))
                                         (div ,(if (eq? '() (post-ref post 'tag))
                                                   "none"
                                                   (let ((span-tag (lambda (tag-list output-list)
                                                                     (cons `(span ,(car tag-list))
                                                                           output-list)))
                                                         (current-list '())
                                                         (tag-list (post-ref post 'tag)))
                                                     (while (not (eq? '() tag-list))
                                                            (set! current-list (span-tag tag-list current-list))
                                                            (set! tag-list (cdr tag-list)))
                                                     current-list))))
                                   `(div ,(post-ref post 'content)))))

                 ;; require each post has these metadatas
                 ;; title if none output "title" -- title is a string
                 ;; author if none output "anonymous" -- author is a string
                 ;; date if none output "today" -- date is a string
                 ;; img if none don't display the image -- img is a src string
                 ;; tag if none output "none" -- tag is a !!string list!!
                 ;; content -- must have! otherwise you will get one post without content
                 (define default-process-layer
                   `((meta . ,default-meta-process)
                     (index . ,default-index-process)
                     (post . ,default-post-process))))
         (import process-layer-module)


         ;;; readers
         (module reader-module
                 [sxml-reader
                   default-readers]

                 (import type-reader)
                 ;;;; define simple reader  ;;;
                 ;; sxml-reader
                 ;; take the file as the scheme code and then read it
                 (define sxml-reader
                   (make-reader (make-reader-matcher "sxml")
                                (lambda (file-name cache-directory)
                                  (let ((contents ((lambda (file-name)
                                                     (define fasl (string-append cache-directory (string (directory-seperator)) (basename file-name)))
                                                     (fasl-file file-name fasl)
                                                     (define port (open-file-input-port fasl))
                                                     (define content (fasl-read port))
                                                     (close-input-port port)
                                                     content) file-name)))
                                    (values (alist-delete 'content contents eq?) ;; return the metadata list
                                            (cdr (assq contents 'content)))))))    ;; return the list

                 ; not available now!!
                 ; (define commonmark-reader
                 ;   (make-reader (make-reader-matcher "md")
                 ;                (lambda (file)
                 ;                  (call-with-input-file file
                 ;                                        (lambda (port)
                 ;                                          (values (read-metadata-headers port)
                 ;                                                  (commonmark->sxml port)))))))

                 (define default-readers (list sxml-reader)))

         (module builder-module
                 [default-builder]

                 (define (build site)
                   )
                 )

         (module chain-module
                 [default-chain]

                 (define default-chain (init-chain #t (lambda ()
                                                        (display "Building ...\n"))
                                                   (init-empty-data)))

                 (add-hook default-chain (create-hook 'site 'data (create-data ())))
                 (add-hook default-chain (create-hook 'data (create-data '(posts-directory build-directory asset process-layer readers)
                                                                         (list ))))

                 )

         )
