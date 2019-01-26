(library (Flax defaults)
         (export default-process-layer
                 default-readers
                 simple-readers
                 default-builders)
         (import (scheme)
                 (Flax structure)
                 (Flax utils)
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
         (module simple-readers
                 [sxml-reader
                   default-readers]

                 (import type-reader)
                 ;;;; define simple reader  ;;;
                 ;; sxml-reader
                 ;; take the file as the scheme code and then read it
                 (define sxml-reader
                   (make-reader (make-reader-matcher "sxml")
                                (lambda (file)
                                  (let ((contents (load (get-absolute-path file))))
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

                 (define default-readers (list sxml-reader )))
         (import simple-readers)

         (define default-builders)



         )
