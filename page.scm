(library (Flax page)
         (export page)
         (import (scheme)
                 (Flax structure)
                 (Flax srfi match)
                 (Flax utils)
                 (Flax asset)
                 (Flax reader sxml)
                 (Flax defaults))

         (import type-page)
         (import type-process)
         (import type-post)

         ;; create the default writer for page
         (define (create-writer)
           (lambda (sxml-tree output)
             (let ((port (open-output-file output)))
               (sxml->html sxml-tree port)
               (close-output-port port))))

         ;; write the rendered page to one directory
         ;; the directory can be a path
         ;; directory must be absolute path!!!
         (define (write-page page directory)
           (match page
                  (($ page name content writer)
                   (let ((output (string-append directory 
                                                (directory-separator-string) 
                                                name)))
                     (mkdir-r (path-parent output))
                     (writer content output)))))

         ;; build the page obj and write it to disk
         (define page
           (case-lambda
             [(post directory)
              (let* ((process-layer default-process-layer)
                     (writer (create-writer))
                     (page* (make-page (string-append (path-root (post-name post))
                                                      (directory-separator-string)
                                                      ".html")
                                       ((process-procedure (assq-ref process-layer 'meta)) process-layer post)
                                       writer)))
                (write-page page* directory))]
             [(post directory process-layer)
              (let* ((writer (create-writer))
                     (page* (make-page (string-append (path-root (post-name post))
                                                      (directory-separator-string)
                                                      ".html")
                                       ((process-procedure (assq-ref process-layer 'meta)) process-layer post)
                                       writer)))
                (write-page page* directory))]))

         )
