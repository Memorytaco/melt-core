(library (flax page)
         (export )
         (import (scheme)
                 (Flax structure))

         (import type-page)

         ;; write the rendered page to one directory
         ;; the directory can be a path
         (define (write-page page prefix-directory)
           (match page
                  (($ <page> file-name contents writer)
                   (let ((output (string-append prefix-directory "/" file-name)))
                     (mkdir-p (dirname output))
                     (writer contents output)))))

         ;; create the default writer for page
         (define (create-writer)
           (lambda (contents output)
             (let ((port (open-output-file output)))
               (sxml->html contents port)
               (close-output-port port))))

         ;; build the page obj and write it to disk
         (define page
           (case-lambda
             [(post prefix-directory)
              (let ((process-layer default-processlayer)
                    (writer (create-writer)))
                (let ((file-name (regexp-substitute #f (string-match ".[a-zA-Z]+$" (get-post-file-name post)) ;; use regexp to change the ext to "html"
                                                    'pre ".html")))
                  (let ((page* (make-page file-name
                                          ((get-processor (assq-ref process-layer 'meta))
                                           #:process-layer process-layer
                                           ;; the process-object is a post-object
                                           #:process-object post)
                                          writer)))
                    (write-page page* prefix-directory))))]
             [(post prefix-directory process-layer)
              (let ((writer (create-writer)))
                (let ((file-name (regexp-substitute #f (string-match ".[a-zA-Z]+$" (get-post-file-name post)) ;; use regexp to change the ext to "html"
                                                    'pre ".html")))
                  (let ((page* (make-page file-name
                                          ((get-processor (assq-ref process-layer 'meta))
                                           #:process-layer process-layer
                                           ;; the process-object is a post-object
                                           #:process-object post)
                                          writer)))
                    (write-page page* prefix-directory))))]))

         ; (define-module (Flax page)
         ;   #:use-module (Flax post)
         ;   #:use-module (Flax html)
         ;   #:use-module (Flax process)

         ;   #:use-module (ice-9 regex)
         ;   #:use-module (ice-9 match)
         ;   #:use-module (srfi srfi-9)
         ;   #:use-module (srfi srfi-26)
         ;   #:use-module (Flax utils)

         ;   #:export (make-page
         ;             is-page?
         ;             get-page-file-name
         ;             get-page-contents
         ;             get-page-writer
         ;             write-page
         ;       create-writer
         ;       page))

