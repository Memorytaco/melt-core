(library (Flax post)
         (export read-post
                 post-ref)
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
                   (make-post (basename file-name) meta-data content))
                 (begin
                   (format (current-error-port) "Unmatched file : ~a ~%" file-name)
                   '()))))

         (define (post-ref key post)
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

           (let loop ((metadata '()))
             (let ((line (get-line port)))
               (cond
                 [(eof-object? line)
                  => (error (port-name port)
                            "end of file while readig metadata: " )]
                 [(string=? line "---")
                  => metadata]
                 (else
                   (match (map string-trim-both (string-split-dual line #\:))
                          (((= string->symbol key) value)
                           (loop (alist-cons key (parse-metadata key value) metadata)))
                          (_ (error line "invalid metadata format."))))))))

         ;; just like echo, return what it accept!!
         (define identity
           (lambda (obj)
             obj))

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

; (define-module (Flax post)
;   #:use-module (Flax utils)
;   #:use-module (Flax reader)

;       read-post
;       post-ref
;       register-metadata-parser!
;       read-metadata-headers
;       parse-metadata))


