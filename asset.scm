(library (Flax asset)
         (export asset-cp
                 asset-list-cp
                 cp-f
                 cp-rf
                 mkdir-r)
         (import (scheme)
                 (Flax srfi match)
                 (Flax utils)
                 (Flax structure))

         (import type-asset)

         ;; for single object
         ;; copy the src file to the target-directory
         (define (asset-cp asset-obj)
           (let ((src-directory (asset-source asset-obj))
                 (target-directory (asset-target asset-obj)))
             (cp-rf src-directory
                    (string-append target-directory
                                   (directory-separator-string)
                                   src-directory))))

         (define (asset-list-cp asset-list)
           (map asset-cp asset-list))

         ;; accept strings as arguments
         ;; if the target exsites, replace it.
         (define (cp-f src-file target-file)
           (if (not (file-exists? src-file)) (error src-file "File not exists!"))
           (mkdir-r (path-parent target-file))
           (copy-file src-file target-file (file-options no-fail)))

         ;; copy recursively and force
         (define (cp-rf src-file target-file)
           (if (file-exists? src-file)
               (cond
                 [(file-regular? src-file)
                  (cp-f src-file target-file)]
                 [(file-directory? src-file)
                  (mkdir-r target-file)
                  (do ((file-list (directory-list src-file) (cdr file-list))
                       (element (car (directory-list src-file)) (car file-list)))
                    ((null? file-list)
                     (cp-rf (string-append src-file (directory-separator-string) element)
                            (string-append target-file (directory-separator-string) element))
                     #t)
                    (cp-rf (string-append src-file (directory-separator-string) element)
                           (string-append target-file (directory-separator-string) element)))])
               (error src-file "File not exists")))


         ;; create directory recursively
         ;; if one directory exists, just enter it and create rest directory
         ;; it will never report an error!!
         (define (mkdir-r dir)
           ; Create the dir just like use makedir bash command but clever
           (define dir-list (decompose-path-name dir))
           (let ((file-name (if (path-absolute? dir)
                                (string-append "/" (car dir-list))
                                (car dir-list))))
             (while (eq? '() dir-list)
                    (if (file-exists? file-name)
                        (if (not (file-directory? file-name))
                            (begin
                              (format (current-error-port) "There exists conficts file!!~%")
                              (break)))
                        (mkdir file-name))
                    (set! dir-list (cdr dir-list))
                    (if (not (eq? '() dir-list))
                        (set! file-name (string-append file-name "/" (car dir-list)))))
             #t))


         )
