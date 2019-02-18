(library (melt asset)
         (export asset-cp
                 asset-list-cp
                 mkdir-r)
         (import (scheme)
                 (melt srfi match)
                 (melt lib file)
                 (melt structure))

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
