(library (Flax reader reader)
         (export make-reader-matcher)
         (import (scheme)
                 (Flax structure)
                 (Flax utils))

         (import type-reader)

         ;; make file extension matcher
         ;; return the function which returns #t if the file's
         ;; extension is ext
         (define make-reader-matcher
           (lambda (ext)
             (lambda (arg)
               (string=? ext (path-extension arg)))))
         )
