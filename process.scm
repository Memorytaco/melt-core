(library (Flax process)
         (export process-ref
                 procedure-ref)
         (import (scheme)
                 (Flax structure)
                 (Flax post))


         (import type-process)

         ;; return the process from the process-layer otherwise
         ;; return '()
         (define (process-ref process-layer key)
           (assq-ref process-layer key))

         ;; return the processor of one process in process-layer
         (define (procedure-ref process-layer key)
           (process-procedure (process-ref process-layer key)))


         ;; read each post, genereate an alist of post path , not completed!!
         (define (Hello)
           (format #t "This is not done!!~%"))

         )
