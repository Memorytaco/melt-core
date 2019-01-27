(library (Flax process)
         (export process-ref
                 procedure-ref)
         (import (scheme)
                 (Flax structure)
                 (Flax post))


         (import type-process)

         ;; return the process from the process-layer otherwise
         ;; return #f
         (define (process-ref key process-layer)
           (let ((process (assq-ref process-layer key)))
             (if process
                 process
                 #f)))

         ;; return the processor of one process in process-layer
         (define (procedure-ref key process-layer)
           (let ((process (process-ref key process-layer)))
             (if process
                 (process-procedure process)
                 #f)))


         ;; read each post, genereate an alist of post path , not completed!!
         (define (Hello)
           (format #t "This is not done!!~%"))

         )
