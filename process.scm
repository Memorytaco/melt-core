(library (Flax process)
  (export process-ref
          procedure-ref)
  (import (scheme)
          (Flax structure)
          (Flax utils)
          (Flax post))
  
  
  (import type-process)
  
  ;; return the process from the process-layer otherwise
  ;; return #f
  (define (process-ref key process-layer)
    (let ((process (assq-ref key process-layer)))
      (if process
          process
          (error 'process-ref "process not avaible"))))
  
  ;; return the processor of one process in process-layer
  (define (procedure-ref key process-layer)
	(let ((process (process-ref key process-layer)))
      (if process
          (process-procedure process)
          (error 'procedure-ref "procedure not avalible"))))
  )
