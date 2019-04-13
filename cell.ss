(library
  (melt cell)
  (export make-cell)
  (import (scheme))

  (define make-cell
    (case-lambda
      [(default-value)
       (let ((internal-value default-value))
         (case-lambda
           [(value)
            (set! internal-value value)]
           [()
            internal-value]))]
      ;; need to add filter
      [(default-value filter)
       (let ((internal-value default-value))
         (case-lambda
           [(value)
            (set! internal-value value)]
           [()
            internal-value]))]))



  )
