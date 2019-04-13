(library
  (melt cell)
  (export make-cell)
  (import (scheme))

  (define make-cell
    (lambda (default-value . filter)
      (let ((internal-value default-value)
            (internal-cell (let ((internal (lambda (x) x)))
                             (if (and (not (null? filter))
                                      (eq? 2 (abs (procedure-arity-mask (car filter)))))
                                 (set! internal (car filter))
                                 (error 'make-cell "incorrect number of arguments of cell filter definition"))
                             (case-lambda
                               [(value)
                                (if (eq? 2 (abs (procedure-arity-mask value)))
                                    (set! internal value)
                                    (error 'make-cell "incorrect number of arguments of cell filter definition"))]
                               [() internal]))))
        (case-lambda
          [(value . upt-filter)
           (set! internal-value value)
           (if (not (null? upt-filter))
               (internal-cell (car upt-filter)))]
          [()
           ((internal-cell) internal-value)]))))



  )
