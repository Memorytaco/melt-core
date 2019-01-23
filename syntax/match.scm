(library (Flax syntax match)
         (export match)
         (import (scheme))

         (define-syntax match
           (syntax-rules ()
             [(_ expression (pattern . body) ...)
              (match-gen-labels expression start () (pattern . body) ...)]))

         (define-syntax match-gen-labels
           (syntax-rules (=>)
             [(_ expression label ((k1 fk1 pattern1 . body1) (k fk pattern . body) ...))
              (let ((var expression))
                (letrec ((k (lambda ()
                              (match-one var pattern (begin . body) (fk))))
                         ...
                         (label (lambda () (error var "no matches"))))
                  (match-one var pattern1 (begin . body1) (fk1))))]
             [(_ expression label (labels ...) (pattern (=> fk) . body) . rest)
              (match-gen-labels expression fk (labels ... (label fk pattern . body)) . rest)]
             [(_ expression label (labels ...) (pattern . body) . rest)
              (match-gen-labels expression fail (labels ... (label fail pattern . body)) . rest)]))

         (define-syntax match-one
           (syntax-rules (* ___ quote ? and or not)
             ((match-one var () sk fk)
              (if (null? var) sk fk))
             ((match-one var (quote a) sk fk)
              (if (equal? var 'a) sk fk))
             ((match-one var (and) sk fk) sk)
             ((match-one var (and a b ...) sk fk)
              (match-one var a (match-one var (and b ...) sk fk) fk))
             ((match-one var (or) sk fk) fk)
             ((match-one var (or a ...) sk fk)
              (let ((sk2 (lambda () sk)))
               (match-one var (not (and (not a) ...)) (sk2) fk)))
             ((match-one var (not a) sk fk)
              (match-one var a fk sk))
             ((match-one var (? pred a ...) sk fk)
              (if (pred var) (match-one var (and a ...) sk fk) fk))
             ((match-one var (a ___) sk fk)
              (match-extract-variables a (match-gen-ellipses var a sk fk) ()))
             ((match-one var (a) sk fk)
              (if (and (pair? var) (null? (cdr var)))
                  (let ((tmp (car var)))
                   (match-one tmp a sk fk))
                  fk))
             ((match-one var (a . b) sk fk)
              (if (pair? var)
                  (let ((tmp1 (car var)))
                   (match-one tmp1 a (let ((tmp2 (cdr var))) (match-one tmp2 b sk fk)) fk))
                  fk))
             ((match-one var #(a ...) sk fk)
              (if (vector? var)
                  (let ((ls (vector->list var)))
                   (match-one ls (a ...) sk fk))
                  fk))
             ((match-one var * sk fk) sk)
             ((match-one var x sk fk)
              (let-syntax ((sym?
                             (syntax-rules ()
                               ((sym? x) (let ((x var)) sk))
                               ((sym? y) (if (equal? var x) sk fk)))))
                (sym? abracadabra)))
             ))

         (define-syntax match-gen-ellipses
           (syntax-rules ()
             ((_ var a sk fk ((v v-ls) ...))
              (let loop ((ls var) (v-ls '()) ...)
               (cond ((null? ls)
                      (let ((v (reverse v-ls)) ...) sk))
                     ((pair? ls)
                      (let ((x (car ls)))
                       (match-one x a (loop (cdr ls) (cons v v-ls) ...) fk)))
                     (else
                       fk))))
             ))

         (define-syntax match-extract-variables
           (syntax-rules (* ___ quote ? and or not)
             ((_ (a . b) k v)
              (match-extract-variables a (match-extract-variables-step b k v) ()))
             ((_ #(a ...) k v)
              (match-extract-variables (a ...) k v))
             ((_ a (k ...) (v ...))
              (let-syntax ((sym?
                             (syntax-rules ()
                               ((sym? a) (k ... (v ... (a a-ls))))
                               ((sym? b) (k ... (v ...))))))
                (sym? abracadabra)))
             ))

         (define-syntax match-extract-variables-step
           (syntax-rules ()
             ((_ a k (v ...) (v2 ...))
              (match-extract-variables a k (v ... v2 ...)))))


         (define-syntax match-lambda
           (syntax-rules ()
             ((_ clause ...) (lambda (expression) (match expression clause ...)))))

         (define-syntax match-lambda*
           (syntax-rules ()
             ((_ clause ...) (lambda expression (match expression clause ...)))))

         (define-syntax match-let
           (syntax-rules ()
             ((_ ((pattern expression)) . body)
              (match expression (pattern . body)))
             ((_ ((pattern expression) ...) . body)
              (match (list expression ...) ((pattern ...) . body)))
             ((_ loop . rest)
              (match-named-let loop () . rest))
             ))

         (define-syntax match-named-let
           (syntax-rules ()
             ((_ loop ((pattern expression var) ...) () . body)
              (let loop ((var expression) ...)
               (match-let ((pattern var) ...)
                          . body)))
             ((_ loop (v ...) ((pattern expression) . rest) . body)
              (match-named-let loop (v ... (pattern expression tmp)) rest . body))
             ))

         (define-syntax match-letrec
           (syntax-rules ()
             ((_ vars . body) (match-letrec-helper () vars . body))))

         (define-syntax match-letrec-helper
           (syntax-rules ()
             ((_ ((pattern expression var) ...) () . body)
              (letrec ((var expression) ...)
                (match-let ((pattern var) ...)
                           . body)))
             ((_ (v ...) ((pattern expression) . rest) . body)
              (match-letrec-helper (v ... (pattern expression tmp)) rest . body))
             ))

         (define-syntax match-let*
           (syntax-rules ()
             ((_ () . body)
              (begin . body))
             ((_ ((pattern expression) . rest) . body)
              (match expression (pattern (match-let* rest . body))))))
         )
