(library (Flax invoke)
         (export init-chain
                 execute-chain
                 data-keys
                 data-alist
                 chain-condition-value
                 create-hook
                 hook-execute
                 get-hook-args
                 get-hook-proc
                 get-hook-keys
                 get-hook-alist
                 add-hook)
         (import (scheme)
                 (Flax utils)
                 (Flax structure))

         (import type-hook)
         (import type-chain)

         ;; asset procedure
         (define init-empty-data
           '(() . ()))

         ; (define make-data
         ;   (case-lambda
         ;     [(alist)
         ;      (define data '())
         ;      (let ((keys )))]))

         (define create-hook
           (lambda (type arg)
             (cond
               [(eq? type 'data)
                (make-hook 'data #f arg)]
               [(eq? type 'proc)
                (make-hook 'proc arg #f)]
               [else (error type "type must be 'data or 'proc!")])))

         ;; accept a chain record return the data key list
         (define (data-keys chain)
           (if (chain? chain)
               (car (chain-data chain))
               (error chain "argument should be type chain!")))

         ;; accept a chain record return the data alist
         (define (data-alist chain)
           (if (chain? chain)
               (cdr (chain-data chain))
               (error chain "argument should be type chain!")))

         ;; change the default make-chain
         (define init-chain
           (lambda (condition execution data)
             (if (not (null? (cdr data)))
                 (error data "data should contain key list!"))
             (let ((chain (make-chain condition (list execution) data)))
               (chain-data-set! chain (cons (if (element-exist? 'chain (data-keys chain))
                                                (error data "You shouldn't use 'chain as key!")
                                                (cons 'chain (data-keys chain)))
                                            (cons (cons 'chain chain) (data-alist chain))))
               chain)))

         ;; execute chain
         (define (execute-chain chain)
           (if (chain-condition-value chain)
               (do ((execute-list (chain-execution chain) (cdr execute-list)))
                 ((null? execute-list) #t)
                 ((car execute-list)))))

         (define (chain-condition-value chain)
           (let ((condition (chain-condition chain)))
             (cond
               [(procedure? condition)
                (condition)]
               [else condition])))

         (define (get-hook-proc hook)
           (if (and (hook? hook)
                    (eq? 'proc (hook-type hook)))
               (car (hook-proc-arg hook))
               (error hook "argument must be hook type or hook must be 'proc type!")))

         (define (get-hook-args hook)
           (if (and (hook? hook)
                    (eq? 'proc (hook-type hook)))
               (cdr (hook-proc-arg hook))
               (error hook "argument must be hook type or hook must be 'proc type!")))

         (define (get-hook-keys hook)
           (if (and (hook? hook)
                    (eq? 'data (hook-type hook)))
               (car (hook-data hook))
               (error hook "argument must be hook type or hook must be 'data type!")))

         (define (get-hook-alist hook)
           (if (and (hook? hook)
                    (eq? 'data (hook-type hook)))
               (cdr (hook-data hook))
               (error hook "argument must be hook type or hook must be 'data type!")))

         (define (hook-execute hook)
           (if (eq? 'proc (hook-type hook))
               (apply (get-hook-proc hook)
                      (get-hook-args hook))
               (error 'hook "hook type is not 'proc! : hook-execute")))

         (define add-hook
           (case-lambda
             [(chain hook symbol)
              (define symbol-list '(before after data))
              (cond
                [(not (chain? chain))
                 (error 'chain "must be a chain record!")]
                [(not (hook? hook))
                 (error 'hook "must be a hook record!")]
                [(not (element-exist? symbol symbol-list))
                 (error 'symbol "must be 'before, 'after or 'data(if hook type is data) symbol!")])
              (cond
                [(eq? symbol 'before)
                 (execution-set! chain
                                 (cons (lambda ()
                                         (hook-execute hook))
                                       (chain-execution chain)))]
                [(eq? symbol 'after)
                 (execution-set! chain
                                 (append (chain-execution chain)
                                         (list (lambda () 
                                                 (hook-execute hook)))))]
                [(eq? symbol 'data)
                 (if (eq? 'data (hook-type hook))
                     (let ((keys (get-hook-keys hook))
                           (alist (get-hook-alist hook)))
                       ;;; need to check if the keys is existing
                       (chain-data-set! chain (cons (append keys (data-keys chain)) ;;here
                                                    (append alist (data-alist chain)))))
                     (error 'hook "type is not data!"))]
                [else (error symbol "the symbol must be 'before 'after or 'data!")])]
             [(chain hook)
              (add-hook chain hook 'after)]))

         (define chain-connect
           (lambda ()
             (format #t "not ready")))

         )
