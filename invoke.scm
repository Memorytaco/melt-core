#!chezscheme
(library (Flax invoke)
  (export init-chain
          execute-chain
          data-keys
          data-alist
		  data-query
		  data-update
          init-empty-data
          create-data
          chain-hooks-list-query
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
    (lambda ()
      '(() . ())))
  
  (define create-data
    (lambda (keys data)
      (cons keys
            (map cons keys data))))
  
  (define create-hook
    (lambda (name type arg)
      (cond
       [(eq? type 'data)
                (make-hook name 'data #f arg)]
       [(eq? type 'proc)
        (make-hook name 'proc arg #f)]
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

  ;; if the key not in chain keys, return #f
  ;; otherwise return the value
  (define (data-query key chain)
	(if (chain? chain)
		(if (element-exist? key (data-keys chain))
			(cdr (assq key (data-alist chain)))
			#f)
		(error chain "argument should be type chain!")))
  
  ;; keys and datas must be a list and must be match
  ;; if the keys doesn't exist, create it!
  ;; otherwise alter it!
  (define data-update
    (case-lambda
      [(chain data)
       (do ((keys (car data) (cdr keys))
            (alist (cdr data)))
           ((null? keys) #t)
         (chain-data-set! chain
						  (cons (cons (car keys)
									  (remove (car keys)
											  (data-keys chain)))
								(cons (assq (car keys)
											alist)
									  (remove (assq (car keys)
													(data-alist chain))
											  (data-alist chain))))))]
      [(chain keys values)
       (data-update chain (create-data keys values))]))

  ;; change the default make-chain
  (define init-chain
    (lambda (condition execution data)
      (if (not (list? (car data)))
          (error 'data "data should contain key list!"))
      (let ((chain (make-chain condition (list execution) data)))
        (chain-data-set! chain (cons (if (element-exist? 'chain (data-keys chain))
                                         (error 'data "You shouldn't use 'chain as key!")
                                         (cons 'chain (data-keys chain)))
                                     (cons (cons 'chain chain) (data-alist chain))))
        (chain-data-set! chain (cons (if (element-exist? 'hooks (data-keys chain))
                                         (error 'data "You shouldn't use 'hooks as key!")
                                         (cons 'hooks (data-keys chain)))
                                     (cons (cons 'hooks '((data . ())
                                                          (proc . ()))) (data-alist chain))))
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
        (error hook "argument must be hook type or hook must be 'proc type! : get-hook-proc")))
  
  (define (get-hook-args hook)
    (if (and (hook? hook)
             (eq? 'proc (hook-type hook)))
        (cdr (hook-proc-arg hook))
        (error hook "argument must be hook type or hook must be 'proc type! : get-hook-args")))
  
  (define (get-hook-keys hook)
    (if (and (hook? hook)
             (eq? 'data (hook-type hook)))
        (car (hook-data hook))
        (error hook "argument must be hook type or hook must be 'data type! : get-hook-keys")))
  
  (define (get-hook-alist hook)
    (if (and (hook? hook)
             (eq? 'data (hook-type hook)))
        (cdr (hook-data hook))
        (error hook "argument must be hook type or hook must be 'data type! : get-hook-alist")))
  
  (define (hook-execute hook)
    (if (eq? 'proc (hook-type hook))
        (apply (get-hook-proc hook)
               (if (list? (get-hook-args hook))
				   (get-hook-args hook)
				   (error 'hook-execute "the arguments must be wrapped in a list")))
        (error 'hook "hook type is not 'proc! : hook-execute")))

  (define (chain-hooks-list-query chain hook-type-key)
	(if (not (element-exist? hook-type-key '(proc data)))
		(error 'hook-type-key "key is not 'proc or 'data! : chain-hooks-list-query"))
	(let ((hooks-alist (data-query 'hooks chain)))
	  (cdr (assq hook-type-key hooks-alist))))

    ;; not export this function.
  (define (chain-hooks-update chain hook symbol)
	(define symbol-list '(before after data))
	(cond
     [(not (chain? chain))
      (error 'chain "must be a chain record!")]
     [(not (hook? hook))
      (error 'hook "must be a hook record!")]
     [(not (element-exist? symbol symbol-list))
      (error 'symbol "must be 'before, 'after or 'data(if hook type is data) symbol!")])
	(cond
	 [(eq? 'proc (hook-type hook))
	  (data-update chain
				   (create-data '(hooks)
								`(((data . ,(cdr (assq 'data
													   (data-query 'hooks
																   chain))))
								   (proc . ,(let ((proc-list (cdr (assq 'proc
																		(data-query 'hooks
																					chain)))))
											  (cond
											   [(eq? 'before symbol)
												(cons (hook-name hook)
													  proc-list)]
											   [(eq? 'after symbol)
												(append proc-list (list (hook-name hook)))]
											   [else (display "This should never appear: in invoke>chain-hooks-update !")])))))))]
	 [(eq? 'data (hook-type hook))
	  (data-update chain
				   (create-data '(hooks)
								`(((data . ,(cons (hook-name hook)
												  (cdr (assq 'data
															 (data-query 'hooks
																		 chain)))))
								   (proc . ,(cdr (assq 'proc
													   (data-query 'hooks
																   chain))))))))]
	 [else (error 'hook "hook type is not correct!")]))
  
  ;; add-hook to the chain
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
                               (chain-execution chain)))
		 (chain-hooks-update chain hook symbol)]
        [(eq? symbol 'after)
         (execution-set! chain
                         (append (chain-execution chain)
                                 (list (lambda ()
                                         (hook-execute hook)))))
		 (chain-hooks-update chain hook symbol)]
        [(eq? symbol 'data)
         (if (eq? 'data (hook-type hook))
             (begin
			   (data-update chain (hook-data hook))
			   (chain-hooks-update chain hook symbol))
			 (error 'hook "type is not data!"))]
        [else (error symbol "the symbol must be 'before 'after or 'data!")])]
      [(chain hook)
	   (cond
		[(eq? 'data (hook-type hook))
		 (add-hook chain hook 'data)]
		[(eq? 'proc (hook-type hook))
		 (add-hook chain hook 'after)]
		[else (error 'hook "incorrect hook type!")])]))
  
  (define chain-connect
    (lambda ()
      (format #t "not ready")))
  
  )
