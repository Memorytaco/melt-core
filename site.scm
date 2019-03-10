(library (melt site)
  (export create-site)
  (import (scheme)
          (melt structure)
		  (melt invoke)
          (melt utils)
		  (melt data)
		  (melt asset))
  
  (import type-site)

  (define create-site
    (lambda args
	  (cond
	   [(null? args)
		(make-site (create-data '(index.html asset)
								(list (lambda (chain) (let ((ex (chain-data-query 'index chain)))
												   (if ex (ex))))
									  (lambda (asset)
										(load ".melt/asset.scm")
										(call/cc (lambda (cc)
												   (cc asset))))))
				   (create-data)
				   (create-data '(domain)
								'("localhost")))]
	   [else
		(let ((layout (car args))
			  (comt (car (cdr args)))
			  (attr (car (cdr (cdr args)))))
		  (make-site (apply create-data layout)
				   (apply create-data comt)
				   (apply create-data attr)))])))
  
  )
