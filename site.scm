(library (melt site)
  (export site)
  (import (scheme)
          (melt structure)
          (melt utils))
  
  (import type-site)

  (define create-site
    (lambda args
	  (cond
	   [(null? args)
		(make-site (create-data '(index )
								（list )
				   (create-data '(domain)
								'("sample.com")))])))
  
  )
