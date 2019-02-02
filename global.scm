(library (Flax global)
  (export chain)
  (import (scheme)
		  (Flax invoke)
		  (Flax asset)
		  (Flax utils)
		  (Flax page)
		  (Flax post)
		  (Flax reader reader)
		  (Flax structure))

  (import type-chain)
  (import type-hook)
  (import type-site)
  (import type-process)


  (define chain (init-chain #t
							(lambda ()
								 (display "Building ...\n"))
							(init-empty-data)))
  
  (define (build chain)
	(display "in build command!\n")
	(let ((site (data-query 'site chain)))
	  (if (site? site)
		  (let ((posts-directory (site-posts-directory site))
				(build-directory (site-build-directory site))
				(process-layer (site-process-layer site))
				(readers (site-readers site)))
			(define (build-r path)
			  (cond
			   [(file-regular? path)
				(display "\nnow in regular file path\n")
				(display "path is")
				(display path)
				(display "\n")
				(if (reader-available? readers path)
					(page (read-post path readers)
						  build-directory
						  process-layer)
					(display (string-append "Error: the file " path
											" is not support!\n")))]
			   [(file-directory? path)
				(display (string-append "current path is: " path "\n"))
				(do ((directorys (directory-list path) (cdr directorys)))
					((null? directorys) (display "this layers posts have been builded!\n"))
				  (begin
					(display "current directory list: ")
					(display directorys)
					(display "\n")
					(display (string-append path
											(directory-separator-string)
											(car directorys))))
				  (build-r (string-append path
										  (directory-separator-string)
										  (car directorys))))]))
			(display "get the site obj!\n")
			(display (string-append "the posts directory is " posts-directory "\n"))
			(build-r posts-directory))
		  (error 'site "This is not a site object!"))))

  (define (asset-install chain)
	(display "in asset-install procedure!\n")
	(let ((asset (site-asset (data-query 'site chain))))
	  (asset-cp asset)))

  (add-hook chain (create-hook 'asset-install 'proc `(,asset-install . (,(data-query 'chain chain)))))
  
  (add-hook chain (create-hook 'build 'proc `(,build . (,(data-query 'chain chain)))))

  )
