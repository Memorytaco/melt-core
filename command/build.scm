(library (melt command build)
  (export build)
  (import (scheme)
          (melt structure)
		  (melt invoke)
		  (melt glob)
		  (melt lib console)
          (melt utils))
  
  (import type-command)
  ;; display the build command usage
  (define (build-help)
    (gem-display (gem "[37;1m" "melt")
			  (gem "[38;5;67m" " build")
			  (gem "[38;5;253m" " <user-file> \n"))
    (gem-display "If the"
			  (gem "[38;5;253m" " <user-file> ")
			  "is not provided, use"
			  (gem "[38;5;190m" " melt.scm ")
			  "instead. \n"))
  
  ;; the build command
  (define (build-cli user-file)
    (if (file-exists? user-file)
        (begin
		  (load user-file)
		  (execute-chain %%chain))
		(begin
          (format #t "Coundn't find config file !! ~%expect ~a but got nothing !~%" (basename user-file)))))

  (define build
	(make-command 'build
				  "build command to build the site"
				  build-cli))
  
  )
