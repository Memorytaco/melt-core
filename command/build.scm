(library (Flax command build)
  (export show-build-help
          build)
  (import (scheme)
          (Flax structure)
		  (Flax invoke)
		  (Flax global)
          (Flax utils))
  
  (import type-site)
  ;; display the build command usage
  (define (show-build-help)
    (display "Useage: flax build <user-file> \n")
    (display "If the <user-file> is not provided, use \"env.scm\" instead. \n"))
  
  ;; the build command which is invoked by the ui
  (define (build user-file)
    (if (file-exists? user-file)
        (begin
		  (load user-file)
		  (execute-chain chain))
		(begin
          (format #t "Coundn't find config file !! ~%expect ~a but got nothing !~%" (basename user-file)))))
  
  )
