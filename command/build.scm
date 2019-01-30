(library (Flax command build)
         (export show-build-help
                 build)
         (import (scheme)
                 (Flax structure)
                 (Flax utils))

         (import type-site)
         ;; display the build command usage
         (define (show-build-help)
           (format #t "Useage: flax build <ConfigFileName> \n")
           (format #t "If the <ConfigFileName> is not provided, use \"config.scm\" instead. \n"))

         ;; the build command which is invoked by the ui
         (define (build config-file)
           (if (file-exists? config-file)
               (let ((obj (load config-file)))
                 (if (site? obj)
                     (build-site obj)
                     (format (current-error-port) "Didn't receive site object !~%Last config expression must be site ~%")))
               (begin
                 (format #t "Coundn't find config file !! ~%expect ~a but got nothing !~%" (basename config-file)))))

         )
