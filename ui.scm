(library (Flax ui)
         (export flax)
         (import (scheme)
                 (Flax utils)
                 ; (Flax command build)
                 )

         ;; command list
         (define commands (list "build" "serve"))

         ;; there are three numbers in the version string
         ;; -- the first is the main version number, when it updates, this means
         ;;    Flax can be packaged and can run independently. Flax now is not
         ;;    compatibal with previous main version.
         ;; -- the second number is the minor version number, when it updates,
         ;;    Flax is stable with all branch version. Flax can be patched
         ;;    and distributed.
         ;;    Flax is compatibal with previous second version.
         ;; -- the third number is the branch version number. Every bug fix or feature
         ;;    added or some procedure updated will update the number.
         (define (show-version)
           (format #t "Flax version 0.0.5 ~%"))

         (define version-history
           '["Flax 0.0.1 ---  complete the basic functions, split the process procedure out \n"
             "Flax 0.0.2 ---  refine the site procedure, refine the page procedure adding file extension support \n"
             "Flax 0.0.3 ---  refine the write-content procedure, let the logic be better understood!! \n"
             "Flax 0.0.4 ---  add markdown support and fix some bugs! \n"
             "Flax 0.0.5 ---  successfully navigate the utils and ui! \n"
             "Flax 1.0.0 ---  navigate to chezscheme! \n"])

         (define (show-history)
           (while (eq? '() version-history)
                  (display (car version-history))
                  (set! version-history (cdr version-history))))

         ;; the basic information
         (define (show-flax)
           (format #t "This is just another static site generator~%")
           (format #t "Please add \"-h\" or \"--help\" to get further help~%")
           (format #t "For more information please follow github io page~%"))

         ;; basic help information
         (define (show-flax-help)
           (format #t "Basic usage : ~%")
           (format #t "flax [ command ] [ options ] [ arguments ] ~%~%")
           (format #t "command list as follows :: ~%")
           (format #t "        build~%")
           (format #t "basic information argument:~%")
           (format #t "[ -v || --version ]  version number~%"))


         ;; The main function
         (define (flax arg . extra-args)
           (cond
             [(eq? extra-args '())
              (show-flax)]
             [(or (string=? (car extra-args) "-h")
                  (string=? (car extra-args) "--help"))
              (show-flax-help)]
             [(or (string=? (car extra-args) "-v")
                  (string=? (car extra-args) "--version"))
              (show-version)]
             [(or (string=? (car extra-args) "-vs")
                  (string=? (car extra-args) "--version-history"))
              (show-history)]
             ; (("build")
             ;  (build))
             ; (("build" (or "-h" "--help"))
             ;  (show-build-help))
             ; (("build" args ...)
             ;  (build (car args)))
             ; (("serve")
             ;  (format #t "Not ready now, Sorry!~%"))
             (else (show-flax))))
         )
