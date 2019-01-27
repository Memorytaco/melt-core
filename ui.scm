(library (Flax ui)
         (export flax)
         (import (scheme)
                 (Flax utils)
                 (Flax srfi match)
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
           (format #t "Flax version 0.0.8 ~%"))

         (define version-history
           '["Flax 0.0.1 ---  complete the basic functions, split the process procedure out. \n"
             "Flax 0.0.2 ---  refine the site procedure, refine the page procedure adding file extension support. \n"
             "Flax 0.0.3 ---  refine the write-content procedure, let the logic be better understood!! \n"
             "Flax 0.0.4 ---  add markdown support and fix some bugs! \n"
             "Flax 0.0.5 ---  successfully navigate the utils and ui! \n"
             "Flax 0.0.6 ---  add match support!! add srfi !! Will do it all the time! \n"
             "Flax 0.0.7 ---  get defaults.scm done! get sxml->html done! Now left post.scm site.scm page.scm space.scm command builder. \n"
             "Flax 0.0.8 ---  complete post.scm and page.scm comming up with some extra utilities in utils.scm. \n"
             "Flax 0.0.9 ---  get the asset.scm done! fix string-trim procedure and add some usefull utilities. \n"
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

         (define (flax arg . extra-args)
           (match extra-args
                  [("-h" "--help")
                   (show-flax-help)]
                  [(or ("-v") ("--version")) 
                   (show-version)]
                  [(or ("-vs") ("--version-history"))
                   (show-history)]
                  [("build")
                  ;  (build)
                   (display "not ready yet!\n")]
                  [("build" (or "-h" "--help"))
                  ;  (show-build-help)
                   (display "build help\n")]
                  [("build" args ...)
                  ;  (build (car args))
                   (display "build with many args\n")
                   (map display (map string-append args (make-list (length args) "\n")))]
                  [("serve")
                   (display "not ready yet!\n")]
                  (else (show-flax))))

         )
