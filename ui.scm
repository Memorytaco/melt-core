#!chezscheme
(library
  (melt ui)
  (export melt)
  (import (scheme)
          (melt lib console)
          (melt registry)
          (melt command)
          (melt cell)
          (melt srfi match))

  (define (show-version)
    (gemd:info (string-append
                 (gem:text "[37;1m" "melt")
                 (gem:text "[38;5;15m" " version ")
                 (gem:text "[38;5;165m" (config-query 'version))) ))

  ;; the basic information
  (define (introduction)
    (gem:display (gem:text "[37m" "This is melt! Meta Excellent Local Note System.\n")
                 (gem:text "[37m" "Please use \"-h\" or \"--help\" to get further help.\nFor more information please follow")
                 (gem:text "[36m" " github io page.\n"))
    (gemd:info "github blog https://haxpeta.github.io/Lago"))

  ;; basic help information
  (define (help)
    (gem:display (gem:text "[37;1m" "melt ")
                 (gem:text "[38;5;102m" "{[options] or [subcommand]} [args] \n"))
    (gem:display (gem:text "[38;5;80m" "options are :")
                 (gem:text "[38;5;111m" " -h | -v | -l\n")))

  ;; to load the source file
  ;; mostly to load config file first
  (define melt-load
    (make-cell ".melt/config.scm"
               (lambda (value)
                 (if (file-regular? value)
                     (begin (load value) #t)
                     #f))))

  ;; to structure commands
  (define list-command
    (make-cell
      #t
      (lambda (value)
        (if (melt-load)
            (begin
              (gemd:info "Available commands :")
              (show-commands 'inter)
              (show-commands 'self))
            (begin
              (gemd:info "Available commands :")
              (show-commands 'inter))))))

  ;; user interface
  (define (melt self . extra-args)
    (match extra-args
           [(or ("-h") ("--help"))
            (help)
            (exit 0)]
           [(or ("-v") ("--version"))
            (show-version)
            (exit 0)]
           [(or ("-l") ("--list"))
            (list-command)
            (exit 0)]
           ['()
            (introduction)
            (exit 0)]
           [else (gemd:info "searching command ...")])

    ;; if config file exists, just load it
    (melt-load)
    (let ((inter-command (command-query (string->symbol (car extra-args)) 'inter))
          (self-command (command-query (string->symbol (car extra-args)) 'self)))
      (cond
        [inter-command
          (command-execute inter-command (cdr extra-args))]
        [self-command
          (command-execute self-command (cdr extra-args))]
        [else
          (gemd:error (gem:text "[38;5;99m" "Command not available!"))
          (list-command)])))

  )
