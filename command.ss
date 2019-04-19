(library
  (melt command)
  (export create-command
          command-proc
          command-desc
          command-name
          command-execute
          show-command)
  (import (scheme)
          (melt data)
          (melt lib console))

  ;; command itself is a data
  ;; symbol, which specifics string in command line
  ;; string, one line description
  ;; procedure for the command, accept arguments
  (define (create-command name desc proc)
    (create-data '(name desc proc)
                 `(,name ,desc ,proc)))

  ;; useful utility
  (define (command-name command)
    (data-value-query 'name command))
  (define (command-desc command)
    (data-value-query 'desc command))
  (define (command-proc command)
    (data-value-query 'proc command))
  (define (command-execute command args)
    (apply (command-proc command) args))

  ;; fstring the template, should contain two ~a
  (define (show-command fstring command)
    (format #t fstring (symbol->string (command-name command)) 
            (command-desc command)))

  )
