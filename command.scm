(library (melt command)
  (export %builtin-commands
		  %user-commands

		  add-command
		  show-commands)
  (import (scheme)
		  (melt data)
		  (melt lib color)
		  (melt structure))

  (import type-command)
  (import type-data)

  (define %builtin-commands (create-data))

  (define %user-commands (create-data))

  (define (add-command command . command-data )
	(update-data!  (if (null? command-data)
					   %user-commands
					   (car command-data))
				   (create-data (list (command-name command))
								(list command))))

  (define (remove-command command . command-data)
	(update-data! (if (null? command-data)
					   %user-commands
					   (car command-data))
				  (list (command-name command))))

  (define (show-commands commands)
	(define (show-command command)
	  (cdisplay (ctext "[2C" "")
				(ctext "[37;1m" (symbol->string (command-name command)))
				":\n"
				(ctext "[5C" "")
				(ctext "[38;5;166m" (command-desc command))
				"\n"))
	(do ((command-list (data-keys commands) (cdr command-list)))
		((null? command-list) #t)
	  (show-command (cdr (assq (car command-list) (data-cont commands))))))

  (add-command (make-command 'build "This is for building the page!" '() '())
			   %builtin-commands)
  (add-command (make-command 'serve "A little server!" '() '()))
  
;  (import (melt command build))
;  (add-command build $builtin-commands)


  )
