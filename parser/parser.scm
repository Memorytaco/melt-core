(library (melt parser parser)
  (export make-filter
		  make-type
		  create-parser
          parser-query)
  (import (scheme)
          (melt structure)
          (melt utils))
  
  (import type-parser)
  
  ;; make file extension matcher
  ;; return the function which returns #t if the file's
  ;; extension is ext
  (define make-filter
    (lambda (ext)
      (lambda (path)
        (string=? ext (path-extension path)))))

  ;; make a parser type dot list
  (define make-type
	(lambda (arg-type)
	  (define type ((lambda (type)
					  (cond
					   [(symbol? type)
						type]
					   [(string? type)
						(string->symbol type)]
					   [else (error 'type "in *make-type* : type must be symbol or string")]))
					arg-type))
	  (cons type
			(make-filter (symbol->string type)))))

  ;; please use this function instead of make-parser
  (define create-parser
	(case-lambda
	  [(type proc)
	   (create-parser type proc '())]
	  [(type proc refp)
	   (let ((proc (if (procedure? proc)
					   proc
					   (error proc "in *create-parser* : proc is not a procedure")))
			 (refp (if (procedure? refp)
					   refp
					   (error refp "in *create-parser* : proc is not a procedure"))))
		 (make-parser (make-type type) proc resr refp))]))
  
  ;; query one parser in a parser list
  ;; if exists return it or return #f
  (define parser-query
    (lambda (arg-type parsers)
	  (let ((type ((lambda (type)
					 (if (symbol? type)
						 type
						 (string->symbol type)))
				   arg-type)))
		(cond
		 [(null? parsers)
		  #f]
		 [(atom? parsers)
		  (error 'parsers "in *parser-query* : parsers must be a list of parser, but got an atom")]
		 [(eq? type (car (parser-type (car parsers))))
		  (car parsers)]
		 [else (parser-query type (cdr parsers))]))))
  
  )
