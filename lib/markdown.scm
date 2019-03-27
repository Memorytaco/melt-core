#!chezscheme
(library
  (melt lib markdown)
  (export markdown->sxml
          scone
          check-chars
          position-forward
          eof)
  (import (scheme))

  ;; define append but contain one single element
  (define-syntax scone
    (syntax-rules ()
      ((_ ls new) (append ls (list new)))))

  ;; define the eof object for pattern
  (define eof (eof-object))

  ;; if n larger than position, do nothing
  (define (position-back port n)
    (let ((position (port-position port)))
      (set-port-position! port (if (and (> position 0) (> position n) (> n 0))
                                   (- position n)
                                   position))))

  ;; forward n position
  (define (position-forward port n)
    (set-port-position! port (+ n (port-position port))))

  ;; #######################################################
  ;; the pattern is a char list. port is a text port.

  ;; pattern can include a nested char list.
  ;; normal pattern is (#\space #\a ...)
  ;; if the pattern is (#\space #a), it match " a", return the pattern means true.

  ;; and a nested pattern is (#\space (#\a #\b #\c) #\\)
  ;; this pattern match " a\" and " b\" and " c\".
  ;; the nested char match a single char.
  ;; if the nested list is '(), it will match any char, any!
  (define (check-chars pattern port)
    (define ($$check pattern position port)
      (if (null? pattern)
          (begin (set-port-position! port position) pattern)
          (let ((pattern-char (car pattern)))
            (cond
              [(null? pattern-char)
               (read-char port) ;; accept it, if the pattern is '()
               ($$check (cdr pattern) position port)]
              [(list? pattern-char)
               (cond
                 [(eq? (car pattern-char) '!)
                  (if (not (member (read-char port) pattern-char))
                      ($$check (cdr pattern) position port)
                      (begin (set-port-position! port position) #f))]
                 [else
                   (if (member (read-char port) pattern-char)
                       ($$check (cdr pattern) position port)
                       (begin (set-port-position! port position) #f))])]
              [(char? pattern-char)
               (if (eq? (read-char port) pattern-char)
                   ($$check (cdr pattern) position port)
                   (begin (set-port-position! port position) #f))]
              [(eof-object? pattern-char)
               (if (eof-object? (read-char port))
                   ($$check (cdr pattern) position port)
                   (begin (set-port-position! port position) #f))]
              [else
                (error 'pattern-chars "unproperly pattern char!")]))))
    ($$check pattern (port-position port) port))

  ;; when match, return numbers chars in a list
  ;; when not match, return #f
  (define (terminate-pattern-match pattern port number)
    (define (return-chars char-list port number)
      (cond
        [(= number 0)
         char-list]
        [(> number 0)
         (return-chars (scone char-list (read-char port))
                       port
                       (- number 1))]
        [else (error 'return-chars "error occured in terminate-pattern-match")]))

    (let ((n (if (> number (length pattern)) (length pattern) number)))
      (if (check-chars pattern port)
          (return-chars (list) port n)
          #f)))

  (define (check-list ls ob)
    (if (eq? (length ls) (length ob))
        (cond
          [(null? ls)
           #t]
          [(equal? (car ls) (car ob))
           (check-list (cdr ls) (cdr ob))]
          [else #f])
        #f))

  (define (g-check-list ls)
    (lambda (ob)
      (check-list ls ob)))

  ;; use the pattern to determine which context to enter in
  ;; return the parsed sxml and pass previous context(procedure) to it
  ;; so it can return to previous context
  ;; 这里之后可以使用cc来进行错误处理和异常报告排错, 需要和define-FA 联合
  (define-syntax context-transform
    (syntax-rules ()
      [(_ sxml port key-patterns FAs cur-context)
       (do ((patterns key-patterns (cdr patterns))
            (key-context-list (map cons key-patterns FAs)))
         ((null? patterns) #f)
         (display "in context-transform \n")
         (let ((cur-pattern (car patterns)))
           (if (check-chars cur-pattern port)
               ((lambda (item)
                  (cur-context (scone sxml item) port))
                ((cdr (assp (g-check-list cur-pattern) key-context-list)) (list) port)))))]))

  ;; check each terminate pattern and end the context if matched returning value.
  ;; forward-numbers list is to forward the port position if matched.
  ;; if not match, return #f.
  (define-syntax context-return
    (syntax-rules ()
      [(_  value port key-pattern forward-numbers)
       (call/cc
         (lambda (cc)
           (do ((terminate-pattern (cons `(,eof) key-pattern) (cdr terminate-pattern))
                (query-forward-numbers (map cons (cons `(,eof) key-pattern)
                                            (cons 0 forward-numbers))))
             ((null? terminate-pattern) #f)
             (let* ((cur-pattern (car terminate-pattern))
                    (number (cdr (assp (g-check-list cur-pattern) query-forward-numbers))))
               (if (terminate-pattern-match cur-pattern port number)
                   (cc value))))))]
      [(_ value port) ;; just for single eof
       (context-return value port '() '())]))

  ;; define a FA
  ;; 之后可以在else后边加入错误处理
  (define-syntax define-FA
    (syntax-rules ()
      [(_ name end-lambda (end-patterns number-list) (key-patterns FAs) )
       (define name
         (lambda (sxml port)
           (cond
             [(context-return (end-lambda sxml) port end-patterns number-list)]
             [(context-transform sxml port key-patterns FAs name)]
             [else (error name "no proper transform context definition")])))]))

  ;; (g 'x 3) ==> (x x x)
  (define (g ele num)
    (cond
      [(= num 0)
       '()]
      [(> num 0)
       (cons ele (g ele (- num 1)))]))

  ;;; above is the common utility
  ;;; #################################################################
  ;;; #################################################################
  ;;; #################################################################
  ;;; below is the definition

  ;; define makrdown-> sxml
  (define (markdown->sxml port)
    (pattern-top-parse (list) port))

  ;;; ================== escape ================
  ;; return the escaped char
  (define (aux-escape sxml port)
    (cadr (terminate-pattern-match '(#\\ ()) port 2)))

  ;; aux helpler
  (define (aux-block-begin number)
    (lambda (sxml port)
      (position-forward port number) '()))
  (define (aux-block-end sxml port)
    (peek-char port))

  ;; read one char
  (define (aux-common sxml port)
    (read-char port))
  (define (aux-ignore sxml port)
    (position-forward port 1) '())

  (define (pattern-parse-list sxml port)
    (display "not ready"))

  ;; the top environment(context)
  (define-FA pattern-top-parse
             (lambda (sxml) sxml)
             ((list `(,eof)) '(0))
             ('((#\\ ())
                (#\newline)
                (#\` #\` #\`)
                (#\#)
                (#\* #\space)
                (#\- #\space)
                (#\! #\[)
                (#\> #\space)
                (()))
              (list aux-escape
                    aux-ignore
                    pattern-parse-block-code
                    pattern-parse-header
                    pattern-parse-list
                    pattern-parse-list
                    pattern-parse-img
                    pattern-parse-block-quote
                    pattern-parse-paragraph)))

  ;;; ==================== paragraph ===================
  ;; return the paragraph
  (define-FA pattern-parse-paragraph
             (lambda (sxml) `(p ,sxml))
             ('((#\newline #\newline)
                (#\newline #\` #\` #\`)
                (#\newline #\#)
                (#\newline #\* #\space)
                (#\newline #\- #\space)
                (#\newline #\! #\[)
                (#\newline #\> #\space))
              (2 1 1 1 1 1 1))
             ('((#\\ ())
                (#\newline)
                (#\[)
                (#\`)
                (#\*)
                (#\-)
                (#\* #\*)
                (#\- #\-))
              (list aux-escape
                    aux-paragraph-space
                    pattern-parse-link
                    pattern-parse-inline-code
                    pattern-parse-em
                    pattern-parse-em
                    pattern-parse-strong
                    pattern-parse-strong)))

  (define (aux-paragraph-space sxml port)
    (context-return #\space port '(#\newline) 1))

  ;; =====================  block-quote =======================
  ;; return block quote
  (define-FA pattern-parse-block-quote
             (lambda (sxml) `(blockquote (p ,sxml)))
             ('((#\newline #\newline)
                (#\newline #\` #\` #\`)
                (#\newline #\#)
                (#\newline #\* #\space)
                (#\newline #\- #\space)
                (#\newline #\! #\[)
                (#\newline #\> #\space))
              (2 1 1 1 1 1 1))
             ('((#\\)
                (#\newline)
                (#\`)
                (#\[)
                (#\*)
                (#\-)
                (#\* #\*)
                (#\- #\-))
              (list aux-escape
                    aux-paragraph-space
                    pattern-parse-inline-code
                    pattern-parse-link
                    pattern-parse-em
                    pattern-parse-em
                    pattern-parse-strong
                    pattern-parse-strong)))

  ;;;   =============== header ===============
  ;; return the header
  (define-FA pattern-parse-header
             (lambda (sxml) sxml)
             ((list '(#\newline)) (list 1))
             ((list '(#\#))
              (list aux-parse-header)))

  (define (aux-parse-header sxml port)
    (display "in parse-header\n")
    (display sxml)
    (display "\n")
    (cond
      [(check-chars '(#\newline) port)
       sxml]
      [(check-chars '(#\# #\space) port)
       (position-forward port 2)
       (list 'h1 (aux-parse-header sxml port))]
      [(check-chars '(#\# #\# #\space) port)
       (position-forward port 3)
       (list 'h2 (aux-parse-header sxml port))]
      [(check-chars '(#\# #\# #\# #\space) port)
       (position-forward port 4)
       (list 'h3 (aux-parse-header sxml port))]
      [(check-chars '(#\# #\# #\# #\# #\space) port)
       (position-forward port 5)
       (list 'h4 (aux-parse-header sxml port))]
      [(check-chars '(#\# #\# #\# #\# #\# #\space) port)
       (position-forward port 6)
       (list 'h5 (aux-parse-header sxml port))]
      [(check-chars '(#\# #\# #\# #\# #\# #\# #\space) port)
       (position-forward port 7)
       (list 'h6 (aux-parse-header sxml port))]
      [else (aux-parse-header (scone sxml (read-char port)) port)]))

  ;; =================  block code ==========================
  ;; return the block code
  (define-FA pattern-parse-block-code
             (lambda (sxml) `(pre (code (car sxml) (car (cdr sxml)))))
             ((list '(#\` #\` #\` #\newline)) (list 4))
             ((list '(#\` #\` #\`)
                    '(()))
              (list aux-parse-block-code-type
                    aux-parse-block-code-cont)))

  (define aux-parse-block-code-type
    (case-lambda
      [(sxml port)
       (cond
         [(context-return `(@ (class ,(apply string sxml)))
                          port '((#\newline)) '(1))]
         [(check-chars '(#\` #\` #\` #\space) port)
          (position-forward port 4)
          (aux-parse-block-code-type (scone sxml (read-char port)) port)]
         [else (aux-parse-block-code-type (scone sxml (read-char port)) port)])]))

  (define aux-parse-block-code-cont
    (case-lambda
      [(sxml port)
       (cond
         [(check-chars '(#\` #\` #\` #\newline) port)
          sxml]
         [else (aux-parse-block-code-cont
                 (scone sxml (read-char port)) port)])]))


  ;; =================== inline code ===========================
  ;; return the code type
  (define-FA pattern-parse-inline-code
             (lambda (sxml) `(code ,sxml))
             ((list '(() #\`)) (list 2))
             ('((#\\ ())
                (#\`)
                (() #\`)
                (()))
              (list aux-escape
                    (aux-block-begin 1)
                    aux-block-end
                    aux-common)))

  ;; ==================== strong =============================
  (define-FA pattern-parse-strong
             (lambda (sxml) (cons 'strong sxml))
             ((list '(() #\* #\*)
                    '(() #\_ #\_))
              (list 3 3))
             ((list '(#\* #\*)
                    '(#\_ #\_)
                    '(() #\_ #\_)
                    '(() #\* #\*)
                    '(()))
              (list (aux-block-begin 2)
                    (aux-block-begin 2)
                    aux-block-end
                    aux-block-end
                    aux-common)))

  ;; ===================== em    ================================
  ;; return the emphsis
  (define-FA pattern-parse-em
             (lambda (sxml) (cons 'em sxml))
             ((list '(() #\*)
                    '(() #\_))
              (list 2 2))
             ((list '(#\*)
                    '(#\_)
                    '(() #\*)
                    '(() #\_)
                    '(()))
              (list (aux-block-begin 1)
                    (aux-block-begin 1)
                    aux-block-end
                    aux-block-end
                    aux-common)))

  ;; ====================== hr ==================================
  ;; return (hr)
  (define-FA pattern-parse-hr
             (lambda (sxml) '(hr))
             ((list '(#\newline)) '(1))
             ((list '(()))
              (list aux-parse-hr)))

  (define (aux-parse-hr sxml port)
    (read-char port) '())

  ;; ====================== link ==================================
  (define-FA pattern-parse-link
             (lambda (sxml) `(a ,(car (cdr sxml)) ,(car sxml)))
             ((list '(#\))) '(1))
             ((list '(#\()
                    '(#\[))
              (list aux-parse-link-end
                    aux-parse-link-begin)))

  (define (aux-parse-link-begin sxml port)
    (cond
      [(check-chars '(#\[) port)
       (position-forward port 1)
       (aux-parse-link-begin sxml port)]
      [(check-chars '(#\]) port)
       (position-forward port 1)
       sxml]
      [else (aux-parse-link-begin (scone sxml (read-char port)) port)]))

  (define (aux-parse-link-end sxml port)
    (cond
      [(check-chars '(#\() port)
       (position-forward port 1)
       (aux-parse-link-end sxml port)]
      [(check-chars '(#\)) port)
       `(@ (href ,(apply string sxml)))]
      [else (aux-parse-link-end (scone sxml (read-char port)) port)]))

  ;; ======================= img ================================
  ;; return an img
  (define-FA pattern-parse-img
             (lambda (sxml) `(img ,(car (cdr sxml)) ,(car sxml)))
             ((list '(#\))) '(1))
             ((list '(#\()
                    '(#\! #\[))
              (list aux-parse-img-end
                    aux-parse-img-begin)))

  (define (aux-parse-img-begin sxml port)
    (cond
      [(check-chars '(#\! #\[) port)
       (position-forward port 2)
       (aux-parse-img-begin sxml port)]
      [(check-chars '(#\]) port)
       (position-forward port 1)
       sxml]
      [else (aux-parse-img-begin (scone sxml (read-char port)) port)]))

  (define (aux-parse-img-end sxml port)
    (cond
      [(check-chars '(#\() port)
       (position-forward port 1)
       (aux-parse-img-end sxml port)]
      [(check-chars '(#\)) port)
       `(@ (src ,(apply string sxml)))]
      [else (aux-parse-img-end (scone sxml (read-char port)) port)]))

  )
