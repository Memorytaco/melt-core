(library
  (melt command post)
  (export post)
  (import (scheme)
          (melt glob)
          (melt uutil)
          (melt lib console)
          (melt config)
          (melt structure))

  (import type-command)

  (define post
    (make-command 'post "note post command"
                  (lambda (command . arg)
                    (cond
                      [(equal? command "new")
                       (apply action-new arg)]
                      [(equal? command "list")
                       (apply action-list arg)]
                      [(equal? command "edit")
                       (apply action-edit arg)]
                      [(equal? command "-h")
                       (gem-display
                         (gem "[38;5;15m" "this is the post command, contain 'new', 'list', and 'edit' subcommands.\n")
                         (gem "[38;5;15m" "use -h in each subcommand to get more info.\n"))]
                      [else (display "unrecognized command, add -h to get info\n")]))))

  ;; ====================================================================
  ;; command new
  (define action-new
    (lambda title
      (let* ((str-num-sequence (map car (get-md-post-title-list "post")))
             (numbers (sort > (map string->number str-num-sequence))))
        (if (not (null? title))
            (write-template (car title)
                            (string-append (config-query 'post)
                                           "/"
                                           (number->string (+ 1 (car numbers))) ".md"))
            (display "please provide a title\n")))))


  (define (write-template title path)
    (call-with-output-file
      path
      (lambda (port)
        (display "~~~\n" port)
        (format port "title : ~a~%" title)
        (format port "date : ~a~%" (date-and-time))
        (display "~~~\n\n" port))))

  ;; ====================================================================
  ;; command list
  (define action-list
    (lambda args
      (if (not (null? args))
          (if (equal? (car args) "-h")
              (gem-display
                (gem "[38;5;15m" "A subcommand to list post sequence\n")))
          (list-post))))

  (define list-post
    (lambda ()
      (do ((title-list (sort (lambda (pre aft) (string>? (car pre) (car aft))) (get-md-post-title-list (config-query 'post)))
                       (cdr title-list)))
        ((null? title-list) (gem-display (gem "[37m" "=====\n")))
        (gem-display (gem "[38;5;15m" (car (car title-list)))
                     " ==> "
                     (gem "[38;5;15m" (cdr (car title-list)))
                     "\n"))))


  ;; ====================================================================
  ;; command edit
  (define action-edit
    (lambda number
      (let* ((str-num-sequence (map car (get-md-post-title-list "post")))
             (numbers (sort > (map string->number str-num-sequence))))
        (if (null? number)
            (call-editor (number->string (car numbers)))
            (call-editor (car number))))))


  ;; use external shell to call editor
  (define (call-editor number)
    (system (string-append (config-query 'editor)
                           " "
                           (config-query 'post)
                           "/"
                           number ".md")))


  )
