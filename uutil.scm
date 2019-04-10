(library
  (melt uutil)
  (export parse-posts)
  (import (scheme)
          (melt post)
          (melt lib file)
          (melt data)
          (melt parser))

  (define (flatten x)
    (cond ((null? x) '())
          ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
          (else (list x))))

  (define-syntax scone!
    (syntax-rules ()
      [(_ ls item)
       (set! ls (append ls (if (not (null? item)) (list item) '())))]))

  ;; parse a directory and return a list of posts
  (define (parse-posts path parser-list)
    (let ((dir-ls (directory-list path))
          (post-ls '()))
      (do ((obj-ls (map string-append
                        (make-list (length dir-ls) (string-append path (directory-separator-string)))
                        dir-ls)
                   (cdr obj-ls)))
        ((null? obj-ls) (flatten post-ls))
        (if (file-directory? (car obj-ls))
            (scone! post-ls (parse-posts (car obj-ls) parser-list))
            (let ((raw-sxml (parse-with-parsers (car obj-ls) parser-list)))
              (if raw-sxml
                  (scone! post-ls (compose-post
                                    `((path . ,(car obj-ls))
                                      (name . ,(path-last (path-root (car obj-ls)))))
                                    (car raw-sxml)
                                    (car (cdr raw-sxml))))))))))

  )
