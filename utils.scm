(library (Flax utils)
         (export get-absolute-path
                 mkdir-p
                 decompose-path-name
                 compose-path-name
                 basename
                 hash-ref
                 assq-ref
                 alist->hash-table
                 alist-delete
                 string-split-dual
                 alist-cons
                 string-trim
                 while)
         (import (scheme)
                 (Flax structure))

         ;; library body  ---------------
         (import type-site)


         ;; return the absolute path of the file
         (define (get-absolute-path file-name)
           (if (string? file-name)
               (if (path-absolute? file-name)
                   file-name
                   (string-append (current-directory) "/" file-name))
               (error file-name "must be string!")))

         ;; decompose a string
         ;; example
         ;; "/usr/lib/share" => ("usr" "lib" "share")
         (define (decompose-path-name path-name)
           (define (generate-list path)
             (if (eq? path "")
                 '()
                 (if (eq? "" (path-first path))
                     (cons (path-rest path)
                           '())
                     (cons (path-first path)
                           (generate-list (path-rest path))))))
           (if (string? path-name)
               (if (eq? "" path-name)
                   '()
                   (let ((name (if (path-absolute? path-name)
                                   (path-rest path-name)
                                   path-name)))
                     (generate-list name)))
               (error path-name "must be string!")))

         ;; define string join
         (define (string-join str-list seperator command)
           (define (join-seperator str seperator command)
             (cond
               [(eq? command 'prefix)
                (string-append seperator str)]
               [(eq? command 'suffix)
                (string-append str seperator)]))
           (define (verify-string str-list)
             (if (eq? str-list '())
                 #f
                 (if (string? (car str-list))
                     (if (eq? '() (cdr str-list))
                         #t
                         (verify-string (cdr str-list)))
                     #f)))

           (cond
             [(atom? str-list)
              (if (eq? '() str-list)
                  (error str-list "Empty list!")
                  (error str-list "Must be a list!"))]
             [(verify-string str-list)
              (if (or (string? seperator)
                      (char? seperator))
                  (let ((sep (if (char? seperator)
                                 (string seperator)
                                 seperator)))
                    (cond
                      [(or (eq? command 'prefix)
                           (eq? command 'suffix))
                       (let* ((number (length str-list))
                              (new-list (map join-seperator str-list (make-list number sep) (make-list number command))))
                         (apply string-append new-list))]
                      [(eq? command 'middle)
                       (let* ((number (- (length str-list) 1))
                              (new-list (map join-seperator (cdr str-list) (make-list number sep) (make-list number 'suffix))))
                         (apply string-append (cons (car str-list)
                                                    new-list)))]
                      [else (error command "isn't a proper command!")]))
                  (error seperator "isn't a string or char!"))]))

         ;; components is a list of strings
         ;; like ("hello" "nice" "good") => /hello/nice/good
         (define (compose-path-name str-list)
           (string-join str-list "/" 'prefix))

         ;; while the test is satified, end the iterate
         (define-syntax while
           (syntax-rules ()
             [(_ test forms ...)
              (do ()
                (test)
                forms ...)]))

         ;; create directory
         ;; mark !! need to improve!!!!!!!!!!!!!!!!!!!!!!!!!!
         (define (mkdir-p dir)
           ; Create the dir just like use makedir bash command but clever
           (define dir-list (decompose-path-name dir))
           (let ((file-name (if (path-absolute? dir)
                                (string-append "/" (car dir-list))
                                (car dir-list))))
             (while (eq? '() dir-list)
                    (if (file-exists? file-name)
                        (if (not (file-directory? file-name))
                            (begin
                              (format (current-error-port) "There conficts file exists!!~%")
                              (break)))
                        (mkdir file-name))
                    (set! dir-list (cdr dir-list))
                    (if (not (eq? '() dir-list))
                        (set! file-name (string-append file-name "/" (car dir-list)))))
             #t))

         (define alist->hash-table
           (lambda (alist)
             (let ((ht (make-eqv-hashtable)))
               (do ((iterate-alist alist (cdr iterate-alist))
                    (pair-list (car alist) (car iterate-alist)))
                 ((eq? iterate-alist '()) ht)
                 (hashtable-set! ht
                                 (car pair-list)
                                 (cdr pair-list))))))

         (define hash-ref
           (case-lambda
             [(hashtable key)
              (hashtable-ref hashtable key #f)]
             [(hashtable key default)
              (hashtable-ref hashtable key default)]))

         ;; return the left alist which dosn't contain the symbal
         (define alist-delete
           (lambda (symbal alist)
             (do ((arg-list alist (cdr arg-list))
                  (compare-list (assq symbal alist))
                  (rest-alist '() (if (equal? compare-list (car arg-list))
                                      rest-alist
                                      (cons (car arg-list) rest-alist))))
               ((eq? arg-list '()) rest-alist))))

         (define assq-ref
           (lambda (symbol alist)
             (cdr (assq symbol alist))))

         (define basename
           (lambda (path)
             (path-last (path-root path))))

         (define string-split-dual
           (lambda (arg-string char)
             (define first-position
               (lambda (arg-char-list char)
                 (do ((char-list arg-char-list (cdr char-list))
                      (end? #f)
                      (number 0 (+ 1 number)))
                   (end? (- number 1))
                   (if (eq? (car char-list)
                            char)
                       (set! end? #t))))) ;; zero based
             (let* ((char-list (string->list arg-string))
                    (position (first-position char-list char)))
               (list (apply string (list-head char-list position))
                     (apply string (list-tail char-list (+ 1 position)))))))

         (define string-trim
           (lambda (arg-string command)
             (cond
               [(eq? command 'prefix)
                => (do ((string-list (string->list arg-string))
                        (end? #f))
                     (end? (string string-list))
                     (if (equal? (car string-list)
                                 #\space)
                         (set! string-list (cdr string-list))
                         (set! end? #t)))]
               [(eq? command 'suffix)
                => (string (reverse (string->list (string-trim (string (reverse (string->list arg-string))) 'prefix))))]
               [(eq? command 'both)
                => (string-trim (string-trim arg-string 'prefix)
                                'suffix)]
               [else arg-string])))

         (define alist-cons
           (lambda (key obj alist)
             (cons (cons key obj)
                   alist)))
         )
