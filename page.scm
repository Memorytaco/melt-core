(library (melt page)
  (export page
		  create-writer)
  (import (scheme)
          (melt srfi match)
          (melt parser sxml)
          (melt utils)
          (melt asset)
		  (melt renderer)
          (melt structure)
          (melt product))
  
  (import type-page)
  (import type-post)
  
  ;; convert the sxml to html and write it to disk
  (define (create-writer)
    (lambda (sxml output)
      (let ((port (open-output-file output 'replace)))
        (sxml->html sxml-tree port)
        (close-output-port port))))
  
  ;; it requires a 'src-path attr in page
  ;; write the src page into the directory
  (define (write-page page directory)
    (let ((obj-path (cdr (assq 'src-path (page-attr page))))
		  (content (page-content page)))
	  (mkdir-r (string-append directory "/" (path-parent obj-path)))
	  ((cdr (page-proc page)) content
	   (string-append directory "/" obj-path))))

  (define create-proc
	(lambda (compose-procedure output-directory)
	  ))

  (define create-page
	(case-lambda
	  [(components proc cont attr)
	   ]
	  [()
	   ]))
  
  ;; build the page obj and write it to disk
  (define page
    (lambda (post directory process-layer)
      (let* ((writer (create-writer))
             (page* (make-page (string-append (path-root (post-name post))
                                              ".html")
                               ((procedure-ref 'meta process-layer)
								process-layer post)
                               writer)))
        (write-page page* directory)))))
