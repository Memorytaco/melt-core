(library (Flax page)
  (export page
		  create-writer)
  (import (scheme)
          (Flax structure)
          (Flax srfi match)
          (Flax utils)
		  (Flax process)
          (Flax asset)
          (Flax reader sxml)
          (Flax product))
  
  (import type-page)
  (import type-process)
  (import type-post)
  
  ;; create the default writer for page
  (define (create-writer)
    (lambda (sxml-tree output)
      (let ((port (open-output-file output 'replace)))
        (sxml->html sxml-tree port)
        (close-output-port port))))
  
  ;; write the rendered page to one directory
  ;; the directory can be a path
  ;; directory must be absolute path!!!
  (define (write-page page directory)
    (let* ((name (page-name page))
		  (content (page-content page))
		  (writer (page-writer page))
		  (output (string-append directory 
                                 (directory-separator-string) 
                                 name)))
	  (mkdir-r (path-parent output))
	  (writer content output)))
  
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
