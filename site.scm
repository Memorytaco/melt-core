(library
  (melt site)
  (export create-site
          load-source)
  (import (scheme)
          (melt structure)
          (melt data)
          (melt page))

  (import type-site)

  ;; TODO: complete the create-site procedure
  (define create-site
    (lambda (layout comt attr)
      (make-site layout comt attr)))

  ;; get the layout lambda
  (define (load-source name)
    (define (read-fasl-lambda path)
      (define binary-port (open-file-input-port path))
      (define obj (eval (fasl-read binary-port)))
      (close-input-port binary-port)
      obj)
    (define fasl-name (string-append ".melt/fasl/" name))
    (define res-name (string-append ".melt/resource/" name ".ss"))
    (if (file-exists? res-name)
        (begin
          (if (file-exists? fasl-name)
              (delete-file fasl-name))
          (fasl-file res-name fasl-name)
          (read-fasl-lambda fasl-name))
        (if (file-exists? fasl-name)
            (read-fasl-lambda fasl-name)
            (error name "file not exists")))
    )
  )
