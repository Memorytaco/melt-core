(library (Flax site)
  (export site)
  (import (scheme)
          (Flax structure)
          (Flax asset)
          (Flax utils)
          (Flax product)
          (Flax srfi srfi))
  
  (import type-site)
  (import type-asset)
  (import reader-module)
  (import process-layer-module)

  ;; create the site object and return it
  ;; The defaults are :
  ;; posts-directory -> posts
  ;; build-directory -> blog
  ;; asset           -> src: assets trg: blog
  ;; process-layer   -> default-process-layer
  ;; readers         -> default-reader-list
  ;; the argument must be an assoc-list
  ;; like '((posts-directory . "post") (build-directory . "blog") ....)         
  (define site
    (define-assoc-lambda make-site
      '[post-directory build-directory asset
                       process-layer readers]
      `["posts" "blog"
        ,(make-asset "assets" "blog")
        ,process-layer
        ,(list sxml-reader commonmark-reader)]))
  
  )
