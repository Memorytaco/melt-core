(library (Flax structure)
         (export type-site
                 type-reader
                 type-page
                 type-post
                 type-asset
                 lago-system)
         (import (scheme))


         ;; help system will
         (module lago-system
                 [help-type]

                 ;; help-type will tell the basic information for the type
                 (define (help-type arg)
                   (cond
                     [(string? arg) (cond
                                      ([equal? arg "site"]
                                        (display "this is the help doc for site!\n"))
                                      (else (type-list)))]
                     [else (display "Sorry! No doc provided!\n")]))

                 (define (type-list)
                   (display "Types are listed :\n")
                   (display "|| site || reader || page || post || asset ||\n")))


         ;; ~title~ is a string
         ;; ~domain~ is a string
         ;; ~posts-directory~ where posts found
         ;; ~build-directory~ generated pages stored in
         ;; ~readers~ a list of reader objects
         ;; ~builders~ a list of procedures for building pages
         (module type-site
                 [make-site site?
                  site-asset asset-set!
                  site-process-layer process-layer-set!
                  site-posts-directory posts-directory-set!
                  site-build-directory build-directory-set!
                  site-readers readers-set!
                  site-builders builders-set!]
                 (define-record-type
                   site
                   (nongenerative flax-site)
                   (fields (mutable asset           site-asset asset-set!)
                           (mutable process-layer   site-process-layer process-layer-set!)
                           (mutable posts-directory site-posts-directory posts-directory-set!)
                           (mutable build-directory site-build-directory build-directory-set!)
                           (mutable readers         site-readers readers-set!)
                           (mutable builders        site-builders builders-set!))))

         ;; ~matcher~ is a function to judge whether the file is supported
         ;; by this reader
         ;; ~proc~ is the processor function which process the supported file
         (module type-reader
                 [make-reader reader?
                  reader-matcher matcher-set!
                  reader-proc proc-set!]
                 (define-record-type
                   reader
                   (nongenerative flax-reader)
                   (fields
                     (mutable matcher reader-matcher matcher-set!)
                     (mutable proc    reader-proc    proc-set!))))


         ;; define the record <page>
         ;; ~file-name~ is a string
         ;; ~contents~ is the page content
         ;; ~writer~ is the procedure to write the
         ;;          content to disk and the procedure may do
         ;;          some extra job like converting the content
         ;;          to some formats
         (module type-page
                 [make-page page?
                  page-name name-set!
                  page-content content-set!
                  page-writer writer-set!]
                 (define-record-type
                   page
                   (nongenerative flax-page)
                   (fields
                     (mutable name    page-name    name-set!)
                     (mutable content page-content content-set!)
                     (mutable writer  page-writer  writer-set!))))

         ;; ~metadata~ is an alist
         ;; ~sxml~ is a sxml tree
         (module type-post
                 [make-post post?
                  post-name name-set!
                  post-metadata metadata-set!
                  post-sxml sxml-set!]
                 (define-record-type
                   post
                   (nongenerative flax-post)
                   (fields (mutable name     post-name     name-set!)
                           (mutable metadata post-metadata metadata-set!)
                           (mutable sxml     post-sxml     sxml-set!))))

         (module type-asset
                 [make-asset asset?
                  asset-target
                  asset-source]
                 (define-record-type
                   asset
                   (nongenerative flax-asset)
                   (fields
                     (immutable target asset-target)
                     (immutable source asset-source))))
         )
