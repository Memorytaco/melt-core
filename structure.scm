(library (Flax structure)
         (export type-site
                 type-reader
                 type-page
                 type-post
                 type-process
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
                   (fields
                     ;; string or string list which is where the posts are
                     (mutable posts-directory site-posts-directory posts-directory-set!)
                     ;; the object directory where the builded page is sended to
                     (mutable build-directory site-build-directory build-directory-set!)
                     (mutable asset           site-asset asset-set!)   ;; the asset obj
                     ;; process-layer must be a list of processes
                     (mutable process-layer   site-process-layer process-layer-set!)
                     ;; transfer the post file to the post
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
                  page-name page-name-set!
                  page-content content-set!
                  page-writer writer-set!]
                 (define-record-type
                   page
                   (nongenerative flax-page)
                   (fields
                     (mutable name    page-name    page-name-set!)
                     (mutable content page-content content-set!)
                     (mutable writer  page-writer  writer-set!))))

         ;; ~metadata~ is an alist
         ;; ~sxml~ is a sxml tree
         (module type-post
                 [make-post post?
                  post-name post-name-set!
                  post-metadata metadata-set!
                  post-sxml sxml-set!]
                 (define-record-type
                   post
                   (nongenerative flax-post)
                   (fields 
                     (mutable name     post-name     post-name-set!)
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

         (module type-process
                 [make-process process?
                  process-key
                  process-procedure]
                 (define-record-type
                   process
                   (nongenerative flax-process)
                   (fields
                     ;; you'd better set the key as symbol
                     (immutable key process-key)
                     ;; the processor is a procedure which process the sxml tree
                     (immutable procedure process-procedure))))

         )
