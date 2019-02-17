(library (Flax structure)
  (export type-parser
          type-post
          type-renderer
          type-page

		  type-site
		  type-asset

		  type-hook
		  type-trigger
		  type-chain
		  type-data)
  
  (import (scheme))
  
  ;; it now is an uniform utility! can be stroed in
  ;; one place and use multiple times!
  (module type-parser
          [make-parser
		   parser?
		   parser-type
		   parser-proc
		   parser-refp]
          (define-record-type
              parser
            (nongenerative flax-parser)
            (fields
			 ;; the type is an unproper dot list like (html . #:proc)
			 ;; car of type is a symbol, mark the type of the parser
			 ;; the #:proc is a procedure to filter file and match parser
			 (immutable type parser-type)
			 ;; proc is the procedure which take charge with
			 ;; the source file
             (immutable proc parser-proc)
			 ;; refp==>resource procedure : update the resource file
			 ;; it need to be designed carefully
			 (immutable refp parser-refp))))

  ;; there maybe a lot of procedure between
  ;; this two components.
  
  ;; the post recieve the data from parser
  ;; and then process the data to satisfy its
  ;; need. So the data stored in a post is all
  ;; the data one post needs. No more change but
  ;; use.

  ;; the meta and attr is all an assoc list
  (module type-post
          [make-post
		   post?
		   post-meta meta-set!
		   post-attr attr-set!
		   post-cont cont-set!]
          (define-record-type
              post
            (nongenerative flax-post)
            (fields
			 ;; it contains the attribute about the
			 ;; source file!
             (mutable meta post-meta meta-set!)
             (mutable attr post-attr attr-set!)
             (mutable cont post-cont cont-set!))))

  ;; used to render the page component
  (module type-renderer
		  [make-renderer
		   renderer?
		   renderer-type
		   renderer-proc proc-set!
		   renderer-data data-set!]
		  (define-record-type
			  renderer
			(nongenerative flax-renderer)
			(fields
			 ;; the type is an unique id to distinguish the render
			 (immutable type renderer-type)
			 ;; proc==>process process function used to render the
			 ;; page
			 (mutable proc renderer-proc proc-set!)
			 ;; data is the data which maybe be needed
			 ;; it is the same as chain data!
			 (mutable data renderer-data data-set!))))
  
  ;; page is used to compose one page
  ;; and use the proc to write ti to disk
  ;; all the information relevant should
  ;; be done before page, page only store
  ;; information about the page it self.
  (module type-page
          [make-page
		   page?
		   page-comt comt-set!
		   page-proc proc-set!
		   page-cont cont-set!
		   page-attr attr-set!]
          (define-record-type
              page
            (nongenerative flax-page)
            (fields
			 ;; comt==>component : it store the components of one
			 ;; page (including some metadata), anything one page
			 ;; needed. split one page into many components and
			 ;; compose it.
			 ;; proc==>procedure : it is the function which used
			 ;; to write the page and also before write a page, it
			 ;; needs to compose the page from components first.
			 ;; cont==>content : it store the full sxml tree
			 ;; of the page.
			 ;; attr==>attribute : it is an assoc list contains some extra
			 ;; settings on page
             (mutable comt page-comt comt-set!)
             (mutable proc page-proc proc-set!)
			 (mutable cont page-cont cont-set!)
			 (mutable attr page-attr attr-set!))))

  (module type-site
          [make-site
		   site?
		   site-layout layout-set!
		   site-comt comt-set!
		   site-attr attr-set!]
          (define-record-type
              site
            (nongenerative flax-site)
            (fields
			 ;; it store a type data. the symbol is an key word which
			 ;; specifics the purpose of the 
			 (mutable layout site-layout layout-set!)
			 ;; comt==>component : it describes the composement of the
			 ;; site and the action on each component.
			 (mutable comt site-comt comt-set!)
             ;; it is the attribute of the site like domain name
			 ;; it is a data type
			 (mutable attr site-attr attr-set!))))
  
  (module type-asset
          [make-asset
		   asset?
           asset-source
           asset-target]
          (define-record-type
              asset
            (nongenerative flax-asset)
            (fields
             (immutable source asset-source)
             (immutable target asset-target))))
  
  ;; hook is the small data cell or execute cell
  (module type-hook
          [make-hook
		   hook?
		   hook-name
		   hook-type
		   hook-proc-arg proc-arg-set!
		   hook-data hook-data-set!]
          (define-record-type
              hook
            (nongenerative flax-hook)
            (fields
             ;; if the type is 'data, proc-arg contain data
             ;; else if the type is 'proc, proc-arg is defined as following
             (immutable name hook-name)
             (immutable type hook-type)
             ;; proc-arg is a dot pair
             ;; (procedure . args)
             ;; the hook function
             ;; the arguments, it must be wrapped in a list
             (mutable proc-arg hook-proc-arg proc-arg-set!)
             ;; it sotres hook data, it must be same as chain data
             (mutable data hook-data hook-data-set!))))

  ;; the trigger module for future
  (module type-trigger
		  [make-trigger
		   trigger?
		   trigger-cond
		   trigger-act]
		  (define-record-type
			  trigger
			(nongenerative flax-trigger)
			(fields
			 (immutable cond trigger-cond)
			 (immutable act  trigger-act))))

  
  ;; define the execution priority and data transform
  (module type-chain
          [make-chain
		   chain?
		   chain-condition condition-set!
		   chain-execution execution-set!
		   chain-data chain-data-set!]
          (define-record-type
              chain
            (nongenerative flax-chain)
            (fields
             ;; condition must be #t or #f
             ;; or a procedure which return #t
             ;; or #f and accept no argument
             (mutable condition chain-condition condition-set!)
             ;; it is a list of procedure without arguments
             (mutable execution chain-execution execution-set!)
             ;; it's a unproper dot list,
             ;; and car is the key list
             ;; while cdr is an assoc list
             (mutable data chain-data chain-data-set!))))

  (module type-data
		  [make-data
		   data?
		   data-keys data-keys-set!
		   data-cont data-cont-set!]
		  (define-record-type
			  data
			(nongenerative flax-data)
			(fields
			 ;; a list of symbols
			 (mutable keys data-keys data-keys-set!)
			 ;; an assoc list which contains keys in the
			 ;; keys field
			 (mutable cont data-cont data-cont-set!))))
  
  )
