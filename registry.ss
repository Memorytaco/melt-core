#!chezscheme
(library
  (melt registry)
  (export registry!
          registry-remove!
          registry-query

          command-alter!
          command-remove!
          command-query
          show-commands

          config-alter!
          config-remove!
          config-query)
  (import (scheme)
          (melt data)
          (melt invoke)
          (melt command)
          (melt lib console))

  (import (melt command init))
  (import (melt command invoke))
  (import (melt command serve))
  (import (melt command post))

  ;; the only glob variable
  (define registry-hub (create-data))
  ;; insert a key and value to the glob hub
  ;; if the key exist, update it!
  (define (registry! key value)
    (update-data! key value registry-hub))
  ;; remove the value
  (define (registry-remove! key)
    (update-data! (list key) registry-hub))
  ;; get the value for the key
  (define (registry-query key)
    (data-value-query key registry-hub))

  ;; ==============   command   =========
  ;; command definition
  ;; add one command
  (define (command-alter! command type)
    (if (memv type '(inter self))
        (update-data! (command-name command) command (registry-query type))
        (gemd:error "in add:should use 'inter or 'self for command type")))
  ;; the key is the command name and the type is the command type
  (define (command-remove! key type)
    (if (memv type '(inter self))
        (update-data! (list key) (registry-query type))))
  (define (command-query name type)
    (if (memv type '(inter self))
        (data-value-query name (registry-query type))
        (gemd:error "in query:should use 'inter or 'self for command type")))

  ;; display command names and its description
  (define (show-commands type)
    (do ((command-list (data-values (registry-query type)) (cdr command-list)))
      ((null? command-list) (void))
      (show-command "  ~8a>>>   ~a~%" (car command-list))))


  ;; ==============   configuration   ================
  (define (config-alter! key value)
    (update-data! key value (registry-query 'config)))
  (define (config-remove! key)
    (update-data! (list key) (registry-query 'config)))
  (define (config-query key)
    (data-value-query key (registry-query 'config)))

  ;; define some useful predefined value
  (registry! 'config (create-data)) ;; for config
  (registry! 'inter (create-data)) ;; for builtin command
  (registry! 'self (create-data))  ;; for userdefined command
  (registry! 'invocation (create-invocation)) ;; global invocation

  (config-alter! 'editor "vim")
  (config-alter! 'domain "http://example.com")
  (config-alter! 'post "post")
  (config-alter! 'public "public")
  (config-alter! 'config "melt.ss")
  (config-alter! 'version "0.0.5-2")

  ;; registry command to inter
  (command-alter! init-cli 'inter)
  (command-alter! (invoke-cli (registry-query 'invocation)) 'inter)
  (command-alter! serve-cli 'inter)
  (command-alter! (post (registry-query 'config)) 'inter)

  )
