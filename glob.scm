#!chezscheme
(library
  (melt glob)
  (export %%chain
          %%config)
  (import (scheme)
          (melt structure)
          (melt invoke)
          (melt data)
          (melt asset))

  (import type-chain)
  (import type-hook)
  (import type-site)

  (define %%chain (init-chain
                    #t
                    (lambda () (display "Building ...\n"))
                    (create-data)))

  (define %%config (create-data))
  )
