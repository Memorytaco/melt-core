(library
  (melt config)
  (export config-update!
          config-query)
  (import (scheme)
          (melt glob)
          (melt data))

  ;; accept an alist or single list as argument
  (define (config-update! a-list)
    (update-data! %%config a-list))

  (define (config-query key)
    (data-value-query key %%config))
  )
