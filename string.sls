
(library (indexable-sequence string)
  
  (export string-fold-left
          string-fold-right
          string-for-each
          string-for-each-with-index
          string-copy
          string-map!
          string-map
          string-subseq
          string-take
          string-drop
          string-filter-to-reverse-list
          string-filter
          string-index
          string-find
          string-swap!
          string-reverse!
          string-reverse)
  
  (import (except (rnrs)
                  string-for-each
                  string-copy)
          (rnrs mutable-strings)
          (indexable-sequence define-indexable-sequence-operations))

  (define-indexable-sequence-operations
    string
    string-length
    string-ref
    string-set!
    make-string))

