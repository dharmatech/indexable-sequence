;; Copyright (c) 2011 Eduardo Cavazos.
;; See LICENSE for terms and conditions of use.

(library (indexable-sequence define-indexable-sequence-operations)
  
  (export define-indexable-sequence-operations)
  
  (import (rnrs)
          (rename (rnrs)
                  (fold-left list-fold-left))
          (for (indexable-sequence gen-id) (meta 1)))
  
  (define (list-for-each-with-index proc ls)
    (list-fold-left (lambda (i elt)
                      (proc i elt)
                      (+ i 1))
                    0
                    ls))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (define (gen-id template-id . args)
  ;;   (datum->syntax template-id
  ;;     (string->symbol
  ;;      (apply string-append
  ;;             (map (lambda (x)
  ;;                    (if (string? x)
  ;;                        x
  ;;                        (symbol->string (syntax->datum x))))
  ;;                  args)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax define-indexable-sequence-operations

    (lambda (stx)

      (syntax-case stx ()

        ((_ type size ref put! new-of-size)

         (with-syntax ((fold-left              (gen-id #'type #'type "-fold-left"))
                       (fold-right             (gen-id #'type #'type "-fold-right"))
                       (for-each               (gen-id #'type #'type "-for-each"))
                       (for-each-with-index    (gen-id #'type #'type "-for-each-with-index"))
                       (copy                   (gen-id #'type #'type "-copy"))
                       (map-to-reverse-list    (gen-id #'type #'type "-map-to-reverse-list"))
                       (from-reverse-list      (gen-id #'type #'type "-from-reverse-list"))
                       (map!                   (gen-id #'type #'type "-map!"))
                       (map                    (gen-id #'type #'type "-map"))
                       (subseq                 (gen-id #'type #'type "-subseq"))
                       (take                   (gen-id #'type #'type "-take"))
                       (drop                   (gen-id #'type #'type "-drop"))
                       (filter-to-reverse-list (gen-id #'type #'type "-filter-to-reverse-list"))
                       (filter                 (gen-id #'type #'type "-filter"))
                       (index                  (gen-id #'type #'type "-index"))
                       (find                   (gen-id #'type #'type "-find"))
                       (swap!                  (gen-id #'type #'type "-swap!"))
                       (reverse!               (gen-id #'type #'type "-reverse!"))
                       (reverse                (gen-id #'type #'type "-reverse")))

           #'(begin

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define-syntax fold-left
                 (lambda (stx)
                   (syntax-case stx ()
                     ((fold-left ?proc ?init ?seq ?rest (... ...))
                      (with-syntax (((rest (... ...)) (generate-temporaries #'(?rest (... ...)))))
                        #'(let ((proc ?proc) (init ?init) (seq ?seq) (rest ?rest) (... ...))
                            (let ((n (size seq)))
                              (let loop ((i 0) (val init))
                                (if (>= i n)
                                    val
                                    (loop (+ i 1)
                                          (proc val
                                                (ref seq i)
                                                (ref rest i)
                                                (... ...)))))) ))))))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define-syntax fold-right
                 (lambda (stx)
                   (syntax-case stx ()
                     ((fold-right ?proc ?init ?seq ?rest (... ...))

                      (with-syntax (((rest (... ...)) (generate-temporaries #'(?rest (... ...)))))

                        #'(let ((proc ?proc)
                                (init ?init)
                                (seq  ?seq)
                                (rest ?rest)
                                (... ...))

                            (let ((n (size seq)))
                              (let loop ((i (- n 1)) (val init))
                                (if (< i 0)
                                    val
                                    (loop (- i 1)
                                          (proc (ref seq i)
                                                (ref rest i)
                                                (... ...)
                                                val)))))))))))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define-syntax for-each
                 (lambda (stx)
                   (syntax-case stx ()
                     ((for-each ?proc ?seq ?rest (... ...))
                      (with-syntax (((param (... ...)) (generate-temporaries #'(?seq ?rest (... ...))))
                                    ((rest (... ...)) (generate-temporaries #'(?rest (... ...)))))
                        #'(let ((proc ?proc) (seq ?seq) (rest ?rest) (... ...))
                            (fold-left (lambda (val param (... ...))
                                         (proc param (... ...)))
                                       #f
                                       seq
                                       rest
                                       (... ...))))))))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define-syntax for-each-with-index
                 (lambda (stx)
                   (syntax-case stx ()
                     ((for-each-with-index ?proc ?seq ?rest (... ...))
                      (with-syntax (((param (... ...)) (generate-temporaries #'(?seq ?rest (... ...))))
                                    ((rest (... ...)) (generate-temporaries #'(?rest (... ...)))))
                        #'(let ((proc ?proc) (seq ?seq) (rest ?rest) (... ...))
                            (fold-left (lambda (i param (... ...))
                                         (proc i param (... ...))
                                         (+ i 1))
                                       0
                                       seq
                                       rest
                                       (... ...))))))))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (copy seq)
                 (let ((new (new-of-size (size seq))))
                   (for-each-with-index (lambda (i elt)
                                          (put! new i elt))
                                        seq)
                   new))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (map! proc seq)
                 (for-each-with-index (lambda (i elt)
                                        (put! seq i (proc elt)))
                                      seq)
                 seq)

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (from-reverse-list ls n)
                 (let ((seq (new-of-size n)))
                   (list-for-each-with-index (lambda (i elt)
                                               (put! seq (- (- n 1) i) elt))
                                             ls)
                   seq))

               (define-syntax map-to-reverse-list
                 (lambda (stx)
                   (syntax-case stx ()
                     ((map-to-reverse-list ?proc ?seq ?rest (... ...))
                      (with-syntax (((elt (... ...)) (generate-temporaries #'(?seq ?rest (... ...))))
                                    ((rest (... ...)) (generate-temporaries #'(?rest (... ...)))))
                        #'(let ((proc ?proc)
                                (seq  ?seq)
                                (rest ?rest)
                                (... ...))
                            (fold-left (lambda (ls elt (... ...))
                                         (cons (proc elt (... ...)) ls))
                                       '()
                                       seq
                                       rest
                                       (... ...))))))))

               (define-syntax map
                 (lambda (stx)
                   (syntax-case stx ()
                     ((map ?proc ?seq ?rest (... ...))
                      (with-syntax (((rest (... ...)) (generate-temporaries #'(?rest (... ...)))))
                        #'(let ((proc ?proc)
                                (seq  ?seq)
                                (rest ?rest)
                                (... ...))
                            (from-reverse-list (map-to-reverse-list proc seq rest (... ...))
                                               (size seq))))))))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (subseq seq start end)
                 (let ((new (new-of-size (- end start))))
                   (for-each-with-index (lambda (i elt)
                                          (put! new i (ref seq (+ start i))))
                                        new)
                   new))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (take seq n)
                 (subseq seq 0 n))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (drop seq n)
                 (subseq seq n (size seq)))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (filter-to-reverse-list proc seq)
                 (fold-left (lambda (ls elt)
                              (if (proc elt)
                                  (cons elt ls)
                                  ls))
                            '()
                            seq))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (filter proc seq)
                 (let ((n (size seq)))
                   (let ((new (new-of-size n)))
                     (let loop ((i 0) (j 0))
                       (if (>= i n)
                           (subseq new 0 j)
                           (let ((elt (ref seq i)))
                             (cond ((proc elt)
                                    (put! new j elt)
                                    (loop (+ i 1) (+ j 1)))
                                   (else
                                    (loop (+ i 1) j)))))))))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (index proc seq)
                 (let ((n (size seq)))
                   (let loop ((i 0))
                     (if (>= i n)
                         #f
                         (let ((elt (ref seq i)))
                           (if (proc elt)
                               i
                               (loop (+ i 1))))))))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (find proc seq)
                 (let ((i (index proc seq)))
                   (if i (ref seq i) #f)))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (swap! seq i j)
                 (let ((a (ref seq i))
                       (b (ref seq j)))
                   (put! seq i b)
                   (put! seq j a)
                   seq))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (reverse! seq)
                 (let ((n (size seq)))
                   (let loop ((i 0) (j (- n 1)))
                     (if (>= i j)
                         seq
                         (begin (swap! seq i j)
                                (loop (+ i 1) (- j 1)))))))

               ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               (define (reverse seq)
                 (reverse! (copy seq)))

               )))))))



