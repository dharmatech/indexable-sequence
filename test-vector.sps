
(import (rnrs)
        (indexable-sequence vector))

(assert
 (equal?
  (vector-fold-left (lambda (ls a)
                      (cons (list a) ls))
                    '()
                    '#(1 2 3))
  '((3) (2) (1))))

(assert
 (equal?
  (vector-fold-left (lambda (ls a b)
                      (cons (list a b) ls))
                    '()
                    '#(1 2 3)
                    '#(4 5 6))
  '((3 6) (2 5) (1 4))))

(assert
 (equal?
  (vector-fold-left (lambda (ls a b c)
                      (cons (list a b c) ls))
                    '()
                    '#(1 2 3)
                    '#(4 5 6)
                    '#(7 8 9))
  '((3 6 9) (2 5 8) (1 4 7))))


(assert
 (equal?
  (vector-fold-right cons
                     '()
                     '#(10 20 30))
  '(10 20 30)))

(assert
 (equal?
  (vector-fold-right (lambda (a b ls)
                       (cons (list a b) ls))
                     '()
                     '#(1 2 3)
                     '#(4 5 6))
  '((1 4) (2 5) (3 6))))

(assert
 (equal?
  (vector-fold-right (lambda (a b c ls)
                       (cons (list a b c) ls))
                     '()
                     '#(1 2 3)
                     '#(4 5 6)
                     '#(7 8 9))
  '((1 4 7) (2 5 8) (3 6 9))))

(assert
 (null?
  (let ((elts '(10 20 30)))
    (vector-for-each (lambda (elt)
                       (set! elts (remove elt elts)))
                     '#(10 20 30))
    elts)))

(assert
 (null?
  (let ((elts '((1 4) (2 5) (3 6))))
    (vector-for-each (lambda (a b)
                       (set! elts (remove (list a b) elts)))
                     '#(1 2 3)
                     '#(4 5 6))
    elts)))

(assert
 (null?
  (let ((elts '((0 a) (1 b) (2 c))))
    (vector-for-each-with-index (lambda (i a)
                                  (set! elts (remove (list i a) elts)))
                                '#(a b c))
    elts)))

(assert
 (null?
  (let ((elts '((0 a d) (1 b e) (2 c f))))
    (vector-for-each-with-index (lambda (i a b)
                                  (set! elts (remove (list i a b) elts)))
                                '#(a b c)
                                '#(d e f))
    elts)))

(assert
 (null?
  (let ((elts '((0 a d g) (1 b e h) (2 c f i))))
    (vector-for-each-with-index (lambda (i a b c)
                                  (set! elts (remove (list i a b c) elts)))
                                '#(a b c)
                                '#(d e f)
                                '#(g h i))
    elts)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((a '#(10 20 30)))
  (let ((b (vector-copy a)))
    (assert (equal? a b))
    (vector-set! a 0 100)
    (assert (not (equal? a b)))
    (list a b)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert
 (equal?
  (let ((v (vector 1 4 9)))
    (vector-map! sqrt v)
    v)
  '#(1 2 3)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert
 (equal?
  (vector-map sqrt '#(4 9 16))
  '#(2 3 4)))

(assert
 (equal?
  (vector-map list
              '#(a b c)
              '#(d e f))
  '#((a d) (b e) (c f))))

(assert
 (equal?
  (vector-map list
              '#(a b c)
              '#(d e f)
              '#(g h i))
  '#((a d g) (b e h) (c f i))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert
 (equal?
  (vector-subseq '#(a b c d e f g h i) 3 6)
  '#(d e f)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert
 (equal?
  (vector-filter-to-reverse-list odd? '#(1 2 3 4 5 6))
  '(5 3 1)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert
 (equal?
  (vector-filter odd? '#(1 2 3 4 5 6))
  '#(1 3 5)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vector-index (lambda (elt)
                (eq? elt 'c))
              '#(a b c d e f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vector-find (lambda (elt)
               (eq? (car elt) 'c))
             '#((a . 10) (b . 20) (c . 30)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert
 (equal?
  (let ((a (vector 10 20 30)))
    (vector-swap! a 0 2)
    a)
  '#(30 20 10)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert
 (equal?
  (let ((v (vector 'a 'b 'c 'd 'e 'f)))
    (vector-reverse! v)
    v)
  '#(f e d c b a)))

;; (assert
;;  (equal?
;;   (let ((v '#(a b c d e f)))
;;     (reverse! v)
;;     v)
;;   '#(f e d c b a)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert
 (equal?
  (vector-reverse '#(a b c d e f))
  '#(f e d c b a)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

