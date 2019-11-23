
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1.
(define (sequence low high stride)
  (letrec ([sequence-helper (lambda (low high stride res)
                              (if (> low high)
                                  res
                                  (sequence-helper (+ low stride)
                                                   high
                                                   stride
                                                   (append res (cons low null)))))])
    ;; use tail recursion
    (sequence-helper low high stride null)))

;; Problem 2.
(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix))
       xs))

;; Problem 3.
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [else
         (letrec ([i (remainder n (length xs))]
                  [helper (lambda (ys i curr)
                            (if (= i curr)
                                (car ys)
                                (helper (cdr ys) i (+ 1 curr))))])
           (helper xs i 0))]))

;; Problem 4.
(define (stream-for-n-steps s n)
  (letrec
      ([loop (lambda (s n ans)
               (if (= n 0)
                   ans
                   (loop (cdr (s)) (- n 1) (append ans (cons (car (s)) null)))))])
    (loop s n null)))

;; Problem 5.
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                    (cons (- x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; Problem 6.
(define dan-then-dog
  (letrec
      ([f (lambda (x)
            (if (= (remainder x 2) 0)
                (cons "dan.jpg" (lambda () (f (+ x 1))))
                (cons "dog.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 0))))

;; Problem 7.
(define (stream-add-zero s)
  (letrec
      ([f (lambda ()
            (cons (cons 0 (car (s))) (cdr (s))))])
    (lambda ()
      (f))))

;; Problem 8.
(define (cycle-lists xs ys)
  (letrec
      ([loop
        (lambda (n)
          (cons (cons (list-nth-mod xs n)
                      (list-nth-mod ys n))
                (lambda () (loop (+ n 1)))))])
    (lambda () (loop 0))))

;; Problem 9.
(define (vector-assoc v vec)
  (letrec ([loop
            (lambda (v vec i)
              (if (= (vector-length vec) i)
                  #f
                  (if (and (pair? (vector-ref vec i))
                           (equal? (car (vector-ref vec i)) v))
                      (vector-ref vec i)
                      (loop v vec (+ i 1)))))])
    (loop v vec 0)))

;; Problem 10.
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n)]
           [res (lambda (v)
                  (let
                      ([vector-assoc-res (vector-assoc v memo)])
                    (if (false? vector-assoc-res)
                        (assoc v xs)
                        vector-assoc-res)))]
           [memoize (lambda (i v)
                      (vector-set! memo
                                   (remainder i n)
                                   v))]
           [i 0])
    (begin
      (memoize i res)
      (set! i (+ i 1))
      res)))
















