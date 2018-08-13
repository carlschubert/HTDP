;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define L1 (list 1 2 3 4))
(define L2 (list "1" "2" "3" "4"))
(define LA (list "a" "b" "c"))

(check-expect (make-pairs LA L1) (list
                                  (list "a" 1)
                                  (list "a" 2)
                                  (list "a" 3)
                                  (list "a" 4)
                                  (list "b" 1)
                                  (list "b" 2)
                                  (list "b" 3)
                                  (list "b" 4)
                                  (list "c" 1)
                                  (list "c" 2)
                                  (list "c" 3)
                                  (list "c" 4)))

(check-expect (make-pairs L1 LA) (list
                                  (list 1 "a")
                                  (list 1 "b")
                                  (list 1 "c")
                                  (list 2 "a")
                                  (list 2 "b")
                                  (list 2 "c")
                                  (list 3 "a")
                                  (list 3 "b")
                                  (list 3 "c")
                                  (list 4 "a")
                                  (list 4 "b")
                                  (list 4 "c")))

(define (make-pair x ly)
  (if (empty? ly)
      empty
      (cons (list x (first ly))
            (make-pair x (rest ly)))))

(define (make-pairs lx ly)
  (cond [(empty? lx) empty]
        [else
         (append (make-pair (first lx) ly)
                 (make-pairs (rest lx) ly))]))

(check-expect (make-pairs2 LA L1) (list
                                   (list "a" 1)
                                   (list "a" 2)
                                   (list "a" 3)
                                   (list "a" 4)
                                   (list "b" 1)
                                   (list "b" 2)
                                   (list "b" 3)
                                   (list "b" 4)
                                   (list "c" 1)
                                   (list "c" 2)
                                   (list "c" 3)
                                   (list "c" 4)))

(check-expect (make-pairs2 L1 LA) (list
                                   (list 1 "a")
                                   (list 1 "b")
                                   (list 1 "c")
                                   (list 2 "a")
                                   (list 2 "b")
                                   (list 2 "c")
                                   (list 3 "a")
                                   (list 3 "b")
                                   (list 3 "c")
                                   (list 4 "a")
                                   (list 4 "b")
                                   (list 4 "c")))

(define (make-pairs2 lx ly)
  (local [(define (make-pair2 s los)
            (cond [(empty? los) empty]
                  [else
                   (cons (list s (first los))
                         (make-pair2 s (rest los)))]))]
    (cond [(empty? lx) empty]
          [else
           (append (make-pair2 (first lx) ly)
                   (make-pairs2 (rest lx) ly))])))


(check-expect (make-pairs3 LA L1) (list
                                   (list "a" 1)
                                   (list "a" 2)
                                   (list "a" 3)
                                   (list "a" 4)
                                   (list "b" 1)
                                   (list "b" 2)
                                   (list "b" 3)
                                   (list "b" 4)
                                   (list "c" 1)
                                   (list "c" 2)
                                   (list "c" 3)
                                   (list "c" 4)))

(check-expect (make-pairs3 L1 LA) (list
                                   (list 1 "a")
                                   (list 1 "b")
                                   (list 1 "c")
                                   (list 2 "a")
                                   (list 2 "b")
                                   (list 2 "c")
                                   (list 3 "a")
                                   (list 3 "b")
                                   (list 3 "c")
                                   (list 4 "a")
                                   (list 4 "b")
                                   (list 4 "c")))



(define (recurse fnx fny b x loi)
  (cond [(empty? loi) b]
        [else
         (fnx (fny (first loi) x)
              (recurse fnx fny b x (rest loi)))]))

(define (make-pairs3 lx ly)
  (local [(define (make-pair s los)
            (recurse cons list empty s los))]
    (recurse append make-pair empty ly lx))) 

