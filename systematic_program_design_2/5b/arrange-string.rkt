;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arrange-string) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data definitions:


;; ListOfString is one of: 
;;  - empty
;;  - (cons String ListOfImage)
;; interp. a list of strings
(define LS0 empty) 
(define LS1 (cons ("hello" empty))
(define LS2 (cons ("hello" (cons "world" empty)))

#;
(define (fn-for-los los) 
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Template rules used: 
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Image ListOfImage)
;; - atomic non-distinct: (first loi) is String
;; - self-reference: (rest loi) is ListOfString