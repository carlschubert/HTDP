
;; tuition-graph-starter.rkt  (just the problem statements)
#lang htdp/bsl

(require 2htdp/image)
;
; PROBLEM:
;
; Eva is trying to decide where to go to university. One important factor for her is
; tuition costs. Eva is a visual thinker, and has taken Systematic Program Design,
; so she decides to design a program that will help her visualize the costs at
; different schools. She decides to start simply, knowing she can revise her design
; later.
;
; The information she has so far is the names of some schools as well as their
; international student tuition costs. She would like to be able to represent that
; information in bar charts like this one:
;
;
;         .
;
; (A) Design data definitions to represent the information Eva has.
; (B) Design a function that consumes information about schools and their
;     tuition and produces a bar chart.
; (C) Design a function that consumes information about schools and produces
;     the school with the lowest international student tuition.
;

;;;; First step define constants

(define FONT-SIZE 24)
(define FONT-COLOR "black")

(define Y-SCALE 1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "lightblue")

;;;; Next define data definitions
;; in this problem our variable type data is schools we have

;; Data Definitions

;;;; create a struct with the properties we are interested in
(define-struct school (name tuition))
;; school is (make-school String Natural)
;; interp. schools have a string name and a natural tuition in USD

(define S1 (make-school "Brown" 29846))
(define S2 (make-school "Cal. Tech" 24117))
(define S3 (make-school "Rutgers" 7592))
(define S4 (make-school "Cooper Union" 0))
(define S5 (make-school "NYU" 28496))

;;;; Decide what templates are needed to create a stub function
;;;; schools has two properties so

;;Template rules used:
;; - compound (make-school String Natural)

(define (fn-for-school s)
    (... (school-name s)
         (school-tuition s)))

;;;; Now we have a single school well defined but we will want them as a list so

;; ListOfSchool is one of:
;; - empty
;;;; this is a list that consists of a copound data schools and another instance of itself
;; - (cons school ListOfSchool)
;; interp. a list of schools

(define LOS1 empty)
(define LOS2 (cons S1 (cons S2 (cons S3 (cons S4 (cons S5 empty))))))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons school ListOfSchools)
;; - reference: (first los)
;; - self-reference: (rest los) is ListOfSchool

(define (fn-for-los los)
    (cond [(empty? los) (...)]
          [else
           (... (fn-for-school (first los))
                (fn-for-los (rest los)))]))



;;Functions

;; ListOfSchool -> Image
;; produce a bar chart shoing names and tuition of ListOfSchool
(define (chart los) (square 0 "solid" "white"))

(check-expect (chart empty) (square 0 "solid" "white"))
(check-expect (chart (cons (make-school "S1" 25000) empty))
              (beside/align "bottom" (overlay/align "center" "bottom"
                                     (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                     (rectangle BAR-WIDTH (* 25000 Y-SCALE) "outline" "black")
                                     (rectangle BAR-WIDTH (* 25000 Y-SCALE) "solid" BAR-COLOR))
                      (square 0 "solid" "white")))
(check-expect (chart (cons (make-school "S1" 25000) (cons (make-school "S2" 8000) empty)))
              (beside/align "bottom" (overlay/align "center" "bottom"
                                     (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                     (rectangle BAR-WIDTH (* 25000 Y-SCALE) "outline" "black")
                                     (rectangle BAR-WIDTH (* 25000 Y-SCALE) "solid" BAR-COLOR))
                      (overlay/align "center" "bottom"
                                     (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                     (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                     (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                      (square 0 "solid" "white")))
