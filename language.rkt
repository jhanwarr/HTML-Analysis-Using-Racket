#lang racket
(require csc151/rex)
(require csc151)

(provide (all-defined-out))

;Part one: Word counts

; (to-lowercase lst) -> list?
; lst : list
(define to-lowercase
  (lambda (l)
    (map string-downcase l)))

; (help-extract-words str) -> list?
; str : String

(define help-extract-words
  (let
      ([rex-word
        (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                (rex-char-range #\A #\Z)
                                (rex-string "'")))])
    (lambda (str)
      (rex-find-matches rex-word str))))

; (extract-words filename) -> list?
; filename : String
(define extract-words
  (lambda (filename)
    o(help-extract-words (file->string filename))))

; (count-one-word word filename) -> integer?
; word : String
; filename : String
(define count-one-word
  (lambda (word filename)
    (length(filter (lambda (w) (equal? w (string-downcase word)))(to-lowercase (extract-words filename))))))

; (count-words list-of-words filename) -> list?
; list-of-words : List
; filename : String
(define count-words
  (lambda (list-of-words filename)
    (map (lambda (word)
           (list word (count-one-word word filename))) list-of-words)))

;;;===========================================================================================================================================================


;Part two: Readability


; (average-sentence-length total-words num-sentences) -> number?
; total-words : Integer
; num-sentences : Integer
(define average-sentence-length
  (lambda (total-words num-sentences)
    (/ (exact->inexact total-words) (exact->inexact num-sentences))))

; (percent-difficult-word num-difficult-word) -> number?
; num-difficult-word
(define percent-difficult-word
  (lambda (num-difficult-words total-words)
     (* (/ (exact->inexact num-difficult-words) (exact->inexact total-words)) 100)))

; (compute-formula num-difficult-words total-words num-sentences) -> number?
; num-difficult-words : Integer
; total-words : Integer
; num-sentences : Integer
(define compute-formula
  (lambda (num-difficult-words total-words num-sentences)
    (+  (* 0.1579 (percent-difficult-word num-difficult-words total-words))
        (* 0.0496 (average-sentence-length total-words num-sentences)))))

; (compute-dale-chall-score num-difficult-words total-words num-sentences) -> number?
; num-difficult-words : Integer
; total-words : Integer
; num-sentences : Integer
(define compute-dale-chall-score
  (lambda (num-difficult-words total-words num-sentences)
    (if (> (compute-formula num-difficult-words total-words num-sentences) 0.05)
        (+ (compute-formula num-difficult-words total-words num-sentences) 3.6365)
        (compute-formula num-difficult-words total-words num-sentences))))

; (score->grade grade) -> string?
; grade : Number
y(define score->grade
  (lambda (grade)
    (cond
      [(< grade 5.0) "4th grade or lower"]
      [(and (>= grade 5.0) (< grade 6.0)) "5th-6th grade"]
      [(and (>= grade 6.0) (< grade 7.0)) "7th-8th grade"]
      [(and (>= grade 7.0) (< grade 8.0)) "9th-10th grade"]
      [(and (>= grade 8.0) (< grade 9.0)) "11th-12th grade"]
      [(>= grade 9.0) "13th-15th grade"])))


; (splitter) -> list?
(define splitter
  (rex-concat (rex-char-set "?.!")))

; (number-of-difficult-words list-words) -> integer?
; list-words : List
(define number-of-difficult-words
  (lambda (list-words)
    (length(filter (lambda (ele) (eq? (list-ref ele 1) 0)) (count-words list-words "DaleChallEasyWordList.txt")))))

; (count-sentence str) -> integer?
; str : String
(define count-sentence
  (lambda (str)
    (length(filter (lambda (line) (not (equal? line ""))) (rex-split-string splitter str)))))

;(dale-chall-score str) -> number?
; str : String
(define dale-chall-score
  (lambda (str)
    (let*
        ([word-list (help-extract-words str)]
         [num-dif-word  (number-of-difficult-words word-list)]
         [tot-word (length word-list)]
         [count-sen (count-sentence str)])
      (compute-dale-chall-score num-dif-word tot-word count-sen))))

;;;===========================================================================================================================================================


;Part the third: Sentiment analysis

; (number-of-positive-word list-words) -> integer?
; list-words : List
(define number-of-positive-word
  (lambda (list-words)
    (length(filter (lambda (ele) (eq? (list-ref ele 1) 0)) (count-words list-words "positive-words.txt")))))

; (percent-positive str) -> number?
; str : String
(define percent-positive
  (lambda (str)
    (* 100 (/ (exact->inexact (number-of-positive-word (help-extract-words str))) (exact->inexact (length(help-extract-words str)))))))

; (number-of-negative-word list-words) -> integer?
; list-words : List
(define number-of-negative-word
  (lambda (list-words)
    (length(filter (lambda (ele) (eq? (list-ref ele 1) 0)) (count-words list-words "negative-words.txt")))))

; (percent-negative str) -> number?
; str : String
(define percent-negative
  (lambda (str)
    (* 100 (/ (exact->inexact (number-of-negative-word (help-extract-words str))) (exact->inexact (length(help-extract-words str)))))))

; (posneg str) -> string?
; str : String
(define posneg
  (lambda (str)
    (cond
      [(>= (- (percent-positive str) (percent-negative str)) 0.05) "positive"]
      [(>= (- (percent-negative str) (percent-positive str)) 0.05) "negative"]
      [else
       "neutral"])))

;;;===========================================================================================================================================================


;Part four: Basic gender analysis

; (count-one word str) -> integer?
; word : String
; str : String
(define count-one
  (lambda (word str)
    (length(filter (lambda (w)
                     (equal? w word)) (help-extract-words str)))))

; (count-male-female wordlist str) -> integer?
; wordlist : List
; str : String
(define count-male-female
  (lambda (wordlist str)
    (reduce + (map (lambda (word)(count-one word str)) wordlist))))


; (count-male-pronouns str) -> integer?
; str : String
(define count-male-pronouns
  (lambda (str)
    (count-male-female (list "he" "him" "his") str)))


; (count-female-pronouns str) -> integer?
; str : String
(define count-female-pronouns
  (lambda (str)
    (count-male-female (list "she" "her" "hers") str)))

;;;===========================================================================================================================================================


;Part five: Freestyle


; (term-frequency word str) -> number?
; word : String
; str : String
(define term-frequency
  (lambda (word str)
    (let* ([word-occurence (count-one word str)]
           [total-words (length(help-extract-words str))])
      (/ word-occurence total-words))))

; (present? word str) -> boolean?
; word : String
; str : String
(define present?
  (lambda (word str)
    ( if (member? word (help-extract-words str))
         #t #f)))

; (sentence-with-word word str) -> integer?
; word : String
; str : String
(define sentence-with-word
  ( lambda (word str)
     (length(filter (lambda (sentence) (present? word sentence)) (filter (lambda (line) (not (equal? line ""))) (rex-split-string splitter str))))))

; (inverse-document-frequency word str) -> number?
; word : String
; str : String
(define inverse-document-frequency
  (lambda (word str)
    (let* ([num-of-sen (count-sentence str)]
           [num-of-sen-with-word (sentence-with-word word str)])
      (if (= num-of-sen-with-word 0) 0 (log (/ num-of-sen num-of-sen-with-word))))))
   
; (TF-IDF word str) -> number?
; word : String
; str : String
(define TF-IDF
  (lambda (word str)
    (* (term-frequency word str) (inverse-document-frequency word str))))


; (proper-noun-extraction str) -> list?
; str : String
(define proper-noun-extraction
  (lambda (str)
    (regexp-match* #rx"(?<=[a-z,;:&])([A-Z][a-z]+)" str)))

;;;===========================================================================================================================================================
