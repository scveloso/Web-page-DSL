#lang racket

;; get-input-file : port -> string
;; Produces a port to the user input file
(define (get-input-file)
  (open-input-file "test/sample.txt" #:mode 'text))

;; read-user-input : port -> string
;; Produces a port to the user input file
(define (read-user-input input-file)
  (read-line input-file))

(provide (all-defined-out))
