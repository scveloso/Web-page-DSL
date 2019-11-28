#lang racket

;; get-input-file : () -> port
;; Produces a port to the user input file
(define (get-input-file)
  (open-input-file "test/sample.txt" #:mode 'text))

;; read-user-input : port -> listof string
;; Produces the list of strings read from the port
;; TODO: Maybe trim empty lines
(define (read-user-input input-file)
  (local [(define (helper input-file line-acc)
           (let ((line (read-line input-file 'any)))
             (if (eof-object? line)
                 (reverse line-acc)
                 (helper input-file (cons line line-acc)))))]
    (helper input-file empty)))

(provide (all-defined-out))
