#lang plai

(require "file-writer.rkt")
(require "file-reader.rkt")

(define-type exp
  [create-paragraph (t string?)])

;; extract-text-from-create-paragraph-call : string -> string
;; Returns only the text in a create paragraph statement
(define (extract-text-from-create-paragraph-call call)
  (rest (rest (string-split call " "))))

;; build-string-w-spaces : string string -> string
;; Adds the first given string to the second given string with a space in between
(define (build-string-w-spaces s acc)
  (string-append acc (string-append " " s)))

;; parse : any -> exp
(define (parse exp)
  (match exp
    [(regexp #rx"Create paragraph .*") (create-paragraph (foldl build-string-w-spaces "" (extract-text-from-create-paragraph-call exp)))]
    [_ (error 'parse "unable to parse ~a" exp)]))

;; interp : exp -> string
;; consumes an exp and returns a HTML component in a string
(define (interp the-exp)
  ; We're consuming an AE, which leads directly to a natural template
  ; and at least some of our tests.
  (type-case exp the-exp
    [create-paragraph (s) (string-append "<p>" (string-append s "</p>"))])
  )

;; TODO: Tests for parse, interp and helpers


;; interpret-user-input () -> string
;; Produces the HTML to be generated from the user input
(define (interpret-user-input)
  (interp (parse (read-user-input (get-input-file)))))

;; run : () -> void
;; Processes the user input in sample.txt by parsing and interpreting it and generating an
;; HTML component. Then, creates sample.html, a file containing the generated HTML
(define (run)
  (close-output-port
   (write-end-html-to-output-file
    (write-body-to-output-file
     (interpret-user-input)
     (write-start-html-to-output-file
      (create-html-file))))))