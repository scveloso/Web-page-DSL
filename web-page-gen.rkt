#lang plai

(require "file-writer.rkt")
(require "file-reader.rkt")

(define-type exp
  [create-paragraph (t string?)])

(define-type styling
  [styling-color (c string?)]
  [styling-font (f string?)]
  [styling-size (s string?)]) ;; TODO: add support for different spacing units

;; extract-text-from-create-paragraph-call : string -> string
;; Returns only the text in a create paragraph statement
(define (extract-text-from-create-paragraph-call call)
  (second (string-split call "\"")))

;; extract-style : string -> string
;; Returns only the style text in a create paragraph statement
(define (extract-style exp)
  (second (string-split exp "\" ")))

;; extract-style-item : string -> string
;; Returns the actual styling in a style string (e.g. font comic sans -> comic sans)
(define (extract-style-item style-str)
  (string-join (map (λ (s) s) (rest (string-split style-str " "))) " "))

;; parse : any -> exp
;; Consumes an s-expression (in our concrete "surface" syntax) and
;; generates the corresponding expression.
(define (parse exp)
  (match exp
    [(regexp #rx"Create paragraph .*") (create-paragraph (extract-text-from-create-paragraph-call exp))]
    [_ (error 'parse "unable to parse ~a" exp)]))

;; parse : any -> listof styling
;; Consumes an s-expression of the styling (in our concrete "surface" syntax) and
;; generates the corresponding list of style expressions.
(define (parse-style style-string)
  (local ([define styles-list (string-split style-string ", ")]
          [define (helper style-str style-acc)
             (match style-str
               [(regexp #rx"color .*") (cons style-acc (styling-color (extract-style-item style-str)))]
               [(regexp #rx"font .*") (cons style-acc (styling-font (extract-style-item style-str)))]
               [(regexp #rx"size .*") (cons style-acc (styling-size (extract-style-item style-str)))]
               [_ (error 'parse "unable to parse ~a" style-str)])])
    (foldl helper empty styles-list)))

;; interp : exp -> string
;; consumes an exp and returns a HTML component in a string
(define (interp the-exp)
  ; We're consuming an AE, which leads directly to a natural template
  ; and at least some of our tests.
  (type-case exp the-exp
    [create-paragraph (s) (string-append "<p>" (string-append s "</p>"))])
  )

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

;; *** TESTS ***
(define create-paragraph-test "Create paragraph \"Lorem ipsum. \" color red, font comic sans, size 12")

;; Helper tests
(test (extract-text-from-create-paragraph-call create-paragraph-test) "Lorem ipsum. ")
(test (extract-style create-paragraph-test) "color red, font comic sans, size 12")

;; Parse tests
(test (parse create-paragraph-test) (create-paragraph "Lorem ipsum. "))
(test (parse-style "color red, font comic sans, size 12") (cons (cons (cons '() (styling-color "red")) (styling-font "comic sans")) (styling-size "12")))