#lang plai

(require "file-writer.rkt")
(require "file-reader.rkt")

;; The language our interpreter operates in, comprised
;; of a HTML element and the styling to be applied to the element
(define-type expression
  [expr (elt element?) (style (λ (x) (andmap styling? x)))])

;; An HTML element
(define-type element
  [create-paragraph (t string?)])

;; The styling to be assigned to an HTML element
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
(define (extract-style sexp)
  (second (string-split sexp "\" ")))

;; extract-style-item : string -> string
;; Returns the actual styling in a style string (e.g. font comic sans -> comic sans)
(define (extract-style-item style-str)
  (string-join (map (λ (s) s) (rest (string-split style-str " "))) " "))

;; parse : listof sexp -> listof exp
;; Consumes a list of s-expressions (in our concrete "surface" syntax) and
;; generates the corresponding expressions.
(define (parse lof-sexp)
  (local ([define (helper sexp)
            (match sexp
              [(regexp #rx"Create paragraph .*") (expr
                                                  (create-paragraph (extract-text-from-create-paragraph-call sexp))
                                                  (parse-style (extract-style sexp)))]
              [_ (error 'parse "unable to parse ~a" sexp)])])
    (map helper lof-sexp)))

;; parse : sexp -> listof styling
;; Consumes an s-expression of the styling (in our concrete "surface" syntax) and
;; generates the corresponding list of style expressions.
;; (e.g. "color red font comic sans" -> (list (styling-color "red") (styling-font "comic sans"))
(define (parse-style style-sexp)
  (local ([define styles-list (string-split style-sexp ", ")]
          [define (helper style-str)
             (match style-str
               [(regexp #rx"color .*") (styling-color (extract-style-item style-str))]
               [(regexp #rx"font .*") (styling-font (extract-style-item style-str))]
               [(regexp #rx"size .*") (styling-size (extract-style-item style-str))]
               [_ (error 'parse "unable to parse ~a" style-str)])])
    (map helper styles-list)))

;; interp : expression -> string
;; consumes an exp and returns a HTML component in a string
(define (interp the-exp)
  (type-case element (expr-elt the-exp)
    [create-paragraph (s) (string-append "<p>" (string-append s "</p>"))]))

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
(test (parse (list create-paragraph-test)) (list (expr
                                                  (create-paragraph "Lorem ipsum. ")
                                                  (list (styling-color "red") (styling-font "comic sans") (styling-size "12")))))
(test (parse (list create-paragraph-test create-paragraph-test)) (list (expr
                                                                        (create-paragraph "Lorem ipsum. ")
                                                                        (list (styling-color "red") (styling-font "comic sans") (styling-size "12")))
                                                                       (expr
                                                                        (create-paragraph "Lorem ipsum. ")
                                                                        (list (styling-color "red") (styling-font "comic sans") (styling-size "12")))))
(test (parse-style "color red, font comic sans, size 12") (list (styling-color "red") (styling-font "comic sans") (styling-size "12")))