#lang plai

(require "file-writer.rkt")
(require "file-reader.rkt")

;; The language our interpreter operates in, comprised of a unique id,
;; an HTML element and the list of styles to be applied to the element
(define-type expression
  [expr (id symbol?) (elt element?) (styles (λ (x) (andmap styling? x)))])

;; An HTML element
;; TODO: More features
(define-type element
  [create-paragraph (t string?)]
  [create-heading (t string?) (l string?)])

;; The styling to be assigned to an HTML element
;; TODO: More styles
(define-type styling
  [styling-color (c string?)]
  [styling-font (f string?)]
  [styling-size (s string?)]) ;; TODO: add support for different spacing units

;; A result object encapsulating what to print out to the .html and .css
;; files for each Webpage DSL element
(define-type exp-result
  [result (html-str string?) (css-strs (λ (x) (andmap string? x)))])

;; extract-text-from-create-statement : string -> string
;; Returns only the text in a create statement
(define (extract-text-from-create-statement stmt)
  (second (string-split stmt "\"")))

;; extract-text-from-create-statement : string -> number
;; Returns only the level of the create heading statement
;; e.g. Create heading level 2 "sample text" -> 2
(define (extract-heading-level-from-create-statement stmt)
  (fourth (string-split stmt " ")))

;; extract-style : string -> string
;; Returns only the style text in a create paragraph statement
(define (extract-style sexp)
  (second (string-split sexp "\" ")))

;; extract-style-item : string -> string
;; Returns the actual styling in a style string (e.g. font comic sans -> comic sans)
(define (extract-style-item style-str)
  (string-join (map (λ (s) s) (rest (string-split style-str " "))) " "))

;; parse : listof sexp -> listof expression
;; Consumes a list of s-expressions (in our concrete "surface" syntax) and
;; generates the corresponding expressions.
(define (parse lof-sexp)
  (map (λ (sexp)
         (expr
          (gensym)
          (parse-element sexp)
          (parse-style (extract-style sexp))))
       lof-sexp))

;; parse : sexp -> element
;; Consumes an s-expression (in our concrete "surface" syntax) and
;; generates a HTML element
(define (parse-element elt-sexp)
  (match elt-sexp
    [(regexp #rx"^Create paragraph .*$") (create-paragraph (extract-text-from-create-statement elt-sexp))]
    [(regexp #rx"^Create heading level [1-6] .*$") (create-heading (extract-text-from-create-statement elt-sexp) (extract-heading-level-from-create-statement elt-sexp))]
    [_ (error 'parse "unable to parse ~a" elt-sexp)]))

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

;; interp : listof expression -> listof exp-result
;; consumes an exp and returns a list of exp-result, HTML and CSS elements in strings to be written
;; inside the output HTML and CSS files
(define (interp the-exprs)
  (local (;; interp-elt : listof element -> listof string
          ;; consumes an element expression and returns the appropriate html string to print to a .html file
          [define (interp-elt elt the-exp-id)
            (type-case element elt
              [create-paragraph (s) (string-append "<p id=\""
                                                   (string-append (symbol->string the-exp-id)
                                                                  (string-append "\">"
                                                                                 (string-append s "</p>"))))]
              [create-heading (s l) (string-append "<h"
                                                   (string-append l
                                                                  (string-append " id=\""
                                                                                 (string-append (symbol->string the-exp-id)
                                                                                                (string-append "\">"
                                                                                                               (string-append s "</h"
                                                                                                                              (string-append l ">")))))))])]

          ;; interp-styles : listof styling -> listof string
          ;; consumes a list of style expressions and returns an appropriate list of css strings to print to a .css file
          [define (interp-styles styles the-exp-id)
            (append (list (string-append "#" (string-append (symbol->string the-exp-id) " {")))
                         (map (λ (style-exp) (interp-style style-exp)) styles)
                         (list (string-append "}\n")))]

          ;; interp-style : styling -> string
          ;; consumes a style expression and returns the appropriate css string to print to a .css file
          [define (interp-style style)
            (type-case styling style
              [styling-color (c) (string-append "\tcolor: " (string-append c ";"))]
              [styling-font (f) (string-append "\tfont-family: \"" (string-append f "\";"))]
              [styling-size (s) (string-append "\tfont-size: " (string-append s "px;"))])]

          [define (helper the-exp)
            (local ([define the-exp-id (expr-id the-exp)])
              (result (interp-elt (expr-elt the-exp) the-exp-id) (interp-styles (expr-styles the-exp) the-exp-id)))])
    (map helper the-exprs)))

;; interpret-user-input () -> string
;; Produces the HTML to be generated from the user input
(define (interpret-user-input)
  (interp (parse (read-user-input (get-input-file)))))

;; run : () -> void
;; Processes the user input in sample.txt by parsing and interpreting it and generating an
;; HTML component. Then, creates sample.html, a file containing the generated HTML
(define (run)
  (local ([define results (interpret-user-input)]
          [define html-results (map (λ (result) (result-html-str result)) results)]
          [define css-results (map (λ (result) (result-css-strs result)) results)])
    (begin
      (close-output-port
       (write-end-html-to-output-file
        (write-strings-to-output-file
         html-results
         (write-start-html-to-output-file
          (create-html-file)))))
      (close-output-port
       (foldl (λ (los acc) (write-strings-to-output-file los acc)) (create-css-file) css-results)))))

;; *** TESTS ***
(define create-paragraph-test "Create paragraph \"Lorem ipsum. \" color red, font Comic Sans MS, size 12")
(define create-heading-test "Create heading level 2 \"Heading text \" color blue, font Times New Roman, size 14")

;; Helper tests
(test (extract-text-from-create-statement create-paragraph-test) "Lorem ipsum. ")
(test (extract-text-from-create-statement create-heading-test) "Heading text ")
(test (extract-heading-level-from-create-statement create-heading-test) "2")
(test (extract-style create-paragraph-test) "color red, font Comic Sans MS, size 12")
(test (extract-style-item "color red") "red")
(test (extract-style-item "font comic sans") "comic sans")

;; Parse tests
;; Parse one expression
(let ([exprs (parse (list create-paragraph-test))])
  (begin
    (test (expr-elt (first exprs)) (create-paragraph "Lorem ipsum. "))
    (test (expr-styles (first exprs)) (list (styling-color "red") (styling-font "Comic Sans MS") (styling-size "12")))))

;; Parse two expressions
(let ([exprs (parse (list create-paragraph-test create-heading-test))])
  (begin
    (test (expr-elt (first exprs)) (create-paragraph "Lorem ipsum. "))
    (test (expr-elt (second exprs)) (create-heading "Heading text " "2"))
    (test (expr-styles (first exprs)) (list (styling-color "red") (styling-font "Comic Sans MS") (styling-size "12")))
    (test (expr-styles (second exprs)) (list (styling-color "blue") (styling-font "Times New Roman") (styling-size "14")))))

;; Parse helper tests
(test (parse-element create-paragraph-test) (create-paragraph "Lorem ipsum. "))
(test (parse-style "color red, font Comic Sans MS, size 12") (list (styling-color "red") (styling-font "Comic Sans MS") (styling-size "12")))

;; Interp tests
;; Need to use test/pred and lambdas here since interp results have variable ids 
(let ([exprs (interp (parse (list create-paragraph-test create-heading-test)))])
  (begin
    (test/pred (result-html-str (first exprs)) (λ (result-exp) (regexp-match #rx"<p id=\"g[0-9]*\">Lorem ipsum. </p>" result-exp)))
    (test/pred (result-html-str (second exprs)) (λ (result-exp) (regexp-match #rx"<h2 id=\"g[0-9]*\">Heading text </h2>" result-exp)))))