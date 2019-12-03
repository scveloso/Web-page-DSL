#lang plai

(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)

(require "file-writer.rkt")
(require "file-reader.rkt")

;; The language our interpreter operates in, comprised of a unique id,
;; an HTML element and the list of styles to be applied to the element
(define-type expression
  [expr (id symbol?) (elt element?) (styles (λ (x) (andmap styling? x)))])

;; An HTML element
(define-type element
  [create-paragraph (t string?)]
  [create-heading (t string?) (l string?)]
  [create-image (src string?)]
  [body]) ;; used for background

;; The styling to be assigned to an HTML element
;; For DSL simplicity reasons, only use "pt" as the unit of spacing - everyday user is used to this unit (word, google docs, etc).
(define-type styling
  [styling-color (c string?)]
  [styling-font (f string?)]
  [styling-size (s string?)]
  [styling-alignment (a alignment?)]
  [styling-bg-image (img string?)]
  [styling-bg-color (c string?)]
  [styling-bg-repeat])

;; The alignment for the element
(define-type alignment
  [align-center]
  [align-left]
  [align-right])

;; A result object encapsulating what to print out to the .html and .css
;; files for each Webpage DSL element
(define-type exp-result
  [result (html-str string?) (css-strs (λ (x) (andmap string? x)))])

;; Currently does not do escaping ... do not use quotes inside text
(define slwg-text-content/p
  (do (char/p #\")
      [text <- (many/p (char-not/p #\"))]
      (char/p #\")
      (pure (list->string text))))

(define slwg-styling-ident/p
  (do [text <- (many/p (char-not-in/p ",\n"))]
      (pure (list->string text))))

(define slwg-text-styling-color/p
  (do (or/p (try/p (string/p "color"))
            (string/p "colour"))
      (many+/p space/p)
      [color <- slwg-styling-ident/p]
      (pure (styling-color color))))

(define slwg-text-styling-font/p
  (do (string/p "font")
      (many+/p space/p)
      [font <- slwg-styling-ident/p]
      (pure (styling-font font))))

(define slwg-text-styling-size/p
  (do (string/p "size")
      (many+/p space/p)
      [size <- slwg-styling-ident/p]
      (pure (styling-size size))))

(define slwg-styling-align/p
  (or/p (do (string/p "center")
            (pure (align-center)))
        (do (string/p "left")
            (pure (align-left)))
        (do (string/p "right")
            (pure (align-right)))))

(define slwg-text-styling-align/p
  (do (or/p (try/p (string/p "aligned"))
            (string/p "align")
            (string/p "move")
            (string/p "to")
            (string/p "on")
            (string/p "in"))
      (many+/p space/p)
      [align <- slwg-styling-align/p]
      (pure (styling-alignment align))))

(define slwg-text-styling-bg-img/p
  (do (string/p "background-image")
      (many+/p space/p)
      [image <- slwg-styling-ident/p]
      (pure (styling-bg-image image))))

(define slwg-text-styling-bg-color/p
  (do (string/p "background-color")
      (many+/p space/p)
      [color <- slwg-styling-ident/p]
      (pure (styling-bg-color color))))

(define slwg-text-styling-bg-repeat/p
  (do (string/p "repeat")
      (pure (styling-bg-repeat))))

(define slwg-text-styling/p
  (or/p slwg-text-styling-color/p
        slwg-text-styling-font/p
        slwg-text-styling-size/p
        slwg-text-styling-align/p
        slwg-text-styling-bg-img/p
        slwg-text-styling-bg-color/p
        slwg-text-styling-bg-repeat/p))

(define slwg-text-styling-sep/p
  (do (char/p #\,)
      (many/p space/p)))

(define slwg-text-stylings/p
  (many/p #:sep slwg-text-styling-sep/p slwg-text-styling/p))

(define slwg-header-level/p
  integer/p)

(define slwg-header/p
  (do (or/p (try/p (string-ci/p "header"))
            (string-ci/p "heading"))
      (many+/p space/p)
      (string/p "level")
      (many+/p space/p)
      [level <- slwg-header-level/p]
      (many+/p space/p)
      [text <- slwg-text-content/p]
      (pure (create-heading text (number->string level)))))

(define slwg-paragraph/p
  (do (string-ci/p "paragraph")
      (many+/p space/p)
      [text <- slwg-text-content/p]
      (pure (create-paragraph text))))

(define todo
  (message (srcloc 0 #f #f #f #f) #f '("TODO")))

(define slwg-list/p
  (fail/p todo))

(define slwg-image/p
  (do (string-ci/p "image")
      (many+/p space/p)
      [img-link <- slwg-text-content/p]
      (pure (create-image img-link))))

(define slwg-background/p
  (do (string-ci/p "set")
            (many+/p space/p)
            (string-ci/p "background")
            (many+/p space/p)
            [styles <- slwg-text-stylings/p]
            (pure (expr 'body (body) styles))))

(define slwg-constructor/p
  (or/p (string-ci/p "add")
        (try/p (string-ci/p "construct"))
        (string-ci/p "create")
        (string-ci/p "insert")
        (string-ci/p "make")))

(define slwg-fill-opt/p
  (or/p (string-ci/p "add")
        (try/p (string-ci/p "construct"))
        (string-ci/p "create")
        (string-ci/p "insert")
        (string-ci/p "make")))

(define slwg-constructable/p
  (or/p slwg-header/p
        slwg-paragraph/p
        slwg-list/p
        slwg-image/p))

(define slwg/p
  (or/p (try/p slwg-background/p)
        (do slwg-constructor/p
            (many+/p space/p)
            [elem <- slwg-constructable/p]
            (many/p space/p)
            [styles <- slwg-text-stylings/p]
            (pure (expr (gensym) elem styles)))))

#;
(define slwgs/p
  (do [prog <- (many/p #:sep space/p slwg/p)]
      (eof/p)
      (pure prog)))

(define (gen-css-id)
  (string->symbol (string-append "#" (symbol->string (gensym)))))

(define (parse lof-sexp)
  (map (λ (str) (parse-result! (parse-string slwg/p str))) lof-sexp))

;; interp : listof expression -> listof exp-result
;; consumes an exp and returns a list of exp-result, HTML and CSS elements in strings to be written
;; inside the output HTML and CSS files
(define (interp the-exprs)
  (local (;; interp-elt : element symbol -> string
          ;; consumes an element expression and its id and returns the appropriate html string to print to a .html file
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
                                                                                                                              (string-append l ">")))))))]
              [create-image (src) (string-append "<img src=\""
                                                 (string-append src
                                                                (string-append "\" id=\""
                                                                                 (string-append (symbol->string the-exp-id) "\">"))))]
              [body () ""] ;; body automatically added, this is more to carry background styling
              )]
          ;; interp-alignment : alignment -> string
          ;; consumes an alignment and returns the css string to print to a .css file
          [define (interp-alignment almt)
            (type-case alignment almt
              [align-center () "\tmargin: auto;\n\twidth: 50%;"]
              [align-left () "\tfloat: left;\n\twidth: 50%;"]
              [align-right () "\tfloat: right;\n\twidth: 50%;"])]
          
          ;; interp-styles : listof styling, symbol -> listof string
          ;; consumes a list of style expressions and the id of the HTML element these styles refer to
          ;; and returns the corresponding list of css strings to print to a .css file
          [define (interp-styles styles the-exp-id)
            (local ([define css-id-str (if (equal? the-exp-id 'body)
                                       (symbol->string the-exp-id)
                                       (string-append "#" (symbol->string the-exp-id)))])
              (append (list (string-append css-id-str " {"))
                      (map (λ (style-exp) (interp-style style-exp)) styles)
                      (list (string-append "}\n"))))]

          ;; interp-style : styling -> string
          ;; consumes a style expression and returns the appropriate css string to print to a .css file
          [define (interp-style style)
            (type-case styling style
              [styling-color (c) (string-append "\tcolor: " (string-append c ";"))]
              [styling-font (f) (string-append "\tfont-family: \"" (string-append f "\";"))]
              [styling-size (s) (string-append "\tfont-size: " (string-append s "pt;"))]
              [styling-alignment (a) (interp-alignment a)]
              [styling-bg-image (s) (string-append "\tbackground-image: url(\"" (string-append s "\");"))]
              [styling-bg-color (c) (string-append "\tbackground-color: " (string-append c ";"))]
              [styling-bg-repeat () (string-append "\tbackground-repeat: repeat;")])]

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
(define (css-l-to-s css-l)
  (foldl (λ (s acc) (string-append acc s)) "" css-l))

(define create-paragraph-test "Create paragraph \"Lorem ipsum. \" color red, font Comic Sans MS, size 12, align center")
(define create-heading-test "Create heading level 2 \"Heading text \" color blue, font Times New Roman, size 14, align right")

;; Parse tests
;; Parse one expression
(let ([exprs (parse (list create-paragraph-test))])
  (begin
    (test (expr-elt (first exprs)) (create-paragraph "Lorem ipsum. "))
    (test (expr-styles (first exprs)) (list (styling-color "red") (styling-font "Comic Sans MS") (styling-size "12") (styling-alignment (align-center))))))

;; Parse two expressions
(let ([exprs (parse (list create-paragraph-test create-heading-test))])
  (begin
    (test (expr-elt (first exprs)) (create-paragraph "Lorem ipsum. "))
    (test (expr-elt (second exprs)) (create-heading "Heading text " "2"))
    (test (expr-styles (first exprs)) (list (styling-color "red") (styling-font "Comic Sans MS") (styling-size "12") (styling-alignment (align-center))))
    (test (expr-styles (second exprs)) (list (styling-color "blue") (styling-font "Times New Roman") (styling-size "14") (styling-alignment (align-right))))))

;; Interp tests
;; Need to use test/pred and lambdas here since interp results have variable ids 
(let ([exprs (interp (parse (list create-paragraph-test create-heading-test)))])
  (begin
    (test/pred (result-html-str (first exprs)) (λ (result-exp) (regexp-match #rx"<p id=\"g[0-9]*\">Lorem ipsum. </p>" result-exp)))
    (test/pred (result-css-strs (first exprs)) (λ (result-exp) (regexp-match #rx"#g[0-9]* {\tcolor: red;\tfont-family: \"Comic Sans MS\";\tfont-size: 12pt;\tmargin: auto;\n\twidth: 50%;}\n" (css-l-to-s result-exp))))
    (test/pred (result-html-str (second exprs)) (λ (result-exp) (regexp-match #rx"<h2 id=\"g[0-9]*\">Heading text </h2>" result-exp)))
    (test/pred (result-css-strs (second exprs)) (λ (result-exp) (regexp-match #rx"#g[0-9]* {\tcolor: blue;\tfont-family: \"Times New Roman\";\tfont-size: 14pt;\tfloat: right;\n\twidth: 50%;}\n" (css-l-to-s result-exp))))))