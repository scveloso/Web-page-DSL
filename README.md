# Web-page-DSL
A proof of concept for a DSL that generates web-pages.

The project itself is a DSL with concrete syntax resembling everyday language. The DSL will consist of a transpiler that first parses the concrete syntax of the language--checking for errors as it goes. The transpiler then, interprets the meaning of the syntax and generates HTML/CSS to actually produce the web-page that the user envisions.

## Usage
- Type in DSL code in ```./test/sample.txt``` (There's sample code already in there)
- Open web-page-gen.rkt in DrRacket IDE
- Run the code
- Call function (run) in Racket interaction window
- Open ```./test/sample.html``` on your browser
