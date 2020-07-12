 (define hello-world
 (lambda (x)
     (begin
        (newline)
        (display "Hello World")
        (display " ")
        (display x)
        (newline))))

(hello-world "Andy")
(hello-world "Barbara")
(hello-world "Connie")

