(define-test-suite "Input Reading")
(define-test-suite "Increase counting")

(define pos-int-gen (lambda () (random 65536)))

(define (pos-int-list-gen length)
    (lambda ()
        (define (inner-rec current remaining)
            (if (<= remaining 0) 
                current
                (inner-rec (cons (pos-int-gen) current) (- remaining 1))
            )
        )
        (inner-rec (list) length)
    )
)

(define (pos-int-list-random-length-gen)
    (lambda () ((pos-int-list-gen (+ 1 (random 256)))))
)


(define (verify-property generator property)
    (define (inner-rec iterations)
        (let* ((param (generator)) (result (property param)))
            (if (<= iterations 1)
                result
                (inner-rec (- iterations 1))
            )
        )
    )
    (inner-rec 1000)
)

(begin-test-suite "Input Reading")

(define-test "read-lines-from-port reads lines from input"
    (let ((port (open-input-string "one\ntwo\nthree")))
        (assert-equals (list "one" "two" "three") (read-lines-from-port port))
    )
    (let* (
            (port (open-input-string "four\nfive\nsix"))
            (result (read-lines-from-port port))
        )
        (assert-equals (list "four" "five" "six") result result)
    )
)

(define-test "read-lines-from-port reads first element from input"
    (let ((port (open-input-string "one\ntwo\nthree")))
        (assert-equals "one" (car (read-lines-from-port port)))
    )
    (let ((port (open-input-string "four\nfive\nsix")))
        (assert-equals "four" (car (read-lines-from-port port)))
    )
)

(define-test "read-lines-from-port reads empty input"
    (let ((port (open-input-string "")))
        (assert-equals (list) (read-lines-from-port port))
    )
)

(end-test-suite)

(begin-test-suite "Increase counting")

(define-test "an empty list gives zero" (assert-equals 0 (count-increases (list))))

(define-test "a single item list gives zero"
    (verify-property (pos-int-list-gen 1)
        (lambda (items) 
            (assert-equals 0 (count-increases items))
        )
    )
)

(define-test "a list with a larger value prepended gives the same result"
    (verify-property (pos-int-list-gen 7)
        (lambda (items)
            (assert-equals
                (count-increases items)
                (count-increases (cons (+ (car items) 1) items))
            )
        )
    )
)

(define-test "a list with a smaller value prepended gives a result incremented by 1"
    (verify-property (pos-int-list-gen 7)
        (lambda (items)
            (assert-equals
                (+ (count-increases items) 1)
                (count-increases (cons (- (car items) 1) items))
            )
        )
    )
)

(define-test "a list with the same value prependedgives the same result"
    (verify-property (pos-int-list-random-length-gen)
        (lambda (items)
            (assert-equals
                (count-increases items)
                (count-increases (cons (car items) items))
            )
        )
    )
)

(end-test-suite)
