(define (count-up x y action)
    (action x)
    (if (= x y)
        (newline)
        (count-up (+ 1 x) y action)
    )
)

(define (print1 x)
    (display (format "~a " x))
)

(count-up 5 11 print1)


(define (square x)
(* x x))
(display (map square '(1 2 3 4 5 6 7 8 9)))
(newline)
(display (map (lambda (x) (* x x)) '(2 4 6 8 10)))
(newline)
(define (my-select lst)
    (if (null? lst)
        '()
        (if (> (car lst) 0 )
            (cons (car lst) (my-select (cdr lst)))
            (my-select (cdr lst))
        )
    )
)
(display (my-select '(1 -2 3 4 -5 6 -7 9 10)))
(newline)
(require 'list-lib)
(define lst3 '(1 4 7 11 2 5))
(display 
    (reduce (lambda (item partial) (if (> item partial) item partial)) 
    'empty-list lst3)
)
(newline)

(define (count-positive lst)
    (fold 
        (lambda (item partial) 
            (if (> item 0) (+ partial 1) partial)
        )
        0 
        lst
    )
)
(define lst4 '(-5 -6 1 4 -5 7 9 -2))
(display (count-positive lst4))
(newline)

(define (my-map action lst)
    (fold 
        (lambda (item partial) 
            (append partial (list (action item)))
        )
        '()
        lst
    )
)
(display (my-map (lambda (x) (* x x)) '(1 2 3 4 5 6 7)))
(newline)

(define (my-map2 action lst)
    (fold-right
        (lambda (item partial) 
            (cons (action item) partial)
        )
        '()
        lst
    )
)
(display (my-map2 (lambda (x) (* x x)) '(1 2 3 4 5 6 7)))
(newline)


(define (map-by-index action max)
    (if (= max 0)
        '()
        (append (map-by-index action (- max 1)) (list (action (- max 1))))
    )
)

(define (contains value lst)
    (fold 
        (lambda (item partial) (or (eq? value item) partial))
        #f
        lst
    )
)

(define alphabet '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
(define test '((0 0 0 0 0 0) (1 1 1 1 1 1) (2 1 0 1 0 1) (3 0 0 0 0 0) (4 1 1 0 0 0) (5 0 0 1 1 1) (6 1 1 1 1 1)))
(define (print-board lst)
  (let ((transposed-board (apply map list lst)))
    (display " ")
    (do ((i 0 (+ i 1)))
        ((= i (length (car lst))))
      (display (list-ref alphabet i))
      (display " "))
    (newline)
    (do ((i 0 (+ i 1)))
        ((= i (length lst)))
      (display i)
      (display " ")
      (do ((j 0 (+ j 1)))
          ((= j (length (car lst))))
        (display (list-ref (list-ref transposed-board i) j))
        (display " "))
      (newline))))

(newline)
(print-board test)
