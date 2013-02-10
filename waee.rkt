#lang plai
(define-type WAEE
    (num (n number?))
    (add (lhs WAEE?) (rhs WAEE?))
    (sub (lhs WAEE?) (rhs WAEE?))
    (with (name symbol?) (named-expr WAEE?) (body WAEE?))
    (id (name symbol?)))

(define subst
    (lambda (expr sub-id val)
        (type-case WAEE expr
            (num (x) expr)
            (add (l r) (add (subst l sub-id val) (subst r sub-id val)))
            (sub (l r) (sub (subst l sub-id val) (subst r sub-id val)))
            (with (bound-id named-expr bound-body)
                (if (symbol=? bound-id sub-id)
                    (with bound-id (subst named-expr sub-id val) bound-body)
                    (with bound-id (subst named-expr sub-id val) (subst bound-body sub-id val))))
            (id (v) (if (symbol=? v sub-id) val expr)))))

(define parse-waee
    (lambda (SEXP)
        (cond
            ((number? SEXP) (num SEXP))
            ((list? SEXP)
                (case (first SEXP)
                    ((+) (add (parse-waee (second SEXP)) (parse-waee (third SEXP))))
                    ((-) (sub (parse-waee (second SEXP)) (parse-waee (third SEXP))))
                    ((with) (with (first (second SEXP)) (parse-waee (second (second SEXP))) (parse-waee (third SEXP))))))
            ((symbol? SEXP) (id SEXP)))))

(define interp-waee
    (lambda (a-waee)
        (type-case WAEE a-waee
            (num (x) x)
            (add (l r) (+ (interp-waee l) (interp-waee r)))
            (sub (l r) (- (interp-waee l) (interp-waee r)))
            (with (bound-id named-expr bound-body) (interp-waee (subst bound-body bound-id (num (interp-waee named-expr)))))
            (id (v) (error 'interp-waee->id (string-append "Found a free identifier (" (symbol->string v) ")"))))))

(define eval-waee (lambda (SEXP) (interp-waee (parse-waee SEXP))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; returns 5
(interp-waee (num 5))

; returns 8
(interp-waee (add (num 3) (num 5)))

; returns 5
(num-n (num 5))

; returns (num 3)
(add-lhs (add (num 3) (num 5)))

; returns (with 'x (num 5) (add (id 'x) (id 'x)))
(parse-waee '{with {x 5} {+ x x}})

; returns -35
(eval-waee '{+ {- {- 4 3} 15} {+ {+ {- 10 5} {- 3 2}} {- 15 42}}})

; returns 20
(eval-waee '{with {x 5} {with {x {+ x x}} {+ x x}}})

; returns 60
(eval-waee '{with {x 10} {with {y {+ x x}} {with {z {+ y x}} {+ z {+ x y}}}}})

; returns an error (free identifier)
(eval-waee '{with {x 5} {with {x {+ x x}} {+ y x}}})