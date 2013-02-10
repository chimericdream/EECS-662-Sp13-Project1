#lang plai
(define-type WAE
    (num (n number?))
    (add (lhs WAE?) (rhs WAE?))
    (sub (lhs WAE?) (rhs WAE?))
    (with (name symbol?) (named-expr WAE?) (body WAE?))
    (id (name symbol?)))

(define subst
    (lambda (expr sub-id val)
        (type-case WAE expr
            (num (x) expr)
            (add (l r) (add (subst l sub-id val) (subst r sub-id val)))
            (sub (l r) (sub (subst l sub-id val) (subst r sub-id val)))
            (with (bound-id named-expr bound-body)
                (if (symbol=? bound-id sub-id)
                    (with
                        bound-id
                        (subst named-expr sub-id val)
                        bound-body)
                    (with
                        bound-id
                        (subst named-expr sub-id val)
                        (subst bound-body sub-id val))
                )
            ) ; with
            (id (v) (if (symbol=? v sub-id) val expr))
        ) ; type-case
    ) ; lambda
) ; define subst

(define parse
    (lambda (SEXP)
        (cond
            ((number? SEXP) (num SEXP))
            ((list? SEXP)
                (case (first SEXP)
                    ((+) (add
                        (parse (second SEXP))
                        (parse (third SEXP))))
                    ((-) (sub
                        (parse (second SEXP))
                        (parse (third SEXP))))
                    ((with) (with
                        (first (second SEXP))
                        (parse (second (second SEXP)))
                        (parse (third SEXP))))
                ) ; case
            ) ; list
            ((symbol? SEXP) (id SEXP))
        ) ; cond
    ) ; lambda
) ; define parse

(define calc
    (lambda (a-wae)
        (type-case WAE a-wae
            (num (x) x)
            (add (l r) (+ (calc l) (calc r)))
            (sub (l r) (- (calc l) (calc r)))
            (with (bound-id named-expr bound-body)
                (calc (subst bound-body bound-id (num (calc named-expr))))
            )
            (id (v) (error "BAD"))
        ) ; type-case
    ) ; lambda
) ; define calc

(define interp (lambda (SEXP) (calc (parse SEXP))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; returns 5
(calc (num 5))

; returns 8
(calc (add (num 3) (num 5)))

; returns 5
(num-n (num 5))

; returns (num 3)
(add-lhs (add (num 3) (num 5)))

; returns (add (sub (sub (num 4) (num 3)) (num 15)) (add (add (sub (num 10) (num 5)) (sub (num 3) (num 2))) (sub (num 15) (num 42))))
(parse '{+ {- {- 4 3} 15} {+ {+ {- 10 5} {- 3 2}} {- 15 42}}})

; returns (with 'x (num 5) (add (id 'x) (id 'x)))
(parse '{with {x 5} {+ x x}})

; returns (with 'x (num 5) (with 'x (add (id 'x) (id 'x)) (add (id 'x) (id 'x))))
(parse '{with {x 5} {with {x {+ x x}} {+ x x}}})

; returns -35
(interp '{+ {- {- 4 3} 15} {+ {+ {- 10 5} {- 3 2}} {- 15 42}}})

; returns 20
(interp '{with {x 5} {with {x {+ x x}} {+ x x}}})

; returns 60
(interp '{with {x 10} {with {y {+ x x}} {with {z {+ y x}} {+ z {+ x y}}}}})
