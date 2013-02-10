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
                    (with bound-id (subst named-expr sub-id val) bound-body)
                    (with bound-id (subst named-expr sub-id val) (subst bound-body sub-id val))))
            (id (v) (if (symbol=? v sub-id) val expr)))))

(define parse-wae
    (lambda (SEXP)
        (cond
            ((number? SEXP) (num SEXP))
            ((list? SEXP)
                (case (first SEXP)
                    ((+) (add (parse-wae (second SEXP)) (parse-wae (third SEXP))))
                    ((-) (sub (parse-wae (second SEXP)) (parse-wae (third SEXP))))
                    ((with) (with (first (second SEXP)) (parse-wae (second (second SEXP))) (parse-wae (third SEXP))))))
            ((symbol? SEXP) (id SEXP)))))

(define interp-wae
    (lambda (a-wae)
        (type-case WAE a-wae
            (num (x) x)
            (add (l r) (+ (interp-wae l) (interp-wae r)))
            (sub (l r) (- (interp-wae l) (interp-wae r)))
            (with (bound-id named-expr bound-body) (interp-wae (subst bound-body bound-id (num (interp-wae named-expr)))))
            (id (v) (error 'interp-wae->id (string-append "Found a free identifier (" (symbol->string v) ")"))))))

(define eval-wae (lambda (SEXP) (interp-wae (parse-wae SEXP))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; returns 5
(interp-wae (num 5))

; returns 8
(interp-wae (add (num 3) (num 5)))

; returns 5
(num-n (num 5))

; returns (num 3)
(add-lhs (add (num 3) (num 5)))

; returns (with 'x (num 5) (add (id 'x) (id 'x)))
(parse-wae '{with {x 5} {+ x x}})

; returns -35
(eval-wae '{+ {- {- 4 3} 15} {+ {+ {- 10 5} {- 3 2}} {- 15 42}}})

; returns 20
(eval-wae '{with {x 5} {with {x {+ x x}} {+ x x}}})

; returns 60
(eval-wae '{with {x 10} {with {y {+ x x}} {with {z {+ y x}} {+ z {+ x y}}}}})

; returns an error (free identifier)
(eval-wae '{with {x 5} {with {x {+ x x}} {+ y x}}})