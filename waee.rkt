#lang plai
;;;;
;; WAEE stuff
;;
(define-type WAEE
    (num (n number?))
    (binop (op symbol?) (lhs WAEE?) (rhs WAEE?))
    (with (lob lob?) (body WAEE?))
    (id (name symbol?)))

; Check whether something is a binding
(define binding? (lambda (x) (and (symbol? (first x)) (WAEE? (second x)))))

; Check whether something is a list of bindings
(define lob? (lambda (x) (cond ((empty? x) #t) (else (and (binding? (car x)) (lob? (cdr x)))))))

(define subst
    (lambda (expr sub-id val)
        (type-case WAEE expr
            (num (x) expr)
            (binop (op l r) (binop op (subst l sub-id val) (subst r sub-id val)))
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
                    ((with) (with (first (second SEXP)) (parse-waee (second (second SEXP))) (parse-waee (third SEXP))))
                    (else (binop (first SEXP) (parse-waee (second SEXP)) (parse-waee (third SEXP))))))
            ((symbol? SEXP) (id SEXP)))))

(define interp-waee
    (lambda (a-waee)
        (type-case WAEE a-waee
            (num (x) x)
            (binop (op l r) ((lookup op ops) (interp-waee l) (interp-waee r)))
            (with (bound-id named-expr bound-body) (interp-waee (subst bound-body bound-id (num (interp-waee named-expr)))))
            (id (v) (error 'interp-waee->id (string-append "Found a free identifier (" (symbol->string v) ")"))))))

(define eval-waee (lambda (SEXP) (interp-waee (parse-waee SEXP))))

;;;;
;; Binop stuff
;;
(define-type Binop
    (bop (name symbol?) (op procedure?)))

(define lookup
    (lambda (op-name op-table)
        (cond ((empty? op-table) (error 'lookup (string-append "Operator not found: " (symbol->string op-name))))
            (else
                (if (symbol=? (bop-name (car op-table)) op-name)
                    (bop-op (car op-table))
                    (lookup op-name (cdr op-table)))))))

(define ops (list
    (bop '+ +)
    (bop '- -)
    (bop '* *)
    (bop '/ /)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;; Tests
;;

; returns (with 'x (num 5) (add (id 'x) (id 'x)))
(parse-waee '{with {{x 5}} {+ x x}})

; returns -35
(parse-waee '{+ {- {- 4 3} 15} {+ {+ {- 10 5} {- 3 2}} {- 15 42}}})

; returns -35
(eval-waee '{+ {- {- 4 3} 15} {+ {+ {- 10 5} {- 3 2}} {- 15 42}}})

; returns 20
(eval-waee '{with {{x 5}} {with {{x {+ x x}}} {+ x x}}})

; returns 60
(eval-waee '{with {{x 10}} {with {{y {+ x x}}} {with {{z {+ y x}}} {+ z {+ x y}}}}})

; returns 50
(eval-waee '{with {{x 10}} {* x 5}})

; returns 10
(eval-waee '{/ 50 5})

; returns an error (free identifier)
;(eval-waee '{with {x 5} {with {x {+ x x}} {+ y x}}})

; test the op-table
(lookup '+ ops)
(lookup '- ops)
(lookup '* ops)
(lookup '/ ops)

; test the binding? check
(binding? (list 'x (num 5)))

; test the lob? check
(lob? (list (list 'x (num 5)) (list 'y (num 7)) (list 'z (num 9))))

(parse-waee '{with {{x 5} {y 7}} {+ x y}})