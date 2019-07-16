(module addition "pre-base.rkt"
  (#%require (for-syntax "stxcase-scheme.rkt"))

  (define-syntax case-λ
    (syntax-rules ()
      [(_ (param body) ...)
       (case-lambda (param body) ...)]))

  (define-syntax match-λ
    (syntax-rules ()
      [(_ clause ...)
       (match-lambda clause ...)]))

  (define-syntax match-λ*
    (syntax-rules ()
      [(_ clause ...)
       (match-lambda* clause ...)]))

  (define-syntax match-λ**
    (syntax-rules ()
      [(_ clause* ...)
       (match-lambda** clause* ...)]))

  (define-syntax while
    (syntax-rules ()
      [(_ pred body ...)
       (let loop () (when pred body ... (loop)))]))

  (define-syntax defvar
    (syntax-rules ()
      [(_ name value)
       (define name value)]))

  (define-syntax defun
    (syntax-rules ()
      [(_ name param body ...)
       (define name (lambda param body ...))]))

  (define-syntax progn
    (syntax-rules ()
      [(_ body ...)
       (begin body ...)]))

  (define (1+  num) (add1 num))
  (define (1-  num) (sub1 num))
  (define (-1+ num) (sub1 num))

  (define (list-head orig-lst orig-pos)
    (unless (and (fixnum? orig-pos) (>= orig-pos 0))
      (error 'list-head "invalid index ~s" orig-pos))
    (let loop [(lst orig-lst) [pos orig-pos]]
      (cond [(<= pos 1)
             (if (= pos 0)
                 '()
                 (if (pair? lst)
                     (list (car lst))
                     (error 'list-head
                            "~s is not a proper list" orig-lst)))]
            [(pair? lst)
             (let ([a (car lst)] [ls (cdr lst)])
               (if (pair? ls)
                   (list* a (car ls) (loop (cdr ls) (- pos 2)))
                   (error 'list-head
                          "~s is not a proper list" orig-lst)))]
            [else (error 'list-head
                         "~s is not a proper list" orig-lst)])))

  (provide case-λ while
           1+ 1- -1+
           defvar defun progn
           list-head))
