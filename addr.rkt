#lang racket
(require (prefix-in ir: "irsyntax.rkt"))
(provide (all-defined-out))

(struct ofs (loc) #:transparent)
(struct fp (val)  #:transparent)

(define max-loc 0)
(define (reset-max-loc)
  (set! max-loc 0))

(define (assign-addr ir)
  (define initial-delta (lambda (x) #f))
  (define size 4)
  (define (fresh-loc)
    (let ([old-loc max-loc])
      (set! max-loc (+ max-loc size))
      old-loc))
  ;; varにdecl構造体を受け取って、
  ;; 記号表deltaにあるアドレスか#fを返す
  (define (addrof delta var)
    (if (delta var)
        (cons (delta var) delta)
        (let ([new-loc (fresh-loc)])
          (cons new-loc
                (lambda (x)
                  (if (equal? x var)
                      new-loc
                      (delta x)))))))
  ;; delta と 中間命令のリスト stmt-list を受け取り、
  ;; 割り当てを行った中間命令列を返す
  (define (stmt-list->addr-ir delta stmt-list)
    (if (null? stmt-list)
        ;; stmt-listが空なら空リストを返す
        (cons '() delta)
        (let* ([stmt (car stmt-list)]
               [ret (stmt->addr-ir delta stmt)]
               [new-stmt (car ret)]
               [new-delta (cdr ret)]
               [rest-ret (stmt-list->addr-ir new-delta
                                             (cdr stmt-list))]
               [last-delta (cdr rest-ret)])
          (cons (cons new-stmt (car rest-ret)) last-delta))))
  ;; 中間命令文にアドレス割り当てを行う
  (define (stmt->addr-ir delta stmt)
    (cond ((ir:assign-stmt? stmt))
          ((ir:write-stmt? stmt))
          ((ir:read-stmt? stmt))
          ((ir:label-stmt? stmt))
          ((ir:if-stmt? stmt))
          ((ir:goto-stmt? stmt))
          ((ir:call-stmt? stmt))
          ((ir:ret-stmt? stmt))
          ((ir:print-stmt? stmt))
          ((ir:cmpd-stmt? stmt))))
  
  ;; 中間命令の式にアドレス割り当てを行う
  ;; delta , exp -> (exp, delta)
  (define (exp->addr-ir delta exp)
    (cond
      ; 変数参照: <var>
      ((ir:vari-exp? exp)
       (let* ([x (ir:vari-exp-var exp)]
              [ret (addrof delta x)]
              [n (car ret)]
              [delta1 (cdr ret)])
         (cons (ofs n) delta1)))
      ; 整数即値: <val>
      ((ir:liti-exp? exp)
       (cons exp delta))
      ; 算術演算: <left> <op> <right>
      ((ir:aopi-exp? exp)
       (let* ([x1 (ir:aopi-exp-left exp)]
              [x2 (ir:aopi-exp-right exp)]
              [ret1 (addrof delta x1)]
              [n1 (car ret1)]
              [delta1 (cdr ret1)]
              [ret2 (addrof delta1 x2)]
              [n2 (car ret2)]
              [delta2 (cdr ret2)])
         (cons (ir:aopi-exp (ir:aopi-exp-op exp)
                            (ofs n1)
                            (ofs n2))
               delta2)))
      ; 比較演算: <left> <op> <right>
      ((ir:ropi-exp? exp)
       (let* ([x1 (ir:ropi-exp-left exp)]
              [x2 (ir:ropi-exp-right exp)]
              [ret1 (addrof delta x1)]
              [n1 (car ret1)]
              [delta1 (cdr ret1)]
              [ret2 (addrof delta1 x2)]
              [n2 (car ret2)]
              [delta2 (cdr ret2)])
         (cons (ir:ropi-exp (ir:ropi-exp-op exp)
                            (ofs n1)
                            (ofs n2))
               delta2)))
      ; アドレス取得: &<var>
      ((ir:addri-exp? exp)
       (let* ([ret (addrof delta exp)]
              [n (car ret)]
              [delta1 (cdr ret)])
         (cons (ir:addri-exp (fp n))
               delta1)))
      ; intへのキャスト: (int) <src>
      ((ir:casti-exp exp))))
  
  (car (stmt-list->addr-ir initial-delta ir)))
