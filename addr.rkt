#lang racket
(require (prefix-in ir: "irsyntax.rkt")
         "parser.rkt"
         "sem.rkt"
         "ir.rkt")
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
  ;; 記号表deltaにあるアドレスとdeltaのペアを返す
  ;; 登録されてない場合は新しく登録する。
  (define (addrof delta var)
    (if (delta var)
        (cons (delta var) delta)
        (let ([new-loc (fresh-loc)])
          (cons new-loc
                (lambda (x)
                  (if (equal? x var)
                      new-loc
                      (delta x)))))))
  
  ;; delta と プログラム直下の中間命令のリスト を受け取り、
  ;; 割り当てを行った中間命令列を返す
  (define (program->addr-ir delta stmt-list)
    (if (null? stmt-list)
         ;; stmt-listが空なら空リストを返す
        (cons '() delta)
        (let* ([stmt (car stmt-list)]
               [ret (stmt-list->addr-ir delta stmt)]
               [new-stmt (car ret)]
               [new-delta (cdr ret)]
               [rest-ret (program->addr-ir new-delta
                                           (cdr stmt-list))]
               [last-delta (cdr rest-ret)])
          (cond ((ir:var-decl? new-stmt))
                ((ir:fun-def? new-stmt))
                (else
                 (cons (cons new-stmt (car rest-ret)) last-delta))))))
  ;; 中間命令文のリストを受け取り、
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
    (cond ((ir:fun-def? stmt)
           )
          ((ir:assign-stmt? stmt)
           (ir:assign-stmt? stmt)
             (let* ([x (ir:assign-stmt-var stmt)]
                    [exp (ir:assign-stmt-exp stmt)]
                    [ret1 (addrof delta x)]
                    [n1 (car ret1)]
                    [delta1 (cdr ret1)]
                    [ret2 (exp->addr-ir delta1 exp)]
                    [exp1 (car ret2)]
                    [delta2 (cdr ret2)])
               (cons (ir:assign-stmt (ofs n1) exp1)
                     delta2)))
          
          ((ir:write-stmt? stmt)
           (let* ([x1 (ir:write-stmt-dest stmt)]
                  [x2 (ir:write-stmt-src stmt)]
                  [ret1 (addrof delta x1)]
                  [n1 (car ret1)]
                  [delta1 (cdr ret1)]
                  [ret2 (addrof delta1 x2)]
                  [n2 (car ret2)]
                  [delta2 (cdr ret2)])
             (cons (ir:write-stmt (ofs n1) (ofs n2))
                   delta2)))
          
          ((ir:read-stmt? stmt)
           (let* ((x1 (ir:read-stmt-dest stmt))
                  (x2 (ir:read-stmt-src stmt))
                  (ret1 (addrof delta x1))
                  (n1 (car ret1))
                  (delta1 (cdr ret1))
                  (ret2 (addrof delta1 x2))
                  (n2 (car ret2))
                  (delta2 (cdr ret2)))
             (cons (ir:read-stmt (ofs n1) (ofs n2))
                   delta2)))
          
          ((ir:label-stmt? stmt)
           (cons stmt delta))
          
          ((ir:if-stmt? stmt)
           (let* ((x (ir:if-stmt-var stmt))
                  (l1 (ir:if-stmt-tlabel stmt))
                  (l2 (ir:if-stmt-elabel stmt))
                  (ret (addrof delta x))
                  (n (car ret))
                   (delta1 (cdr ret)))
             (cons (ir:if-stmt (ofs n) l1 l2)
                   delta1)))
          
          ((ir:goto-stmt? stmt)
           (cons stmt delta))
          
          ((ir:call-stmt? stmt)
           (let* ([ret1 (stmt-list->addr-ir delta (ir:call-stmt-vars stmt))]
                  [newvars (car ret1)]
                  [delta1 (cdr ret1)]
                  [x (ir:call-stmt-dest stmt)]
                  [ret2 (addrof delta1 x)]
                  [x1 (car ret2)]
                  [delta2 (cdr ret2)])
             (cons (ir:call-stmt x1 (ir:call-stmt-tgt stmt) newvars)
                   delta2)))
          
          ((ir:ret-stmt? stmt)
           (let* ([x (ir:ret-stmt-var stmt)]
                  [ret (addrof delta x)]
                  [n (car ret)]
                  [delta1 (cdr ret)])
           (cons (ir:ret-stmt (ofs n))
                 delta1)))

          ((ir:print-stmt? stmt)
           (let* ([x (ir:print-stmt-var stmt)]
                  [ret (addrof delta x)]
                  [n (car ret)]
                  [delta1 (cdr ret)])
             (cons (ir:print-stmt (ofs n))
                   delta1)))
          
          ((ir:cmpd-stmt? stmt)
           (stmt-list->addr-ir delta (ir:cmpd-stmt-stmts stmt)))))
  
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
      ((ir:varf-exp? exp)
       (let* ([x (ir:varf-exp-var exp)]
              [ret (addrof delta x)]
              [n (car ret)]
              [delta1 (cdr ret)])
         (cons (ofs n) delta1)))
      ; 整数即値: <val>
      ((ir:liti-exp? exp)
       (cons exp delta))
      ((ir:litf-exp? exp)
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
      ((ir:aopf-exp? exp)
       (let* ([x1 (ir:aopf-exp-left exp)]
              [x2 (ir:aopf-exp-right exp)]
              [ret1 (addrof delta x1)]
              [n1 (car ret1)]
              [delta1 (cdr ret1)]
              [ret2 (addrof delta1 x2)]
              [n2 (car ret2)]
              [delta2 (cdr ret2)])
         (cons (ir:aopf-exp (ir:aopf-exp-op exp)
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
      ((ir:ropf-exp? exp)
       (let* ([x1 (ir:ropf-exp-left exp)]
              [x2 (ir:ropf-exp-right exp)]
              [ret1 (addrof delta x1)]
              [n1 (car ret1)]
              [delta1 (cdr ret1)]
              [ret2 (addrof delta1 x2)]
              [n2 (car ret2)]
              [delta2 (cdr ret2)])
         (cons (ir:ropf-exp (ir:ropf-exp-op exp)
                            (ofs n1)
                            (ofs n2))
               delta2)))
      ; アドレス取得: &<var>
      ((ir:addri-exp? exp)
       (let* ([x (ir:addri-exp-var exp)]
              [ret (addrof delta x)]
              [n (car ret)]
              [delta1 (cdr ret)])
         (cons (ir:addri-exp (fp n))
               delta1)))
      ((ir:addrf-exp? exp)
       (let* ([x (ir:addrf-exp-var exp)]
              [ret (addrof delta x)]
              [n (car ret)]
              [delta1 (cdr ret)])
         (cons (ir:addrf-exp (fp n))
               delta1)))
      ; floatへのキャスト: (float) <src-i>
      ((ir:casti-exp exp)
       (let* ([x (ir:casti-exp-src exp)]
              [ret (addrof delta x)]
              [n (car ret)]
              [delta1 (cdr ret)])
         (cons (ir:casti-exp (ofs n))
               delta1)))
      ((ir:castf-exp exp)
       (let* ([x (ir:castf-exp-src exp)]
              [ret (addrof delta x)]
              [n (car ret)]
              [delta1 (cdr ret)])
         (cons (ir:castf-exp (ofs n))
               delta1)))
      (else 'unknown)))
  
  (car (stmt-list->addr-ir initial-delta ir)))
