#lang racket
(require (prefix-in stx: "syntax.rkt")
         (prefix-in ir: "irsyntax.rkt")
         (prefix-in sem: "sem.rkt")
         "parser.rkt"
         "sem.rkt")

(define ... (void))

;; 新規変数
(define s_maxid 0)
(define (fresh-symbol)
  (let ([oldid s_maxid])
    (set! s_maxid (+ s_maxid 1))
    (string-append "_x" (number->string oldid))))

;; 新規ラベル
(define l_maxid 0)
(define (fresh-label)
  (let ([oldid l_maxid])
    (set! l_maxid (+ l_maxid 1))
    (string-append "_l" (number->string oldid))))

;;log-exp用変換関数
(define (log->ir var log)
  (let ([op (stx:log-exp-op log)]
        [left (stx:log-exp-left log)]
        [right (stx:log-exp-right log)])
    (cond ((equal? op '||)
           (let ([e1 (fresh-symbol)]
                 [e2 (fresh-symbol)]
                 [l1 (fresh-label)]
                 [l2 (fresh-label)]
                 [l3 (fresh-label)]
                 [l4 (fresh-label)]
                 [l5 (fresh-label)])
             `(,@(ir-exp e1 left)
               ,@(ir-exp e2 right)
               ,(ir:if-stmt e1 l1 l2)
               ,(ir:label-stmt l1)
               ,@(ir-exp var 1)
               ,(ir:goto-stmt l5)
               ,(ir:label-stmt l2)
               ,(ir:if-stmt right l3 l4)
               ,(ir:label-stmt l3)
               ,@(ir-exp var 1)
               ,(ir:goto-stmt l5)
               ,(ir:label-stmt l4)
               ,@(ir-exp var 0)
               ,(ir:label-stmt l5))))
          ((equal? op '&&)
           (let ([e1 (fresh-symbol)]
                 [e2 (fresh-symbol)]
                 [l1 (fresh-label)]
                 [l2 (fresh-label)]
                 [l3 (fresh-label)]
                 [l4 (fresh-label)]
                 [l5 (fresh-label)])
             `(,@(ir-exp e1 left)
               ,@(ir-exp e2 right)
               ,(ir:if-stmt e1 l1 l2)
               ,(ir:label-stmt l2)
               ,@(ir-exp var 0)
               ,(ir:goto-stmt l5)
               ,(ir:label-stmt l2)
               ,(ir:if-stmt right l3 l4)
               ,(ir:label-stmt l3)
               ,@(ir-exp var 0)
               ,(ir:goto-stmt l5)
               ,(ir:label-stmt l4)
               ,@(ir-exp var 1)
               ,(ir:label-stmt l5)))))))




;; 式を中間構文に変換する関数   ;即値について改良の余地あり
(define (ir-exp var exp)
  (cond
    ;; コンマ演算子 ;完成
    ((stx:comma-exp? exp)
     `(,(ir-exp (stx:comma-exp-left exp)) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;????
       ,@(ir-exp var
                ((stx:comma-exp-right exp)))))
    ;; 代入 ;完成
    ((stx:assign-exp? exp)
     (if (stx:deref-exp? (stx:assign-exp-var exp))
         (let ([t1 (fresh-symbol)]
               [t2 (fresh-symbol)])
         `(,@(ir-exp t1 (stx:deref-exp-arg (stx:assign-exp-var exp)))
           ,@(ir-exp t2 (stx:assign-exp-src exp))
           ,(ir:write-stmt t1 t2)
           ,(ir:read-stmt var t1)))
         `(,@(ir-exp (stx:assign-exp-var exp)
                     (stx:assign-exp-src exp))
           ,(ir:assign-stmt var
                            (stx:assign-exp-var exp)))))
    ;; 論理演算 ;完成
    ((stx:log-exp? exp)
     (log->ir var exp))
    
    ;; 比較演算
    ((stx:rop-exp? exp)
     (let ([t1 (fresh-symbol)]
           [t2 (fresh-symbol)])
     `(,@(ir-exp t1 (stx:rop-exp-left exp))
       ,@(ir-exp t2 (stx:rop-exp-right exp))
       ,(ir:assign-stmt
         (ir:var-exp var)
         (ir:rop-exp (stx:rop-exp-op exp)
                    t1
                    t2)))))
    
    ;; 二項演算 ;未完成
    ((stx:aop-exp? exp)
     (let ([op (stx:aop-exp-op exp)]
           [t1 (fresh-symbol)]
           [t2 (fresh-symbol)])
       (if
        ;;ポインタの演算かどうか
        (and (or (equal? op '+) (equal? op '-))
             (not (equal? (sem:type-inspection (stx:aop-exp-left exp))
                          (sem:type-inspection (stx:aop-exp-right exp)))))
           ;; ポインタと整数の和と差
           (cond
             ;; 左側にintの場合
             ((equal? (sem:type-inspection (stx:aop-exp-left exp)) 'int)
                  `(,@(ir-exp t1 (stx:aop-exp '* 4 (stx:aop-exp-left exp) (void)))
                    ,@(ir-exp t2 (stx:aop-exp-right exp))
                    ,(ir:assign-stmt
                      (ir:var-exp var)
                      (ir:aop-exp op t1 t2))))
             ;; 右側にintの場合
             ((equal? (sem:type-inspection (stx:aop-exp-right exp)) 'int)
              `(,@(ir-exp t1 (stx:aop-exp-left exp))
                ,@(ir-exp t1 (stx:aop-exp '* 4 (stx:aop-exp-right exp) (void)))
                ,(ir:assign-stmt
                  (ir:var-exp var)
                  (ir:aop-exp op t1 t2))))
             ;; 機能しないはずだから消去可能
             (else
              `(,@(ir-exp t1 (stx:aop-exp-left exp))
                ,@(ir-exp t2 (stx:aop-exp-right exp))
                ,(ir:assign-stmt
                  (ir:var-exp var)
                  (ir:aop-exp op t1 t2)))))
           ;; ポインタ演算でなければそのまま変換
           `(,@(ir-exp t1 (stx:aop-exp-left exp))
             ,@(ir-exp t2 (stx:aop-exp-right exp))
             ,(ir:assign-stmt
               (ir:var-exp var)
               (ir:aop-exp op t1 t2))))))
    ((stx:deref-exp? exp)
     (let ([t (fresh-symbol)])
       `(,@(ir-exp t (stx:deref-exp-arg exp))
         ,(ir:read-stmt var t))))
    ((stx:addr-exp? exp)
     (let ([t (fresh-symbol)])
       `(,@(ir-exp t (stx:addr-exp-var exp))
         ,(ir:assign-stmt var (ir:addr-exp t)))))
    ;; 関数呼び出し ;完成
    ((stx:call-exp? exp)
     (let ([newvars (map (lambda (x) (fresh-symbol))
                         (stx:call-exp-args exp))])
       `(,@(ir-exp newvars (stx:call-exp-args exp))
         ,(ir:call-stmt var (stx:call-exp-tgt exp) newvars))))
    ;; 変数 ;完成
    ((stx:var-exp? exp)
     `(,(ir:assign-stmt (ir:var-exp var)
                        (ir:var-exp (stx:var-exp-var exp)))))
    ;; param,args　;完成
    ((list? exp)
     (if (or (null? var) (null? exp))
         '()
         `(,@(ir-exp (car var) (car exp))
           ,@(ir-exp (cdr var) (cdr exp)))))
    ;; 数値
    (else `(,(ir:assign-stmt (ir:var-exp var)
                             (ir:lit-exp exp))))))

(define (ir-stmt sem)
  (cond
    ((stx:program? sem) (map ir-stmt (stx:program-declrs sem)))
    ((stx:declar? sem) (ir:var-decl (stx:declar-dec sem)))
    ((stx:fun-prot? sem) ...)
    ((stx:fun-def? sem)
     (ir:fun-def (stx:fun-def-name sem)
                 (stx:fun-def-parms sem)
                 (ir-stmt (stx:fun-def-body sem))))
    ((stx:cmpd-stmt? sem)
     (map ir-stmt (stx:cmpd-stmt-stmts sem)))

    ;; IF文 ;完成
    ((stx:if-stmt? sem)
     (let ([t (fresh-symbol)]
           [l1 (fresh-label)]
           [l2 (fresh-label)]
           [l3 (fresh-label)])
     `(,@(ir-exp t (stx:if-stmt-test sem))
       ,(ir:if-stmt t l1 l2)
       ,(ir:label-stmt l1)
       ,@(ir-stmt (stx:if-stmt-tbody sem))
       ,(ir:goto-stmt l3)
       ,(ir:label-stmt l2)
       ,@(ir-stmt (stx:if-stmt-ebody sem))
       ,(ir:label-stmt l3))))
    ((stx:while-stmt? sem)
     (let ([t (fresh-symbol)]
           [l1 (fresh-label)]
           [l2 (fresh-label)]
           [l3 (fresh-label)])
     `(,(ir:label-stmt l1)
       ,@(ir-exp t (stx:while-stmt-test sem))
       ,(ir:if-stmt t l2 l3)
       ,(ir:label-stmt l2)
       ,@(ir-stmt (stx:while-stmt-body sem))
       ,(ir:goto-stmt l1)
       ,(ir:label-stmt l3))))
    ((stx:return-stmt? sem)
     (let ([t (fresh-symbol)])
       `(,@(ir-exp t (stx:return-stmt-exp sem))
         ,(ir:ret-stmt t))))
    ((stx:print-stmt? sem)
     (let ([t (fresh-symbol)])
       `(,@(ir-exp t (stx:print-stmt-exp sem))
         ,(ir:print-stmt t))))
    ((stx:assign-exp? sem)
     (if (stx:deref-exp? (stx:assign-exp-var sem))
         ;; * <exp> = <exp>
         (let ([t1 (fresh-symbol)]
               [t2 (fresh-symbol)])
           `(,@(ir-exp t1 (stx:deref-exp-arg (stx:assign-exp-var sem)))
             ,@(ir-exp t2 (stx:assign-exp-src sem))
             ,(ir:write-stmt t1 t2)))
         ;; x = <exp>
         (ir-exp (stx:assign-exp-var sem) (stx:assign-exp-src sem))))
    (else (ir-exp (fresh-symbol) sem))))



