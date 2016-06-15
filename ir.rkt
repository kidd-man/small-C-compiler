#lang racket
(require (prefix-in stx: "syntax.rkt")
         (prefix-in ir: "irsyntax.rkt")
         (prefix-in sem: "sem.rkt"))

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
             `(,@(ir-var e1 left)
               ,@(ir-var e2 right)
               ,(ir:if-stmt e1 l1 l2)
               ,(ir:label-stmt l1)
               ,@(ir-var var 1)
               ,(ir:goto-stmt l5)
               ,(ir:label-stmt l2)
               ,(ir:if-stmt right l3 l4)
               ,(ir:label-stmt l3)
               ,@(ir-var var 1)
               ,(ir:goto-stmt l5)
               ,(ir:label-stmt l4)
               ,@(ir-var var 0)
               ,(ir:label-stmt l5))))
          ((equal? op '&&)
           (let ([e1 (fresh-symbol)]
                 [e2 (fresh-symbol)]
                 [l1 (fresh-label)]
                 [l2 (fresh-label)]
                 [l3 (fresh-label)]
                 [l4 (fresh-label)]
                 [l5 (fresh-label)])
             `(,@(ir-var e1 left)
               ,@(ir-var e2 right)
               ,(ir:if-stmt e1 l1 l2)
               ,(ir:label-stmt l2)
               ,@(ir-var var 0)
               ,(ir:goto-stmt l5)
               ,(ir:label-stmt l2)
               ,(ir:if-stmt right l3 l4)
               ,(ir:label-stmt l3)
               ,@(ir-var var 0)
               ,(ir:goto-stmt l5)
               ,(ir:label-stmt l4)
               ,@(ir-var var 1)
               ,(ir:label-stmt l5)))))))

;; A_x   ;即値について改良の余地あり
(define (ir-var var exp)
  (cond
    ;; コンマ演算子 ;完成
    ((stx:comma-exp? exp)
     `(,(sem->ir (stx:comma-exp-left exp))
       ,@(ir-var var
                ((stx:comma-exp-right exp)))))
    ;; 代入 ;完成
    ((stx:assign-exp? exp)
     `(,@(ir-var (stx:assign-exp-var exp)
                 (stx:assign-exp-src exp))
       ,(ir:assign-stmt var
                        (stx:assign-exp-var exp))))
    ;; 論理演算 ;完成
    ((stx:log-exp? exp)
     (log->ir var exp))
    
    ;; 比較演算
    ((stx:rop-exp? exp)
     (let ([t1 (fresh-symbol)]
           [t2 (fresh-symbol)])
     `(,@(ir-var t1 (stx:rop-exp-left exp))
       ,@(ir-var t2 (stx:rop-exp-right exp))
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
             (not (equal? (sem:decl-type (stx:aop-exp-left exp))
                          (sem:decl-type (stx:aop-exp-right exp)))))
           ;; ポインタと整数の和と差
           (cond
             ;; 左側にintの場合
             ((equal? (sem:decl-type (stx:aop-exp-left exp)) 'int)
                  `(,@(ir-var t1 (stx:aop-exp '* 4 (stx:aop-exp-left exp)))
                    ,@(ir-var t2 (stx:aop-exp-right exp))
                    ,(ir:assign-stmt
                      (ir:var-exp var)
                      (ir:aop-exp op t1 t2))))
             ;; 右側にintの場合
             ((equal? (sem:decl-type (stx:aop-exp-right exp)) 'int)
              `(,@(ir-var t1 (stx:aop-exp-left exp))
                ,@(ir-var t1 (stx:aop-exp '* 4 (stx:aop-exp-right exp)))
                ,(ir:assign-stmt
                  (ir:var-exp var)
                  (ir:aop-exp op t1 t2))))
             ;; 機能しないはずだから消去可能
             (else
              `(,@(ir-var t1 (stx:aop-exp-left exp))
                ,@(ir-var t2 (stx:aop-exp-right exp))
                ,(ir:assign-stmt
                  (ir:var-exp var)
                  (ir:aop-exp op t1 t2)))))
           ;; ポインタ演算でなければそのまま変換
           `(,@(ir-var t1 (stx:aop-exp-left exp))
             ,@(ir-var t2 (stx:aop-exp-right exp))
             ,(ir:assign-stmt
               (ir:var-exp var)
               (ir:aop-exp op t1 t2))))))
    ((stx:deref-exp? exp)　...)
    ((stx:addr-exp? exp) ...)
    ((stx:call-exp? exp) ...)
    ((stx:var-exp? exp)
     `(,(ir:assign-stmt (ir:var-exp var)
                        (ir:var-exp (stx:var-exp-var exp)))))
    ;; 数値
    (else `(,(ir:assign-stmt (ir:var-exp var)
                             (ir:lit-exp exp))))))

(define (sem->ir sem)
  (cond
    ((stx:program? sem) (map sem->ir (stx:program-declrs sem)))
    ((stx:declar? sem) (ir:var-decl (stx:declar-dec sem)))
    ((stx:fun-prot? sem) ...)
    ((stx:fun-def? sem) ...)
    ((stx:cmpd-stmt? sem) ...)

    ;; IF文 ;完成
    ((stx:if-stmt? sem)
     (let ([t (fresh-symbol)]
           [l1 (fresh-label)]
           [l2 (fresh-label)]
           [l3 (fresh-label)])
     `(,(ir:assign-stmt t (sem->ir (stx:if-stmt-test sem)))
       ,(ir:if-stmt t l1 l2)
       ,(ir:label-stmt l1)
       ,(sem->ir (stx:if-stmt-tbody sem))
       ,(ir:goto-stmt l3)
       ,(ir:label-stmt l2)
       ,(sem->ir (stx:if-stmt-ebody sem))
       ,(ir:label-stmt l3))))
    ((stx:while-stmt? sem)
     (let ([t (fresh-symbol)]
           [l1 (fresh-label)]
           [l2 (fresh-label)]
           [l3 (fresh-label)])
     `(,(ir:label-stmt l1)
       ,(ir:assign-stmt t (sem->ir (stx:while-stmt-test sem)))
       ,(ir:if-stmt t l2 l3)
       ,(ir:label-stmt l2)
       ,(sem->ir (stx:while-stmt-body sem))
       ,(ir:goto-stmt l1)
       ,(ir:label-stmt l3))))
    ((stx:return-stmt? sem))
    ((stx:print-stmt? sem))
    ((stx:assign-exp? sem)
     (ir:assign-stmt (sem->ir (stx:assign-exp-var sem))
                     (sem->ir (stx:assign-exp-src sem))))
    ((stx:log-exp? sem))
    ((stx:rop-exp? sem)
     (ir:rop-exp (stx:rop-exp-op sem)
                 (sem->ir (stx:rop-exp-left sem))
                 (sem->ir (stx:rop-exp-right sem))))
    ((stx:aop-exp? sem))
    ((stx:deref-exp? sem))
    ((stx:addr-exp? sem)
     (ir:addr-exp (sem->ir (stx:addr-exp-var sem))))
    ((stx:comma-exp? sem))
    ((stx:var-exp? sem))
    ((stx:call-exp? sem))))