#lang racket
(require (prefix-in stx: "syntax.rkt")
         (prefix-in ir: "irsyntax.rkt")
         (prefix-in sem: "sem.rkt")
         "parser.rkt"
         "sem.rkt"
         parser-tools/lex)
(provide (all-defined-out))

;; 新規変数
(define s_maxid 0)
(define (fresh-symbol)
  (let ([oldid s_maxid])
    (set! s_maxid (+ s_maxid 1))
    (string->symbol
     (string-append "_x" (number->string oldid)))))

;; 新規ラベル
(define l_maxid 0)
(define (fresh-label)
  (let ([oldid l_maxid])
    (set! l_maxid (+ l_maxid 1))
    (string->symbol
     (string-append "_l" (number->string oldid)))))

;;log-exp用変換関数
(define (log->ir var log)
  (let ([op (stx:log-exp-op log)]
        [left (stx:log-exp-left log)]
        [right (stx:log-exp-right log)])
    (cond ((equal? op '||)
           (let ([e1 (sem:decl (fresh-symbol) 0 'var 'tempi)] ;; ||の両辺はint型のみ
                 [e2 (sem:decl (fresh-symbol) 0 'var 'tempi)]
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
               ,(ir:if-stmt e2 l3 l4)
               ,(ir:label-stmt l3)
               ,@(ir-exp var 1)
               ,(ir:goto-stmt l5)
               ,(ir:label-stmt l4)
               ,@(ir-exp var 0)
               ,(ir:label-stmt l5))))
          ((equal? op '&&)
           (let ([e1 (sem:decl (fresh-symbol) 0 'var 'tempi)] ;; &&の両辺はint型のみ
                 [e2 (sem:decl (fresh-symbol) 0 'var 'tempi)]
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
               ,(ir:if-stmt e2 l3 l4)
               ,(ir:label-stmt l3)
               ,@(ir-exp var 0)
               ,(ir:goto-stmt l5)
               ,(ir:label-stmt l4)
               ,@(ir-exp var 1)
               ,(ir:label-stmt l5)))))))




;; 式を中間構文に変換する関数
(define (ir-exp var exp)
  (cond
    ;; コンマ演算子
    ((stx:comma-exp? exp)
     (let* ([left (stx:comma-exp-left exp)]
            [ltype (sem:type-inspection left)]
            [t (fresh-symbol)])
       (cond
         ;; int型
         ((equal? ltype 'int)
          `(,@(ir-exp (sem:decl t 0 'var 'tempi) left)
            ,@(ir-exp var (stx:comma-exp-right exp))))
         ;; float型
         ((equal? ltype 'float)
          `(,@(ir-exp (sem:decl t 0 'var 'tempf) left)
            ,@(ir-exp var (stx:comma-exp-right exp))))
         (else 'unknown))))
    ;; 代入
    ((stx:assign-exp? exp)
     (if (stx:deref-exp? (stx:assign-exp-var exp))
         (let ([src (stx:assign-exp-src exp)]
               [t1 (fresh-symbol)]
               [t2 (fresh-symbol)])
           (cond
             ;; int型
             ((equal? (type-inspection src) 'int)
              `(,@(ir-exp (sem:decl t1 0 'var 'tempi) (stx:deref-exp-arg src))
                ,@(ir-exp (sem:decl t2 0 'var 'tempi) (stx:assign-exp-src exp))
                ,(ir:write-stmt (sem:decl t1 0 'var 'tempi)
                                (sem:decl t2 0 'var 'tempi))))
             ;; float型
             ((equal? (type-inspection src) 'float)
              `(,@(ir-exp (sem:decl t1 0 'var 'tempi)
                          (stx:deref-exp-arg src))
                ,@(ir-exp (sem:decl t2 0 'var 'tempf)
                          (stx:assign-exp-src exp))
                ,(ir:write-stmt (sem:decl t1 0 'var 'tempf)
                                (sem:decl t2 0 'var 'tempf))))
             (else 'unknown)))
         
         `(,@(ir-exp (stx:assign-exp-var exp)
                     (stx:assign-exp-src exp))
           ,(ir:assign-stmt var
                            (stx:assign-exp-var exp)))))
    ;; 論理演算
    ((stx:log-exp? exp)
     (log->ir var exp))
    
    ;; 比較演算
    ((stx:rop-exp? exp)
     (let ([left (stx:rop-exp-left exp)]
           [right (stx:rop-exp-right exp)]
           [t1 (fresh-symbol)]
           [t2 (fresh-symbol)])
       (cond ((equal? (sem:type-inspection left) 'int)
              `(,@(ir-exp (sem:decl t1 0 'var 'tempi) (stx:rop-exp-left exp))
                ,@(ir-exp (sem:decl t2 0 'var 'tempi) (stx:rop-exp-right exp))
                ,(ir:assign-stmt
                  var
                  (ir:ropi-exp (stx:rop-exp-op exp)
                               (sem:decl t1 0 'var 'tempi)
                               (sem:decl t2 0 'var 'tempi)))))
             ((equal? (sem:type-inspection left) 'float)
              `(,@(ir-exp (sem:decl t1 0 'var 'tempf) (stx:rop-exp-left exp))
                ,@(ir-exp (sem:decl t2 0 'var 'tempf) (stx:rop-exp-right exp))
                ,(ir:assign-stmt
                  var
                  (ir:ropi-exp (stx:rop-exp-op exp)
                               (sem:decl t1 0 'var 'tempf)
                               (sem:decl t2 0 'var 'tempf)))))
             (else 'unknown))))
    
    ;; 二項演算
    ((stx:aop-exp? exp)
     (let ([op (stx:aop-exp-op exp)]
           [ltype (sem:type-inspection (stx:aop-exp-left exp))]
           [rtype (sem:type-inspection (stx:aop-exp-right exp))]
           [t1 (fresh-symbol)]
           [t2 (fresh-symbol)])
       (cond
         ((not (equal? ltype 'float))
          (if
           ;;ポインタの演算かどうか
           (and (or (equal? op '+) (equal? op '-))
                (not (equal? ltype rtype)))
           ;; ポインタと整数の和と差
           (cond
             ;; 左側にintの場合
             ((equal? ltype 'int)
              `(,@(ir-exp (sem:decl t1 0 'var 'tempi)
                          (stx:aop-exp '* 4 (stx:aop-exp-left exp) (void)))
                ,@(ir-exp (sem:decl t2 0 'var 'tempi)
                          (stx:aop-exp-right exp))
                ,(ir:assign-stmt
                  var
                  (ir:aopi-exp op
                               (sem:decl t1 0 'var 'tempi)
                               (sem:decl t2 0 'var 'tempi)))))
             ;; 右側にintの場合
             ((equal? rtype 'int)
              `(,@(ir-exp (sem:decl t1 0 'var 'tempi)
                          (stx:aop-exp-left exp))
                ,@(ir-exp (sem:decl t2 0 'var 'tempi)
                          (stx:aop-exp '* 4 (stx:aop-exp-right exp) (void)))
                ,(ir:assign-stmt
                  var
                  (ir:aopi-exp op
                               (sem:decl t1 0 'var 'tempi)
                               (sem:decl t2 0 'var 'tempi)))))
             (else (eprintf "unexpected error"))) ;;エラーハンドリング必要
           ;; ポインタ演算でなければint同士であるからそのまま変換
           `(,@(ir-exp (sem:decl t1 0 'var 'tempi) (stx:aop-exp-left exp))
             ,@(ir-exp (sem:decl t2 0 'var 'tempi) (stx:aop-exp-right exp))
             ,(ir:assign-stmt
               var
               (ir:aopi-exp op
                            (sem:decl t1 0 'var 'tempi)
                            (sem:decl t2 0 'var 'tempi))))))
         ;; float同士の場合
         ((equal? ltype 'float)
          `(,@(ir-exp (sem:decl t1 0 'var 'tempf) (stx:aop-exp-left exp))
             ,@(ir-exp (sem:decl t2 0 'var 'tempf) (stx:aop-exp-right exp))
             ,(ir:assign-stmt
               var
               (ir:aopi-exp op
                            (sem:decl t1 0 'var 'tempf)
                            (sem:decl t2 0 'var 'tempf))))))))
    ;; 間接参照式
    ((stx:deref-exp? exp)
     (let ([t (fresh-symbol)])
       `(,@(ir-exp (sem:decl t 0 'var 'tempi) (stx:deref-exp-arg exp))
         ,(ir:read-stmt var (sem:decl t 0 'var 'tempi)))))
    ;; アドレス取得
    ((stx:addr-exp? exp)
     (let ([t (fresh-symbol)])
       (if (not (equal? 'float (sem:type-inspection exp)))
           ;; int型
           `(,@(ir-exp (sem:decl t 0 'var 'tempi)
                       (stx:addr-exp-var exp))
             ,(ir:assign-stmt var (ir:addri-exp
                                   (sem:decl t 0 'var 'tempi))))
           ;; float型
           `(,@(ir-exp (sem:decl t 0 'var 'tempf)
                       (stx:addr-exp-var exp))
             ,(ir:assign-stmt var (ir:addri-exp
                                   (sem:decl t 0 'var 'tempf)))))))
    ;; 関数呼び出し
    ((stx:call-exp? exp)
     (let ([newvars (map (lambda (x) (fresh-symbol))
                         (stx:call-exp-args exp))])
           `(,@(ir-exp newvars (stx:call-exp-args exp))
             ,(ir:call-stmt var (stx:call-exp-tgt exp) newvars))))
    ;; 変数
    ((stx:var-exp? exp)
     (if (not (equal? 'float (sem:type-inspection exp)))
         ;; int型
         `(,(ir:assign-stmt var
                            (ir:vari-exp (stx:var-exp-var exp))))
         ;; float型
         `(,(ir:assign-stmt var
                            (ir:varf-exp (stx:var-exp-var exp))))))
    ;; キャスト
    ((stx:cast-exp? exp)
     (let* ([t (fresh-symbol)]
            [src (stx:cast-exp-src exp)]
            [type (stx:cast-exp-type exp)]
            [pos (stx:cast-exp-pos exp)]
            [line (position-line pos)]
            [col (position-col pos)])
       ;; int --> float か float --> int しかありえない
       (if (equal? 'float (sem:type-inspection src))
           ;; float型 --> int型
           `(,@(ir-exp (sem:decl t 0 'var 'tempf) src)
              ,(ir:castf-exp (sem:decl t 0 'var 'tempf))
              ,(ir:assign-stmt
                var
                (ir:vari-exp (sem:decl t 0 'var 'tempf))))
           ;; int型 --> float型 
           `(,@(ir-exp (sem:decl t 0 'var 'tempi) src)
             ,(ir:casti-exp (sem:decl t 0 'var 'tempi))
             ,(ir:assign-stmt
               var
               (ir:vari-exp (sem:decl t 0 'var 'tempi)))))))
    ;; param,args
    ((list? exp)
     (if (or (null? var) (null? exp))
         '()
         (if (equal? (type-inspection (caar exp)) 'float)
             `(,@(ir-exp (sem:decl (car var) 0 'var 'tempf) (caar exp))
               ,@(ir-exp (cdr var) (cdr exp)))
             `(,@(ir-exp (sem:decl (car var) 0 'var 'tempi) (caar exp))
               ,@(ir-exp (cdr var) (cdr exp))))))
    ;; 数値
    (else
     (if (exact-integer? exp)
         `(,(ir:assign-stmt var
                            (ir:liti-exp exp)))
         `(,(ir:assign-stmt var
                            (ir:litf-exp exp)))))))

;; 文を中間構文に変換する関数
(define (ir-stmt sem decls)
  (cond
    ;; プログラム
    ((stx:program? sem)
     `(,@(ir-stmt (stx:program-declrs sem) decls)))
    ;; グローバル変数
    ((stx:declar? sem) `(,(ir:var-decl (stx:declar-dec sem))))
    ;; 関数プロトタイプ宣言
    ((stx:fun-prot? sem) `())
    ;; 関数定義
    ((stx:fun-def? sem)
     (let ([name  (stx:fun-def-name sem)]
           [parms (map (lambda (x) (cadr x))
                       (stx:fun-def-parms sem))]
           [body  (car (stx:fun-def-body sem))]
           [delta (cdr (stx:fun-def-body sem))])
     `(,(ir:fun-def name
                    parms
                    (cons (ir-stmt body decls) delta)))))
    ;; 複文
    ((stx:cmpd-stmt? sem)
     (let ([cmpd-decls (filter stx:declar?
                               (stx:cmpd-stmt-stmts sem))]
           [cmpd-stmts (filter (compose1 not stx:declar?)
                               (stx:cmpd-stmt-stmts sem))])
       (ir:cmpd-stmt
        (append decls (ir-stmt cmpd-decls decls))
        (ir-stmt cmpd-stmts
                 (append decls (ir-stmt cmpd-decls decls))))))
    
    ;; IF文
    ((stx:if-stmt? sem)
     (let ([t (fresh-symbol)]
           [l1 (fresh-label)]
           [l2 (fresh-label)]
           [l3 (fresh-label)])
     `(,@(ir-exp (sem:decl t 0 'var 'tempi)
                 (stx:if-stmt-test sem))
       ,(ir:if-stmt (sem:decl t 0 'var 'tempi) l1 l2)
       ,(ir:label-stmt l1)
       ,(cons (ir-stmt (car (stx:if-stmt-tbody sem)) decls)
              (cdr (stx:if-stmt-tbody sem)))
       ,(ir:goto-stmt l3)
       ,(ir:label-stmt l2)
       ,(cons (ir-stmt (car (stx:if-stmt-ebody sem)) decls)
              (cdr (stx:if-stmt-ebody sem)))
       ,(ir:label-stmt l3))))
    ;; WHILE文
    ((stx:while-stmt? sem)
     (let ([t (fresh-symbol)]
           [l1 (fresh-label)]
           [l2 (fresh-label)]
           [l3 (fresh-label)])
     `(,(ir:label-stmt l1)
       ,@(ir-exp (sem:decl t 0 'var 'tempi)
                 (stx:while-stmt-test sem))
       ,(ir:if-stmt (sem:decl t 0 'var 'tempi) l2 l3)
       ,(ir:label-stmt l2)
       ,(cons (ir-stmt (car (stx:while-stmt-body sem)) decls)
              (cdr (stx:while-stmt-body sem)))
       ,(ir:goto-stmt l1)
       ,(ir:label-stmt l3))))
    ;; RETURN文
    ((stx:return-stmt? sem)
     (let ([exp (stx:return-stmt-exp sem)]
           [t (fresh-symbol)])
       (if (not (equal? 'float (sem:type-inspection exp)))
           `(,@(ir-exp (sem:decl t 0 'var 'tempi) exp)
             ,(ir:ret-stmt (sem:decl t 0 'var 'tempi)))
           `(,@(ir-exp (sem:decl t 0 'var 'tempf) exp)
             ,(ir:ret-stmt (sem:decl t 0 'var 'tempf))))))
    ;; PRINT文
    ((stx:print-stmt? sem)
     (let ([exp (stx:print-stmt-exp sem)]
           [t (fresh-symbol)])
       (if (equal? (sem:type-inspection exp))
           `(,@(ir-exp (sem:decl t 0 'var 'tempf) exp)
             ,(ir:print-stmt (sem:decl t 0 'var 'tempf)))
           `(,@(ir-exp (sem:decl t 0 'var 'tempi) exp)
             ,(ir:print-stmt (sem:decl t 0 'var 'tempi))))))
    ;; 代入文
    ((stx:assign-exp? sem)
     (if (stx:deref-exp? (stx:assign-exp-var sem))
         ;; * <exp> = <exp>
         (let ([src (stx:assign-exp-src sem)]
               [t1 (fresh-symbol)]
               [t2 (fresh-symbol)])
           (if (equal? 'float (sem:type-inspection exp))
               `(,@(ir-exp (sem:decl t1 0 'var 'tempi)
                           (stx:deref-exp-arg (stx:assign-exp-var sem)))
                 ,@(ir-exp (sem:decl t2 0 'var 'tempf)
                           (stx:assign-exp-src sem))
                 ,(ir:write-stmt
                   (sem:decl t1 0 'var 'tempf)
                   (ir:varf-exp (sem:decl t2 0 'var 'tempf))))
               `(,@(ir-exp (sem:decl t1 0 'var 'tempi)
                           (stx:deref-exp-arg (stx:assign-exp-var sem)))
                 ,@(ir-exp (sem:decl t2 0 'var 'tempi)
                           (stx:assign-exp-src sem))
                 ,(ir:write-stmt
                   (sem:decl t1 0 'var 'tempi)
                   (ir:vari-exp (sem:decl t2 0 'var 'tempi))))))
         ;; x = <exp>
         (ir-exp (stx:assign-exp-var sem) (stx:assign-exp-src sem))))
    ;; プログラム用    
    ((list? sem)
     (if (null? sem)
         '()
         `(,@(ir-stmt (car sem) decls)
           ,@(ir-stmt (cdr sem) decls))))
    ;; exp
    (else
     (if (equal? 'int (type-inspection sem))
         (ir-exp (decl (fresh-symbol) 0 'var 'tempi) sem)
         (ir-exp (decl (fresh-symbol) 0 'var 'tempf) sem)))))

;; 中間構文に変換する関数
(define (ir ast) (cons (ir-stmt (car ast) '()) (cdr ast)))

