#lang racket
;; (require rackunit)
(provide (all-defined-out))

(struct program (program exdecl pos)            #:transparent)

;; declaration/definition
;; (struct fun-decl  (name ret-ty parm-tys pos)   #:transparent)
;; (struct fun-def   (name ret-ty parms body pos) #:transparent)

(struct declar (type decls)                     #:transparent)

(struct dirc-decl (name num pos)                #:transparent)

(struct parm-decl (name ty pos)                 #:transparent)

;;function-prototype
(struct function-prottype (type name parms)            #:transparent)
;;function-definition
(struct function-definition (type fundec parms body)         #:transparent)
;;function-declarator
(struct function-declarator (name parms)             #:transparent)





;; statement

;; メモリへの書き込み: *<exp> = <exp>;
(struct massign-stmt (dst src pos)              #:transparent)
;; 条件分岐: if(<exp>) <stmt> else <stmt>
(struct if-stmt      (test tbody ebody pos)     #:transparent)
;; 繰り返し: while(<exp>) <stmt>
(struct while-stmt   (test body pos)            #:transparent)
;;繰り返し: for(<exp-opt>;<exp-opt>;<exp-opt>) <stmt>
(struct for-stmt (first second third body pos)  #:transparent)
;; 返り値: return <exp>;
(struct return-stmt (exp pos)                   #:transparent)
;; 値の出力: print(<exp>);
(struct print-stmt   (exp pos)                  #:transparent)


;; 複文

;; 混合文: {<decl-list> <stmt-list-opt>} 
(struct cmpd-stmt (decls stmts pos)             #:transparent)
;; 複文
(struct stmt-lists (lists stmt pos)             #:transparent)
;; コンマ: <elems> , <elem>
(struct comma-stmt (lefts right pos)        #:transparent)


;; expression

;; 変数: <var>
(struct var-exp     (tgt pos)           #:transparent)
;; 偽を表す真偽値: true
(struct true-exp    (pos)               #:transparent)
;; 偽を表す真偽値: false
(struct false-exp   (pos)               #:transparent)
;; 整数即値: <num>
(struct lit-exp     (val pos)           #:transparent)
;; 変数への代入: <var> = <exp>;
(struct assign-exp  (var src pos)       #:transparent)
;; 論理演算: <left-exp> <op> <right-exp>
(struct log-exp     (op left right pos) #:transparent)
;; 算術演算: <left-exp> <op> <right-exp>
(struct aop-exp     (op left right pos) #:transparent)
;; 比較演算: <left-exp> <op> <right-exp>
(struct rop-exp     (op left right pos) #:transparent)
;; 符号反転: -<exp>
(struct neg-exp     (arg pos)           #:transparent)
;; メモリ参照: *<exp>
(struct deref-exp   (arg pos)           #:transparent)
;; アドレスを取る: &<var>
(struct addr-exp    (var pos)           #:transparent)
;; ============: <postfix-expr> [ <exp> ]
(struct postfix-exp (fixed exp pos)     #:transparent)
;; 関数の呼び出し: <identifier> ( <argument-exp-list-opt> )
(struct call-exp    (tgt args pos)      #:transparent)
;; 丸括弧: (<exp>)
(struct par-exp     (exp pos)           #:transparent)
