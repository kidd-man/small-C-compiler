#lang racket
;; (require rackunit)
(provide (all-defined-out))

(struct program (program exdecl pos)            #:transparent)

;; declaration/definition

;; 変数宣言文: type decls;
(struct declar (type decls)                            #:transparent)
;; 関数のプロトタイプ宣言: type name ( parms );
(struct function-prottype (type name parms)            #:transparent)
;; 関数の定義 type fundec(parms)
(struct function-definition (type fundec parms body)   #:transparent)





;; statement

;; 条件分岐: if(<exp>) <stmt> else <stmt>
(struct if-stmt      (test tbody ebody pos)     #:transparent)
;; 繰り返し: while(<exp>) <stmt>
(struct while-stmt   (test body pos)            #:transparent)
;;繰り返し: for(<exp-opt>;<exp-opt>;<exp-opt>) <stmt>
(struct for-stmt (first second third body pos)  #:transparent)
;; 返り値: return <exp>;
(struct return-stmt (exp pos)                   #:transparent)




;; expression

;; 変数への代入: <var> = <exp>;
(struct assign-exp  (var src pos)       #:transparent)
;; 論理演算: <left-exp> <op> <right-exp>
(struct log-exp     (op left right pos) #:transparent)
;; 算術演算: <left-exp> <op> <right-exp>
(struct aop-exp     (op left right pos) #:transparent)
;; 比較演算: <left-exp> <op> <right-exp>
(struct rop-exp     (op left right pos) #:transparent)
;; メモリ参照: *<exp>
(struct deref-exp   (arg pos)           #:transparent)
;; アドレスを取る: &<var>
(struct addr-exp    (var pos)           #:transparent)
;; 関数の呼び出し: <identifier> ( <argument-exp-list-opt> )
(struct call-exp    (tgt args pos)      #:transparent)