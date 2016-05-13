#lang racket
;; (require rackunit)
(provide (all-defined-out))

;; declaration/definition

;; 変数宣言文: decls
(struct declar (declrs pos)                          #:transparent)
;; 配列: <identifer> [ <expression> ]
(struct array-exp (type name size pos)               #:transparent)
;; 関数のプロトタイプ宣言: type name ( parms );
(struct fun-prot (type name parms pos)               #:transparent)
;; 関数の定義: type fundec ( parms ) body
(struct fun-def (type name parms body pos)           #:transparent)




;; statement

;; 条件分岐: if(<exp>) <stmt> else <stmt>
(struct if-stmt      (test tbody ebody pos)         #:transparent)
;; 繰り返し: while(<exp>) <stmt>
(struct while-stmt   (test body pos)                #:transparent)
;; 返り値: return <exp>;
(struct return-stmt  (exp pos)                      #:transparent)




;; expression

;; 変数参照
(struct var-exp (var pos)               #:transparent)
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
;; 前置インクリメント・ディクリメント: ++<exp> / --<exp>
(struct front-inct-exp     (op var pos)  #:transparent)
;; 後置インクリメント・ディクリメント: <exp>++ / <exp>--
(struct back-inct-exp (op var pos)       #:transparent)
;; コンマ: <exp>, <exp>
(struct comma-exp (left right pos)       #:transparent)
