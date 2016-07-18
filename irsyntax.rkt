#lang racket
(provide (all-defined-out))

; ; プログラムは var-decl と fun-def のリスト
; 変数宣言
(struct var-decl (var)           #:transparent)
; 関数定義
(struct fun-def (var parms body) #:transparent) ; parms は var-decl のリスト
 
; ; 文
; 変数への代入: <var> = <exp>;
(struct assign-stmt (var exp)       #:transparent)
; メモリへの書き込み: *<dest> = <src>;
(struct write-stmt (dest src)       #:transparent)
; メモリ参照: <dest> = *<src>;
(struct read-stmt (dest src)        #:transparent)
; ラベル: <name>:
(struct label-stmt (name)           #:transparent)
; 条件分岐: if(<var>){ goto <tlabel>; } else { goto <elabel>; }
(struct if-stmt (var tlabel elabel) #:transparent)
; 無条件分岐: goto <label>;
(struct goto-stmt (label)           #:transparent)
; 関数呼出し: <dest> = <tgt>(<var1>, <var2>, <var3>, ...);
(struct call-stmt (dest tgt vars)   #:transparent) ; vars は var のリスト
; リターン: return <var>;
(struct ret-stmt (var)              #:transparent)
; 値の出力: print(<var>);
(struct print-stmt (var)            #:transparent)
; 複文: {<decls> <stmts>}
(struct cmpd-stmt (decls stmts)     #:transparent) ; decls は var-decl のリスト,stmts は文のリスト
 
;; int用の式
; 変数参照: <var>
(struct vari-exp (var)           #:transparent)
; 整数即値: <val>
(struct liti-exp (val)           #:transparent)
; 算術演算: <left> <op> <right>
(struct aopi-exp (op left right) #:transparent)
; 比較演算: <left> <op> <right>
(struct ropi-exp (op left right) #:transparent)
; アドレス取得: &<var>
(struct addri-exp (var)          #:transparent)
; intへのキャスト: (int) <src>
(struct casti-exp (src)          #:transparent)

;; float用の式
; 変数参照: <var>
(struct varf-exp (var)           #:transparent)
; 整数即値: <val>
(struct litf-exp (val)           #:transparent)
; 算術演算: <left> <op> <right>
(struct aopf-exp (op left right) #:transparent)
; 比較演算: <left> <op> <right>
(struct ropf-exp (op left right) #:transparent)
; アドレス取得: &<var>
(struct addrf-exp (var)          #:transparent)
; floatへのキャスト: (float) <src>
(struct castf-exp (src)          #:transparent)