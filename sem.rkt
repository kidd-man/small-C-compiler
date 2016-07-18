#lang racket
(provide (all-defined-out))
(require "parser.rkt"
         (prefix-in stx: "syntax.rkt")
         (prefix-in sms: "semsyntax.rkt")
         parser-tools/lex)

;; 環境変数の定義:
; nameを引数に取り、登録されたdataを返す。
; 未登録なら#fを返す。
; extend-deltaでname付きdataを新しく加えたものを返す。
(define initial-delta (lambda (x) #f))
(define (extend-delta delta x data)
  (lambda (y) (if (equal? x y) data (delta y))))

;; 各種構造体
(struct decl    (name lev kind type) #:transparent)
(struct pointer (type)               #:transparent)
(struct array   (type size)          #:transparent)
(struct fun     (type params)        #:transparent)
; 一時的に生成する構造体
(struct p-and-b (para body)          #:transparent)
(struct returns (type-set)           #:transparent)

;; エラーの種類(構造体)
(define-struct (name-resolve-error exn:fail:user) ())
(define-struct (type-inspect-error exn:fail:user) ())
(define-struct (expression-form-error exn:fail:user) ())
;; エラーの種類(文字列)
(define ename "name resolve error")
(define etype "type inspection error")
(define eform "expression form error")

;; 型のエラー表示用関数
(define (disp-p type)
  (define (make-string now-type last-is-*? acc)
    (cond ((pointer? now-type)
           (make-string
            (pointer-type now-type)
            #t
            (string-append "*" acc)))
          ((array? now-type)
           (let ([size (array-size now-type)])
             (if last-is-*?
                 (make-string
                  (array-type now-type)
                  #f
                  (format "(~a)[~a]" acc size))
                 (make-string
                  (array-type now-type)
                  #f
                  (format "~a[~a]" acc size)))))
          (else (if (equal? acc "")
                    now-type
                    (format "~a ~a" now-type acc)))))
  (make-string type #f ""))

;********************************************************************

;;構文解析した抽象構文木->さらに意味解析をした抽象構文木
;;呼び出すごとに新たな環境を生成する
(define (parse->sem ast env lev)
  ;---------------------------------------------------------------
  ;; 局所関数
  
  ;; スターを構造体に書き換える関数
  ;; (* ... * . type) -> (pointer ... (pointer type))
  (define (make-pointer-type pairs)
    (if (pair? pairs)
        (pointer (make-pointer-type (cdr pairs)))
        pairs))
  
  ;; 配列を構造体に変換する関数
  ;; 配列に紛れたポインタも処理する
  (define (make-type-struct par)
    ;; ポインタを構造体にする関数
    ;; (list * ... * obj) -> (pointer ... (pointer obj))
    (define (make-pointer list)
      (if (equal? (length list) 1)
          (make-type-struct (car list))
          (pointer (make-pointer (cdr list)))))
    ;; id や (* ... * id) ならば#tを返す関数
    (define (id? hoge)
      (cond ((stx:array-exp? hoge) #f)
            ((list? hoge)
             (if (id? (last hoge)) #t #f))
            (else #t)))
    ;; 構造体に置き換える
    (cond ((stx:array-exp? par)
           (let ([name (stx:array-exp-name par)]
                 [type (stx:array-exp-type par)]
                 [size (stx:array-exp-size par)])
           (array
            (make-type-struct
             (if (id? name)
                 (if (list? name)
                     (append (cdr (reverse name))
                             `(,type))
                     type)
                 name))
            size)))
          ((list? par)
           (make-pointer par))
          (else par)))
  
  ;; 多次元配列の名前を発掘する関数
  (define (find-array-name array)
    (cond ((stx:array-exp? array)
           (find-array-name (stx:array-exp-name array)))
          ((list? array)
           (find-array-name (last array)))
          (else array)))
  ;---------------------------------------------------------------
  ;; 解析・木の分解・再構築
  (define (make-sem ast)
    (cond
      ;; 変数宣言
      ((stx:declar? ast)
       (stx:declar
        (let* ([dec (stx:declar-dec ast)]
               [namepos
                (if (equal? (car dec) 'undef)
                    (cdr (find-array-name (cdr dec))) ;;配列
                    (cddr dec))]                      ;;配列でない
               [name
                (if (equal? (car dec) 'undef)
                    (car (find-array-name (cdr dec))) ;;配列
                    (cadr dec))]                      ;;配列でない
               [type
                (if (equal? (car dec) 'undef) 
                    (make-type-struct (cdr dec))      ;;配列
                    (make-pointer-type (car dec)))]   ;;配列でない
               [obj (env name)]
               [line (number->string (position-line namepos))]
               [col (number->string (position-col namepos))]
               [msg "~a:~a: ~a: redeclaration of '~a'"])
          (if obj
              (cond
                ;; 既にその名前が関数として定義されている場合
                ((or (equal? (decl-kind obj) 'fun)
                     (equal? (decl-kind obj) 'proto))
                 (if (equal? lev 0)
                     ;; 例外を投げる
                     (raise (name-resolve-error
                             (string-append
                              (format msg line col ename name)
                              " as different kind of symbol: function")
                             (current-continuation-marks)))
                     ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                     (let ([new-obj (decl name lev 'var type)])
                       (begin (set! env (extend-delta env name new-obj))
                              new-obj))))
                ;; すでにその名前が変数として定義されている場合
                ((equal? (decl-kind obj) 'var)
                 (if (equal? lev (decl-lev obj))
                     ;; 例外を投げる
                     (raise (name-resolve-error
                             (format msg line col ename name)
                             (current-continuation-marks)))
                     ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                     (let ([new-obj (decl name lev 'var type)])
                       (begin (set! env (extend-delta env name new-obj))
                              new-obj))))
                ;; 既にその名前がパラメータとして宣言されている場合
                ((equal? (decl-kind obj) 'parm)
                 (let ([new-obj (decl name lev 'var type)])
                   (begin
                     ;;警告
                     (eprintf
                      (string-append
                       (format msg line col ename name)
                       " as diferent kind of symbol: parameter\n"))
                     ;;環境に新しいオブジェクトを追加して
                     (set! env (extend-delta env name new-obj))
                     new-obj)))) ;;nameをnew-objに置き換える
              (let ([new-obj (decl name lev 'var type)])
                ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                (begin (set! env (extend-delta env name new-obj))
                       new-obj))))
        (stx:declar-pos ast)))
      
      ;; 関数プロトタイプ宣言
      ((stx:fun-prot? ast)
       (let* ([name (stx:fun-prot-name ast)]
              [parms (stx:fun-prot-parms ast)]
              [type (fun (make-pointer-type (stx:fun-prot-type ast))
                         (map (lambda (x) (make-pointer-type (caar x)))
                              (stx:fun-prot-parms ast)))]
              [obj (env name)]
              [tpos (stx:fun-prot-tpos ast)]
              [npos (stx:fun-prot-npos ast)]
              [nline (position-line npos)]
              [ncol (position-col npos)])
         (stx:fun-prot
          (make-pointer-type (stx:fun-prot-type ast))
          (if obj
              ;; エラー処理
              (cond
                ;; 既にその名前が関数としてプロトタイプ宣言されている場合
                ((equal? (decl-kind obj) 'proto)
                 ;; 型が一致しているか確認
                 (let ([old-type         ;登録されている関数の型
                        (fun-type (decl-type obj))]
                       [old-paratypelist ;登録されている関数のパラメータの型の列
                        (fun-params (decl-type obj))]
                       [new-type         ;新しく宣言した関数の型
                        (make-pointer-type (stx:fun-prot-type ast))] 
                       [new-paratypelist ;新しく宣言した関数のパラメータの型の列
                        (map (lambda (x) (make-pointer-type (caar x)))
                             (stx:fun-prot-parms ast))]) 
                   ;; 型が一致するかどうか
                   (if (and (equal? old-type new-type)
                            (equal? old-paratypelist new-paratypelist))
                       ;; 型が一致した場合は環境になにもせずnameをnew-objで置き換える
                       (let ([new-obj (decl name lev 'proto type)])
                         new-obj)
                       ;; 型が一致しない場合はエラー
                       (let ([msg "~a:~a: ~a: conflicting types for '~a'"])
                         (raise (name-resolve-error
                                 (format msg nline ncol ename name)
                                 (current-continuation-marks)))))))
                ;; 既にその名前が関数として定義されている場合
                ((equal? (decl-kind obj) 'fun)
                 ;; 型が一致しているか確認
                 (let ([old-type         ;登録されている関の返り値数の型
                        (fun-type (decl-type obj))]
                       [old-paratypelist ;登録されている関数のパラメータの型の列
                        (fun-params (decl-type obj))]
                       [new-type         ;新しく宣言した関数の返り値の型
                        (stx:fun-prot-type ast)]
                       [new-paratypelist ;新しく宣言した関数のパラメータの型の列
                        (map caar (stx:fun-prot-parms ast))])
                   ;; 型が一致するかどうか
                   (if (and (equal? old-type new-type)
                            (equal? old-paratypelist new-paratypelist))
                       ;; 型が一致した場合は環境に何もせずnameをnew-objで置き換える
                       (let ([new-obj (decl name lev 'proto type)])
                         new-obj)
                       ;; 型が一致しない場合はエラー
                       (let ([msg "~a:~a: ~a: conflicting types for '~a'"])
                         (raise (name-resolve-error
                                 (format msg nline ncol ename name)
                                 (current-continuation-marks)))))))
                ;; すでにその名前が変数として定義されている場合
                ((equal? (decl-kind obj) 'var)
                 (if (equal? lev (decl-lev obj))
                     ;; 大域変数(レベルが等しい)ならば二重宣言なのでエラー
                     (let ([msg (string-append
                                 "~a:~a: ~a: redifinition of "
                                 "'~a' as different kind of symbol")])
                       (raise (name-resolve-error
                               (format msg nline ncol ename name)
                               (current-continuation-marks))))
                     ;; レベルが違う場合はオブジェクトを作成し環境に書き込み
                     ;; nameをnew-objで置き換える
                     (let ([new-obj (decl name lev 'proto type)])
                       (begin (set! env (extend-delta env name new-obj))
                              new-obj)))))
              ;; 未宣言の場合はオブジェクトを作成し環境に書き込みnameをnew-objで置き換える
              (let ([new-obj (decl name lev 'proto type)])
                (begin (set! env (extend-delta env name new-obj))
                       new-obj)))
          ;; パラメータ
          (p-and-b-para (parse->sem (p-and-b parms (void)) env (+ lev 1)))
          tpos
          npos)))
      
      ;; 関数定義
      ((stx:fun-def? ast)
       (let* ([name (stx:fun-def-name ast)]
              [parms (stx:fun-def-parms ast)]
              [type (fun (make-pointer-type (stx:fun-def-type ast))
                         (map (lambda (x) (make-pointer-type (caar x)))
                              (stx:fun-def-parms ast)))]
              [body (stx:fun-def-body ast)]
              [obj (env name)]
              [tpos (stx:fun-def-tpos ast)]
              [npos (stx:fun-def-npos ast)]
              [nline (position-line npos)]
              [ncol (position-col npos)])
         (stx:fun-def
          (make-pointer-type (stx:fun-def-type ast))
          (if obj
              ;; 既に名前が環境に登録されている場合
              (cond
                ;; 既にその名前が関数としてプロトタイプ宣言されている場合
                ((equal? (decl-kind obj) 'proto)
                 (let ([old-type         ;登録されている関数返り値の型
                        (fun-type (decl-type obj))]
                       [old-paratypelist ;登録されている関数のパラメータの型の列
                        (fun-params (decl-type obj))]
                       [new-type         ;新しく宣言した関数返り値の型
                        (make-pointer-type (stx:fun-def-type ast))]
                       [new-paratypelist ;新しく宣言した関数のパラメータの型の列
                        (map (lambda (x) (make-pointer-type (caar x)))
                             (stx:fun-def-parms ast))])
                   ;; 型が一致するかどうか
                   (if (and (equal? old-type new-type)
                            (equal? old-paratypelist new-paratypelist))
                       ;; 型が一致した場合は環境書き込みnameをnew-objで置き換える
                       (let ([new-obj (decl name lev 'fun type)])
                         (begin (set! env (extend-delta env name new-obj))
                                new-obj))
                       ;; 型が一致しない場合はエラー
                       (let ([msg "~a:~a: ~a: conflicting types for '~a'"])
                         (raise (name-resolve-error
                                 (format msg nline ncol ename name)
                                 (current-continuation-marks)))))))
                ;; 既にその名前が関数として定義されている場合
                ((equal? (decl-kind obj) 'fun)
                 ;; 二重宣言なので無条件でエラー
                 (let ([msg "~a:~a: ~a: redifinition of '~a'"])
                   (raise (name-resolve-error
                           (format msg nline ncol ename name)
                           (current-continuation-marks)))))
                ;; 変数として宣言されている場合
                ((equal? (decl-kind obj) 'var)
                 ;; 登録された変数のレベルをチェック
                 (if (equal? (decl-lev obj) lev)
                     ;; 大域変数(レベルが等しい)ならば二重宣言なのでエラー
                     (let ([msg (string-append
                                 "~a:~a: ~a: redifinition "
                                 "of '~a' as different kind of symbol")])
                       (raise (name-resolve-error
                               (format msg nline ncol ename name)
                               (current-continuation-marks))))
                     ;; レベルが異なれば環境にnew-objを追加しnameをnew-objで置き換える
                     (let ([new-obj (decl name lev 'fun type)])
                       (begin (set! env (extend-delta env name new-obj))
                              new-obj)))))
              ;; 環境になければ環境にnew-objを追加しnameをnew-objで置き換える
              (let ([new-obj (decl name lev 'fun type)])
                (begin (set! env (extend-delta env name new-obj))
                       new-obj)))
          ;; パラメータ
          (p-and-b-para (parse->sem (p-and-b parms body) env (+ lev 1)))
          ;;body Lvはparameter-Lv+1
          (p-and-b-body (parse->sem (p-and-b parms body) env (+ lev 1)))
          tpos
          npos)))
      
      ;; パラメータと関数本体を一緒に見るための分岐
      ((p-and-b? ast)
       (p-and-b
        (map
         (lambda (x)
           (let* ([type (make-pointer-type (caar x))]
                  [name (cadr x)]
                  [tpos (cdar x)]
                  [npos (cddr x)]
                  [obj (env name)]
                  [msg "~a:~a: ~a: redefinition of parameter '~a'"]
                  [line (position-line npos)]
                  [col (position-col npos)])
             (if obj
                 (if (equal? 'parm (decl-kind obj))
                     ;; paraなら二重宣言なのでエラーを投げる
                     (raise (name-resolve-error
                             (format msg line col ename name)
                             (current-continuation-marks)))
                     ;; paraでなければ二重宣言ではないので環境に書き込み
                     ;; nameをnew-objに置き換える
                     (let ([new-obj (decl name lev 'parm type)])
                       (begin (set! env (extend-delta env name new-obj))
                              (cons (cons type tpos) (cons new-obj npos)))))
                 ;; 環境にない場合は環境に書き込みnameをnew-objに置き換える
                 (let ([new-obj (decl name lev 'parm type)])
                   (begin (set! env (extend-delta env name new-obj))
                          (cons (cons type tpos) (cons new-obj npos)))))))
         (p-and-b-para ast))
        ;; body。voidはプロトタイプ宣言用
        (if (void? (p-and-b-body ast)) (void)
            (parse->sem (p-and-b-body ast) env (+ lev 1)))))
      
      ;; if文: そのまま
      ((stx:if-stmt? ast)
       (let ([test (stx:if-stmt-test ast)]
             [tbody (stx:if-stmt-tbody ast)]
             [ebody (stx:if-stmt-ebody ast)]
             [pos (stx:if-stmt-pos ast)])
         (stx:if-stmt (make-sem test)
                      (parse->sem tbody env lev)
                      (parse->sem ebody env lev)
                      pos)))
      
      ;; while: そのまま
      ((stx:while-stmt? ast)
       (let ([test (stx:while-stmt-test ast)]
             [body (stx:while-stmt-body ast)]
             [pos (stx:while-stmt-pos ast)])
         (stx:while-stmt (make-sem test)
                         (parse->sem body env lev)
                         pos)))
      
      ;; return文: そのまま
      ((stx:return-stmt? ast)
       (let ([exp (stx:return-stmt-exp ast)]
             [pos (stx:return-stmt-pos ast)])
         (stx:return-stmt (make-sem exp)
                          pos)))
      
      ;; 代入
      ((stx:assign-exp? ast)
       (let* ([var (stx:assign-exp-var ast)]
              [src (stx:assign-exp-src ast)]
              [vpos (stx:assign-exp-vpos ast)]
              [eqpos (stx:assign-exp-eqpos ast)]
              [line (position-line vpos)]
              [col (position-col vpos)]
              [msg "~a:~a: ~a: expression is not assignable"])
         (if (or (stx:deref-exp? var)
                 (stx:var-exp? var))
             ;; *<exp> または <var>のときはok (arrayの除外は型検査で)
             (stx:assign-exp (make-sem var)
                             (make-sem src)
                             vpos
                             eqpos)
             ;; それ以外はエラー
             (raise (name-resolve-error
                     (format msg line col ename)
                     (current-continuation-marks))))))
      
      ;; 論理演算: そのまま
      ((stx:log-exp? ast)
       (let ([op (stx:log-exp-op ast)]
             [left (stx:log-exp-left ast)]
             [right (stx:log-exp-right ast)]
             [pos (stx:log-exp-pos ast)])
         (stx:log-exp op
                      (make-sem left)
                      (make-sem right)
                      pos)))
      
      ;; 比較演算: そのまま
      ((stx:rop-exp? ast)
       (let ([op (stx:rop-exp-op ast)]
             [left (stx:rop-exp-left ast)]
             [right (stx:rop-exp-right ast)]
             [pos (stx:rop-exp-pos ast)])
         (stx:rop-exp op
                      (make-sem left)
                      (make-sem right)
                      pos)))
      
      ;; 四則演算: そのまま
      ((stx:aop-exp? ast)
       (let ([op (stx:aop-exp-op ast)]
             [left (stx:aop-exp-left ast)]
             [right (stx:aop-exp-right ast)]
             [pos (stx:aop-exp-pos ast)])
         (stx:aop-exp op
                      (make-sem left)
                      (make-sem right)
                      pos)))
      
      ;; 関節参照: *
      ((stx:deref-exp? ast)
       (let ([arg (stx:deref-exp-arg ast)]
             [pos (stx:deref-exp-pos ast)])
         (stx:deref-exp (make-sem arg)
                        pos)))
      
      ;; アドレス取得: &
      ((stx:addr-exp? ast)
       (let* ([var (stx:addr-exp-var ast)]
              [pos (stx:addr-exp-pos ast)]
              [line (position-line pos)]
              [col (position-col pos)]
              [type (type-inspection (make-sem var))]
              [msg (string-append
                    "~a:~a: ~a: cannot take the "
                    "address of an value of type '~a'")])
         ;; 変数のみ
         (if (stx:var-exp? var)
             (stx:addr-exp (make-sem var)
                           pos)
             (raise (name-resolve-error
                     (format msg line col ename (disp-p type))
                     (current-continuation-marks))))))
      
      ;; コンマ演算: 何もしない
      ((stx:comma-exp? ast)
       (let ([left (stx:comma-exp-left ast)]
             [right (stx:comma-exp-right ast)]
             [pos (stx:comma-exp-pos ast)])
         (stx:comma-exp (make-sem left)
                        (make-sem right)
                        pos)))
      
      ;; 変数
      ((stx:var-exp? ast)
       (let* ([name (stx:var-exp-var ast)]
              [pos (stx:var-exp-pos ast)]
              [line (position-line pos)]
              [col (position-col pos)]
              [obj (env name)])
         (stx:var-exp
          (if obj
              ;; 環境に登録されている場合
              (cond
                ;; 変数または引数ならばobjを参照して置き換える
                ((or (equal? (decl-kind obj) 'var)
                     (equal? (decl-kind obj) 'parm))
                 obj)
                ;; 関数ならば関数を変数として参照しているのでエラー
                ((or (equal? (decl-kind obj) 'fun)
                     (equal? (decl-kind obj) 'proto))
                 (let ([msg (string-append
                             "~a:~a: ~a: cannot refer "
                             "function name '~a' as a varb expression")])
                   (raise (name-resolve-error
                           (format msg line col ename name)
                           (current-continuation-marks))))))
              ;; 環境に登録されていない場合未宣言の変数を参照しているのでエラー
              (let ([msg (string-append
                          "~a:~a: ~a: implicit declaration"
                          " of varb '~a' is invalid")])
                (raise (name-resolve-error
                        (format msg line col ename name)
                        (current-continuation-marks)))))
          pos)))
      
      ;; 関数呼び出し
      ((stx:call-exp? ast)
       (let* ([name (stx:call-exp-tgt ast)]
              [args (stx:call-exp-args ast)]
              [npos (stx:call-exp-npos ast)]
              [ppos (stx:call-exp-ppos ast)]
              [line (position-line npos)]
              [col (position-col npos)]
              [obj (env name)])
         (stx:call-exp           
          (if obj
              ;; 環境に登録されている場合
              (cond
                ;; 変数または引数ならば変数を関数として参照しているのでエラー
                ((or (equal? (decl-kind obj) 'var)
                     (equal? (decl-kind obj) 'parm))
                 (let ([fulmsg (format
                                (string-append
                                 "~a:~a: ~a: called object '~a' is"
                                 " not a function or function pointer")
                                line col ename name)])
                   (raise
                    (name-resolve-error fulmsg
                                        (current-continuation-marks)))))
                ;; 関数ならばobjを参照して置き換える
                ((or (equal? (decl-kind obj) 'fun)
                     (equal? (decl-kind obj) 'proto))
                 obj))
              ;; 環境に登録されていない場合未宣言の関数を参照しているのでエラー
              (let ([fulmsg (format
                             (string-append "~a:~a: ~a: implicit declaration"
                                            " of function '~a' is invalid")
                             line col ename name)])
                (raise
                 (name-resolve-error fulmsg
                                     (current-continuation-marks)))))
          (map (lambda (x) (cons (make-sem (car x)) (cdr x))) args)
          npos
          ppos)))
      
      ;; キャスト
      ((stx:cast-exp? ast)
       (let ([src (stx:cast-exp-src ast)]
             [type (stx:cast-exp-type ast)]
             [pos (stx:cast-exp-pos ast)])
         ;; キャストを付ける必要が無いなら省略(シンタックスシュガー)
         (if (equal? type (type-inspection (make-sem src)))
             (make-sem src)
             (stx:cast-exp type (make-sem src) pos))))
      
      ;; 出力
      ((stx:print-stmt? ast)
       (let ([src (stx:print-stmt-exp ast)])
         (stx:print-stmt (make-sem src))))
      
      ;; 例外・そのまま
      (else ast)))
  ;---------------------------------------------------------------
  ;; 再帰呼び出しすると環境を新たに作らないといけないので
  ;; 環境の参照先が変わらないための形式的本体
  ;; 複文ごとに新たに環境を作る感じ
  (cond ((stx:program? ast)
         (cons (stx:program (map make-sem (stx:program-declrs ast))) env))
        ((stx:cmpd-stmt? ast)
         (cons (stx:cmpd-stmt (map make-sem (stx:cmpd-stmt-stmts ast))) env))
;        ((list? ast)
         ;(map make-sem ast))
        (else (make-sem ast))))

;********************************************************************

;; 型検査　
(define (type-inspection ast)
  ;---------------------------------------------------------------
  ;; 局所関数たち
  
  ;; array を pointer に変換してしまう関数
  (define make-pointer
    (lambda (x) (cond ((array? x)
                       (pointer (make-pointer (array-type x))))
                      ((pointer? x)
                       (pointer (make-pointer (pointer-type x))))
                      (else x))))
  ;; void または
  ;; (list* * ... * 'intか'float) または
  ;; (pointer ... (pointer 'intか'float)) または
  ;; (array ... (array 'intか'float))
  ;; -> #t
  (define (typed? x)
    (lambda (x)
      (define roop
        (if (boolean? x)
            x
            (cond ((pair? x)
                   (roop (cdr x)))
                  ((pointer? x)
                   (roop (pointer-type x)))
                  ((array? x)
                   (roop (array-type x)))
                  (else (if (or (equal? x 'int)
                                (equal? x 'float))
                            #t #f)))))
      (if (equal? x 'void) #f roop)))
  ;---------------------------------------------------------------
  ;; ここからtype-inspection本体
  (cond
    ;;プログラム
    ((stx:program? ast)
     (andmap type-inspection (stx:program-declrs ast)))
    
    ;; 複文
    ((stx:cmpd-stmt? ast)
     (let* ([return-in '()])
       ;; 各文からreturnを集める
       ;; ここでreturn文以外も評価して検査する
       (for-each
        (lambda (x)
          (if (returns? x)
              (set! return-in
                    (reverse (set-union
                              (reverse return-in) ;; for readable
                              (reverse (returns-type-set x)))))
              (void)))
        (map type-inspection (stx:cmpd-stmt-stmts ast)))
       ;; returnの集合のreturn構造体を返す
       (returns return-in)))
    
    ;; 変数宣言
    ((stx:declar? ast)
     (let* ([type (make-pointer (decl-type (stx:declar-dec ast)))]
            [pos (stx:declar-pos ast)]
            [line (position-line pos)]
            [col (position-col pos)]
            [msg "~a:~a: ~a: illigal type '~a' to verb expression"])
       ;; void関連の除外
       (if (typed? type)
           #t
           ;; エラー
           (raise (type-inspect-error
                   (format msg line col etype type))))))
    
    ;; 関数プロトタイプ
    ((stx:fun-prot? ast)
     (let* ([ftype (make-pointer (stx:fun-prot-type ast))]
            [params (map (lambda (x)
                           (cons (cons (make-pointer (caar x)) (cdar x))
                                 (cdr x)))
                         (stx:fun-prot-parms ast))]
            [fpos (stx:fun-prot-tpos ast)]
            [fline (position-line fpos)]
            [fcol (position-col fpos)]
            [fmsg (string-append "~a:~a: ~a: illigal type '~a' to "
                                 "return type of function prototype")])
       (begin
         ;; 返り値の型検査
         (if (and
              ;; void関連の除外
              (andmap typed? params)
              ;; 関数の返り値はvoidも可
              (or (typed? ftype) (equal? ftype 'void)))
             #t
             ;; エラーを投げる
             (raise (type-inspect-error
                     (format fmsg fline fcol etype ftype)
                     (current-continuation-marks))))
         ;; パラメータの型検査
         (for-each
          (lambda (x)
            (let* ([ptype (caar x)]
                   [ppos (cdar x)]
                   [pline (position-line ppos)]
                   [pcol (position-col ppos)]
                   [pmsg "~a:~a: ~a: illigal type '~a' to parameter"])
              (if (and (not (equal? ptype 'void))
                       (typed? ptype))
                  #t
                  ;; エラー
                  (raise (type-inspect-error
                          (format pmsg pline pcol etype ptype)
                          (current-continuation-marks))))))
          params))))
    
    ;; 関数定義
    ((stx:fun-def? ast)
     (let* ([ftype (make-pointer (stx:fun-def-type ast))]
            [params (map (lambda (x)
                           (cons (cons (make-pointer (caar x)) (cdar x))
                                 (cdr x)))
                         (stx:fun-def-parms ast))]
            [return-set (type-inspection (car (stx:fun-def-body ast)))]
            [fpos (stx:fun-def-tpos ast)]
            [fline (position-line fpos)]
            [fcol (position-col fpos)]
            [fmsg (string-append "~a:~a: ~a: illigal type '~a'"
                                 " to return type of function definition")])
       ;; エラーがなければ#tを返す
       (begin
         ;; 返り値の型検査
         (if (and
              ;; void関連の除外
              (andmap typed? params)
              ;; 関数の返り値はvoidも可
              (or (typed? ftype) (equal? ftype 'void)))
             #t
             ;; エラー
             (raise (type-inspect-error
                     (format fmsg fline fcol etype ftype)
                     (current-continuation-marks))))
         ;; 各パラメータから void 関連の検査
         (for-each
          (lambda (x)
            (let* ([ptype (caar x)]
                   [ppos (cdar x)]
                   [pline (position-line ppos)]
                   [pcol (position-col ppos)]
                   [pmsg "~a:~a: ~a: illigal type '~a' in parameter"])
              (if (and (not (equal? ptype 'void))
                       (typed? ptype))
                  #t
                  ;; エラー
                  (raise (type-inspect-error
                          (format pmsg pline pcol etype ptype)
                          (current-continuation-marks))))))
          params)
         
         ;; body の return の型があってるかどうかのチェックをしてから
         ;; #tを返す。この時他のbodyも評価する。
         (for-each
          (lambda (x)
            (let* ([rtype (if (null? (car x))
                              'void (car x))] ;returnの型
                   [rpos (cdr x)]  ;returnの位置
                   [rline (position-line rpos)]
                   [rcol (position-col rpos)]
                   [rmsg (string-append
                          "~a:~a: ~a: missmatch return statement"
                          " type for function ('~a' for '~a')")])
              (if (equal? ftype 'void)
                  (if (null? rtype) #t
                      ;;エラー
                      (raise (type-inspect-error
                              (format rmsg rline rcol etype rtype ftype)
                              (current-continuation-marks))))
                  (if (equal? ftype rtype) #t
                      ;; エラー
                      (raise (type-inspect-error
                              (format rmsg rline rcol etype rtype ftype)
                              (current-continuation-marks)))))))
          (returns-type-set return-set))　#t)))
    
    ;; if: testと２つのbodyを見る
    ((stx:if-stmt? ast)
     (let* ([test (type-inspection (stx:if-stmt-test ast))]
            [tbody (type-inspection (car (stx:if-stmt-tbody ast)))]
            [ebody (type-inspection (car (stx:if-stmt-ebody ast)))]
            [pos (stx:if-stmt-pos ast)]
            [line (position-line pos)]
            [col (position-col pos)]
            [msg (string-append
                  "~a:~a: ~a: illigal type '~a' in "
                  "IF test expression\nexpected: 'int'")])
       ;; testがintなら
       (if (equal? test 'int)
           ;; returnをかき集めてreturn集合を返す。
           (returns (set-union (returns-type-set tbody)
                               (returns-type-set ebody)))
           ;; それ以外はエラー
           (raise (type-inspect-error
                   (format msg line col etype (disp-p test))
                   (current-continuation-marks))))))
    
    ;; while: testとbodyを見る
    ((stx:while-stmt? ast)
     (let* ([test (type-inspection (stx:while-stmt-test ast))]
            [body (type-inspection (car (stx:while-stmt-body ast)))]
            [pos (stx:while-stmt-pos ast)]
            [line (position-line pos)]
            [col (position-col pos)]
            [msg (string-append
                  "~a:~a: ~a: illigal type '~a' in WHILE"
                  " test expression\nexpected: 'int'")])
       (if (equal? test 'int)
           body
           (raise (type-inspect-error
                   (format msg line col etype (disp-p test))
                   (current-continuation-marks))))))
    
    ;; リターン: returnを集める集合で返す（cdr部はpos）
    ((stx:return-stmt? ast)
     (let ([exp (type-inspection (stx:return-stmt-exp ast))]
           [pos (stx:return-stmt-pos ast)])
       (returns `(,(cons exp pos)))))
    
    ;; 出力処理: 常に正しい
    ((stx:print-stmt? ast) #t)
    
    ;; 変数参照式: decl-typeの型がつく。
    ((stx:var-exp? ast)
     (let ([type (decl-type (stx:var-exp-var ast))])
       type))
    
    ;; コンマ: 右を取るのみ
    ((stx:comma-exp? ast)
     (let ([right (type-inspection (stx:comma-exp-right ast))])
       right))
    
    ;; 代入:
    ((stx:assign-exp? ast)
     (let* ([var (stx:assign-exp-var ast)]
            [src (stx:assign-exp-src ast)]
            [pos (stx:assign-exp-eqpos ast)]
            [line (position-line pos)]
            [col (position-col pos)]
            [msg (string-append
                  "~a:~a: ~a: assignment of distinct"
                  " pointer types ('~a' into '~a')")])
       ;; derefの中身を探す関数
       (define (find-deref-arg drf)
         (if (stx:deref-exp? drf)
             (find-deref-arg (stx:deref-exp-arg drf))
             drf))
       ;; 処理本体
       (if (stx:var-exp? var)
           ;; var-expの場合
           (cond
             ;; varがvar-expであり、arrayでなく、srcと型が一致する場合       
             ((and
               (not (array? (decl-type (stx:var-exp-var var))))
               (equal? (decl-type (stx:var-exp-var var))
                       (type-inspection src)))
              (type-inspection var))
             ;; varがvar-expであり、
             ;; srcがarrayで、arrayをpointerに
             ;; 変えたものとvarの型が一致する場合
             ((and
               (stx:var-exp? var)
               (array? (type-inspection src))
               (equal? (decl-type (stx:var-exp-var var))
                       (pointer
                        (array-type
                         (decl-type (stx:var-exp-var src))))))
              (type-inspection var))
             ;; それ以外はエラー
             (else (raise
                    (expression-form-error
                     (format msg line col etype
                             (disp-p (type-inspection src))
                             (disp-p (type-inspection var)))
                     (current-continuation-marks)))))
           ;; deref-expの場合
           (cond
             ;; 普通に型が一致する場合
             ((and (equal? (type-inspection var)
                           (type-inspection src)))
              (type-inspection var))
             ;; varがvar-expであり、
             ;; srcがvar-expであり、arrayで、arrayをpointerに
             ;; 変えたものと左辺の型が一致する場合
             ((and (stx:var-exp? (find-deref-arg var))
                   (array? (type-inspection src))
                   (equal? (type-inspection var)
                           (pointer (array-type (type-inspection src)))))
              (type-inspection var))
             (else (raise (expression-form-error
                           (format msg line col etype
                                   (disp-p (type-inspection src))
                                   (disp-p (type-inspection var)))
                           (current-continuation-marks))))))))
    
    ;; 論理演算: &&,||
    ((stx:log-exp? ast)
     (let* ([op (stx:log-exp-op ast)]
            [left (type-inspection (stx:log-exp-left ast))]
            [right (type-inspection (stx:log-exp-right ast))]
            [pos (stx:log-exp-pos ast)]
            [line (position-line pos)]
            [col (position-col pos)]
            [msg (string-append
                  "~a:~a: ~a: comparison of illigal"
                  " types ('~a' and '~a')\n"
                  "expected: 'int' and 'int'")])
       
       (cond
         ;; &&,||:leftとrightがともにint型なら、int型がつく
         ((and (equal? left 'int) (equal? right 'int))
          'int)
         ;; そうでなければエラー
         (else
          (raise (type-inspect-error
                  (format msg line col etype
                          (disp-p left)
                          (disp-p right))
                  (current-continuation-marks)))))))
    ;; 比較演算: >,<,>=,<=
    ((stx:rop-exp? ast)
     (let* ([op (stx:rop-exp-op ast)]
            [left (type-inspection (stx:rop-exp-left ast))]
            [right (type-inspection (stx:rop-exp-right ast))]
            [pos (stx:rop-exp-pos ast)]
            [line (position-line pos)]
            [col (position-col pos)]
            [msg (string-append
                  "~a:~a: ~a: comparison of distinct"
                  " types ('~a' and '~a')")])
       ;; 両辺が同じで int か float なら、int型がつく
       (if (and (equal? left right)
                (or (equal? left 'int)
                    (equal? left 'float)))
           'int
           ;;そうでなければエラー
           (raise (type-inspect-error
                   (format msg line col etype
                           (disp-p left)
                           (disp-p right))
                   (current-continuation-marks))))))
    ;; 四則演算
    ((stx:aop-exp? ast)
     (let* ([op (stx:aop-exp-op ast)]
            [left (type-inspection (stx:aop-exp-left ast))]
            [right (type-inspection (stx:aop-exp-right ast))]
            [pos (stx:aop-exp-pos ast)]
            [line (position-line pos)]
            [col (position-col pos)]
            [msg (string-append
                  "~a:~a: ~a: invalid operands to"
                  " binary expression ('~a' and '~a')")])
       (cond
         ;; +の場合
         ((equal? op '+)
          (cond
            ;; int + *..*(int | float)
            ((and (equal? 'int left)
                  (pointer? (make-pointer right)))
             ;; 右辺を返す
             right)
            ;; *..*(int | float) + int
            ((and (equal? 'int right)
                  (pointer? (make-pointer left)))
             ;; 左辺を返す
             left)
            ;; int + int または float + float
            ((and (equal? left right)
                  (or (equal? left 'int)
                      (equal? left 'float)))
             ;; 左辺を返す(右でも可)
             left)
            ;; それ以外はエラー
            (else (raise (type-inspect-error
                          (format msg line col etype
                                  (disp-p left)
                                  (disp-p right))
                          (current-continuation-marks))))))
         ;; -の場合
         ((equal? op '-)
          ;; <typed> - int | int - int | float - float
          (if (or (and (typed? left) (equal? right 'int))
                  (and (equal? left right)
                       (equal? right 'float)))
              ;; 左辺を返す
              left
              ;; それ以外はエラー
              (raise (type-inspect-error
                      (format msg line col etype
                              (disp-p left)
                              (disp-p right))
                      (current-continuation-marks)))))
         ;; *と/の場合
         (else
          ;; 両辺一致すれば
          (if (and (equal? left right)
                   (or (equal? left 'int)
                       (equal? left 'float)))
              ;; 左辺を返す
              left
              ;; それ以外はエラー
              (error (format msg line col etype
                             (disp-p left)
                             (disp-p right))))))))
    ;; アドレス参照 *
    ((stx:deref-exp? ast)
     (let* ([arg (type-inspection (stx:deref-exp-arg ast))]
            [pos (stx:deref-exp-pos ast)]
            [line (position-line pos)]
            [col (position-col pos)]
            [msg (string-append
                  "~a:~a: ~a: indirection requires "
                  "pointer operand ('~a' invalid)")])
       ;; ポインタ・配列は一つ取り除いたものを返す
       (cond ((pointer? arg) 
              (pointer-type arg))
             ((array? arg)
              (array-type arg))
             ;;ポインタでも配列でもない場合はエラー
             (else
              (raise (type-inspect-error
                      (format msg line col etype
                              (disp-p arg))
                      (current-continuation-marks)))))))
    ;; アドレス取得 &
    ((stx:addr-exp? ast)
     (let ([var (stx:addr-exp-var ast)])
       ;; varの型に
       (pointer (type-inspection var))))
    
    ;; 関数呼び出し
    ((stx:call-exp? ast)
     (let* ([fun-para-types (fun-params
                             (decl-type (stx:call-exp-tgt ast)))]
            [call-arg-types (map (lambda (x)
                                   (cons (type-inspection (car x))
                                         (cdr x)))
                                 (stx:call-exp-args ast))]
            [msg (string-append
                  "~a:~a: ~a: incompatible pointer to integer "
                  "conversion passing '~a' to parameter of type '~a'")])
       (cond
         ;; 引数の個数が少ない場合
         ((> (length fun-para-types) (length call-arg-types))
          (let* ([expect (length fun-para-types)]
                 [have (length call-arg-types)]
                 [pos (if (null? call-arg-types)
                          (stx:call-exp-ppos ast)
                          (cdr (last call-arg-types)))]
                 [line (position-line pos)]
                 [col (+ (position-col pos) 1)]
                 [msg (string-append
                       "~a:~a: ~a: too few arguments "
                       "to function call, expected ~a, have ~a")])
            (raise (type-inspect-error
                    (format msg line col etype expect have)
                    (current-continuation-marks)))))
         ;; 引数の個数が多い場合
         ((< (length fun-para-types) (length call-arg-types))
          (let* ([expect (length fun-para-types)]
                 [have (length call-arg-types)]
                 [pos (cdr (list-ref call-arg-types expect))]
                 [line (position-line pos)]
                 [col (position-col pos)]
                 [msg (string-append
                       "~a:~a: ~a: too many arguments "
                       "to function call, expected ~a, have ~a")])
            (raise (type-inspect-error
                    (format msg line col etype expect have)
                    (current-continuation-marks)))))
         ;; 引数の個数が一致している時
         (else
          ;; 引数の型がすべて合ってるかどうか調べる
          ;; 配列の場合は先頭をpointerに置き換えたものになら
          ;; 代入することができる
          (if (and (andmap
               (lambda (x y pos)
                 (let ([line (position-line pos)]
                       [col (position-col pos)])
                   (cond
                     ((equal? x y) #t) ;型が合うかどうか
                     ((and (array? y)
                           (equal? x
                                   (pointer (array-type y))))
                      #t)
                     (else (raise (type-inspect-error
                                   (format msg line col etype
                                           (disp-p y)
                                           (disp-p x))
                                   (current-continuation-marks)))))))
               fun-para-types           ;関数定義の引数の型の列
               (map car call-arg-types) ;呼び出し側の引数の型の列
               (map cdr call-arg-types) ;呼び出し側の引数の位置情報の列
               )
                   (andmap typed? call-arg-types))
              ;; 引数の型が合っていれば返り値の型を返す
              (fun-type (decl-type (stx:call-exp-tgt ast)))
              #f)))))
    
    ;; キャスト
    ((stx:cast-exp? ast)
     (let* ([src (type-inspection (stx:cast-exp-src ast))]
            [type (stx:cast-exp-type ast)]
            [pos (stx:cast-exp-pos ast)]
            [line (position-line pos)]
            [col (position-col pos)]
            [vmsg "~a:~a: ~a: illigal cast type '~a'"]
            [pmsg "~a:~a: ~a: pointer cannot be cast to type '~a'"]
            [worn "~a:~a: worning: '~a' is rounded down to cast into '~a'\n"])
       (cond
         ;; float から int へのキャスト：切り捨てになるので警告
         ((and (equal? src 'float) (equal? type 'int))
          (eprintf (format worn line col src type))
          type)
         ;; int から float へのキャスト
         ((and (equal? src 'int) (equal? type 'float))
          type)
         ;; (void) はエラー
         ((equal? type 'void)
          (raise (type-inspect-error
                  (format vmsg line col etype type)
                  (current-continuation-marks))))
         ;;それ以外ならポインタをキャストしてるのでエラー
         (else (raise (type-inspect-error
                       (format pmsg line col etype type)
                       (current-continuation-marks)))))))
    
    ;; 即値
    (else (cond ((exact-integer? ast) 'int)
                ((flonum? ast) 'float)
                (else ast)))))

;********************************************************************

;; 意味解析本体
(define (sem parse)
  ;; エラーハンドリング
  (with-handlers ([name-resolve-error?
                   (lambda (e) (begin (eprintf (exn-message e))))]
                  [type-inspect-error?
                   (lambda (e) (begin (eprintf (exn-message e))))]
                  [expression-form-error?
                   (lambda (e) (begin (eprintf (exn-message e))))])
    ;; プログラムの埋め込み
    (let ([sem-program
           (parse->sem
            (stx:program
             (append `(,(stx:fun-prot
                         'void
                         'print
                         `(,(cons (cons 'int (position -1 -1 -1))
                                  (cons 'v   (position -1 -1 -1))))
                         (position -1 -1 -1)
                         (position -1 -1 -1)))
                     (stx:program-declrs parse)))
            initial-delta
            0)])
      ;;型検査をしてから意味解析後の構文木を得る
      (begin (type-inspection (car sem-program))
             sem-program))))