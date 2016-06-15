#lang racket
(provide (all-defined-out))
(require "parser.rkt"
         (prefix-in stx: "syntax.rkt")
         (prefix-in sms: "semsyntax.rkt")
         parser-tools/lex)

;(define initial-delta (lambda (x) #f))
;(define (extend-delta delta x data)
;  (lambda (y) (if (equal? x y) data (delta y))))

(define initial-delta '())
(define (extend-delta delta x data)
  (cons (cons x data) delta))
(define (lookup-env delta x)
  (if (null? delta) #f
      (if (equal? x (car (car delta))) (cdr (car delta))
          (lookup-env (cdr delta) x))))
;(define (para-extend-delta delta paras)
;  (if (null? paras) delta
;      (cons (cons (cadar paras) (decl (cadar paras) 1 'para (caaar paras)))
;            (para-extend-delta delta (cdr paras)))))



(define ... 'voidだよ)
(struct decl    (name lev kind type) #:transparent)
(struct pointer (type)               #:transparent)
(struct array   (type size)          #:transparent)
(struct fun     (type params)        #:transparent)
(struct para    (plist)              #:transparent)
(struct p-and-b (para body)          #:transparent)
(struct returns (type-set)           #:transparent)

(define-struct (name-resolve-error exn:fail:user) ())
(define-struct (type-inspect-error exn:fail:user) ())
(define-struct (expression-form-error exn:fail:user) ())

;; エラーの種類
(define ename "name resolve error")
(define etype "type inspection error")
(define eform "expression form error")

;;構文解析した抽象構文木->さらに意味解析をした抽象構文木
(define (parse->sem parse env lev)
    ;; (* ... * type) -> (pointer ... (pointer type))
  (define (make-pointer-type list)
    (if (pair? list)
        (pointer (make-pointer-type (cdr list)))
        list))
  ;; (array-exp '(array) ... (array-exp type (* ... * name) size) ... size)
  ;; -> (array ... (array (pointer ... (pointer type)) size) ... size)
  (define (make-array-type arr)
    (if (stx:array-exp? (stx:array-exp-name arr))
        (array (make-array-type (stx:array-exp-name arr)) (stx:array-exp-size arr))
        (array (make-pointer-type (stx:array-exp-type arr)) (stx:array-exp-size arr))))
  ;; 
  (define (find-array-name array)
    (if (stx:array-exp? (stx:array-exp-name array))
        (find-array-name (stx:array-exp-name array))
        (stx:array-exp-name array)))
  
  ;; 処理本体
  (define (make-sem parse)
    (cond
      ((stx:declar? parse)
       (stx:declar
           (let* ((dec (stx:declar-dec parse))
                  (namepos (if (stx:array-exp? dec) (cdr (find-array-name dec)) (cddr dec)))
                  (name (if (stx:array-exp? dec) (car (find-array-name dec)) (cadr dec)))
                  (type (if (stx:array-exp? dec) (make-array-type dec) (make-pointer-type (car dec))))
                  (obj (lookup-env env name))
                  (line (number->string (position-line namepos)))
                  (col (number->string (position-col namepos)))
                  (msg "~a:~a: ~a: redeclaration of '~a'"))
                   (if obj
                       (cond
                         ;; 既にその名前が関数として定義されている場合
                         ((or (equal? (decl-kind obj) 'fun) (equal? (decl-kind obj) 'proto))
                          (if (equal? lev 0)
                              ;; 例外を投げる
                              (raise (name-resolve-error
                                      (string-append (format msg line col ename name)
                                                     " as different kind of symbol: function")
                                      (current-continuation-marks)))
                              ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                              (let ((new-obj (decl name lev 'var type)))
                                (begin (set! env (extend-delta env name new-obj)) new-obj))))
                         ;; すでにその名前が変数として定義されている場合
                         ((equal? (decl-kind obj) 'var)
                          (if (equal? lev (decl-lev obj))
                              ;; 例外を投げる
                              (raise (name-resolve-error (format msg line col ename name)
                                                         (current-continuation-marks)))
                              ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                              (let ((new-obj (decl name lev 'var type)))
                                (begin (set! env (extend-delta env name new-obj)) new-obj))))
                         ;; 既にその名前がパラメータとして宣言されている場合
                         ((equal? (decl-kind obj) 'parm)
                          (let ((new-obj (decl name lev 'var type)))
                            (begin (eprintf (string-append (format msg line col ename name)
                                                           " as diferent kind of symbol: parameter\n")) ;;警告
                                   (set! env (extend-delta env name new-obj)) ;;環境に新しいオブジェクトを追加して
                                   new-obj)))) ;;nameをnew-objに置き換える
                       (let ((new-obj (decl name lev 'var type)))
                         ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                         (begin (set! env (extend-delta env name new-obj)) new-obj))))
        (stx:declar-pos parse)))
      ((stx:fun-prot? parse)
       (let* ((name (stx:fun-prot-name parse))
              ;(namepos (cddr parse))
              (parms (stx:fun-prot-parms parse))
              (type (fun (make-pointer-type (stx:fun-prot-type parse))
                         (map (lambda (x) (make-pointer-type (caar x))) (stx:fun-prot-parms parse))))
              (obj (lookup-env env name))
              (tpos (stx:fun-prot-tpos parse))
              (npos (stx:fun-prot-npos parse))
              (nline (position-line npos))
              (ncol (position-col npos)))
         (stx:fun-prot (make-pointer-type (stx:fun-prot-type parse))
                       ;(decl name lev 'proto type)
                       (if obj
                           ;; エラー処理
                           (cond
                             ;; 既にその名前が関数としてプロトタイプ宣言されている場合
                             ((equal? (decl-kind obj) 'proto)
                              ;; 型が一致しているか確認
                              (let ((old-type (fun-type (decl-type obj))) ;登録されている関数の型
                                    (old-paratypelist (fun-params (decl-type obj))) ;登録されている関数のパラメータの型の列
                                    (new-type (make-pointer-type (stx:fun-prot-type parse))) ;新しく宣言した関数の型
                                    (new-paratypelist (map (lambda (x) (make-pointer-type (caar x)))
                                                           (stx:fun-prot-parms parse)))) ;新しく宣言した関数のパラメータの型の列
                                ;; 型が一致するかどうか
                                (if (and (equal? old-type new-type) (equal? old-paratypelist new-paratypelist))
                                    ;; 型が一致した場合は環境になにもせずnameをnew-objで置き換える
                                    (let ((new-obj (decl name lev 'proto type)))
                                      new-obj)
                                    ;; 型が一致しない場合はエラー
                                    (let ((msg "~a:~a: ~a: conflicting types for '~a'"))
                                      (raise (name-resolve-error (format msg nline ncol ename name)
                                                                 (current-continuation-marks)))))))
                             ;; 既にその名前が関数として定義されている場合
                             ((equal? (decl-kind obj) 'fun)
                              ;; 型が一致しているか確認
                              (let ((old-type (fun-type (decl-type obj))) ;登録されている関の返り値数の型
                                    (old-paratypelist (fun-params (decl-type obj))) ;登録されている関数のパラメータの型の列
                                    (new-type (stx:fun-prot-type parse)) ;新しく宣言した関数の返り値の型
                                    (new-paratypelist (map caar (stx:fun-prot-parms parse)))) ;新しく宣言した関数のパラメータの型の列
                                ;; 型が一致するかどうか
                                (if (and (equal? old-type new-type) (equal? old-paratypelist new-paratypelist))
                                    ;; 型が一致した場合は環境に何もせずnameをnew-objで置き換える
                                    (let ((new-obj (decl name lev 'proto type)))
                                      new-obj)
                                    ;; 型が一致しない場合はエラー
                                    (let ((msg "~a:~a: ~a: conflicting types for '~a'"))
                                      (raise (name-resolve-error (format msg nline ncol ename name)
                                                                 (current-continuation-marks)))))))
                             ;; すでにその名前が変数として定義されている場合
                             ((equal? (decl-kind obj) 'var)
                              (if (equal? lev (decl-lev obj))
                                  ;; 大域変数(レベルが等しい)ならば二重宣言なのでエラー
                                  (let ((msg "~a:~a: ~a: redifinition of '~a' as different kind of symbol"))
                                    (raise (name-resolve-error (format msg nline ncol ename name)
                                                               (current-continuation-marks))))
                                  ;; レベルが違う場合はオブジェクトを作成し環境に書き込みnameをnew-objで置き換える
                                  (let ((new-obj (decl name lev 'proto type)))
                                    (begin (set! env (extend-delta env name new-obj)) new-obj)))))
                           ;; 未宣言の場合はオブジェクトを作成し環境に書き込みnameをnew-objで置き換える
                           (let ((new-obj (decl name lev 'proto type)))
                             (begin (set! env (extend-delta env name new-obj)) new-obj)))
                       ;; パラメータ
                       (p-and-b-para (parse->sem (p-and-b parms (void)) env (+ lev 1)))
                       tpos
                       npos)))
      
      ;; 関数定義
      ((stx:fun-def? parse)
       (let* ((name (stx:fun-def-name parse))
              (parms (stx:fun-def-parms parse))
              (type (fun (make-pointer-type (stx:fun-def-type parse))
                         (map (lambda (x) (make-pointer-type (caar x))) (stx:fun-def-parms parse))))
              (body (stx:cmpd-stmt-stmts (stx:fun-def-body parse)))
              (obj (lookup-env env name))
              (tpos (stx:fun-def-tpos parse))
              (npos (stx:fun-def-npos parse))
              (nline (position-line npos))
              (ncol (position-col npos)))
         (stx:fun-def (make-pointer-type (stx:fun-def-type parse))
                      (if obj
                          ;; 既に名前が環境に登録されている場合
                          (cond
                            ;; 既にその名前が関数としてプロトタイプ宣言されている場合
                            ((equal? (decl-kind obj) 'proto)
                             (let ((old-type (fun-type (decl-type obj))) ;登録されている関数返り値の型
                                   (old-paratypelist (fun-params (decl-type obj))) ;登録されている関数のパラメータの型の列
                                   (new-type (make-pointer-type (stx:fun-def-type parse))) ;新しく宣言した関数返り値の型
                                   (new-paratypelist (map (lambda (x) (make-pointer-type (caar x)))
                                                          (stx:fun-def-parms parse)))) ;新しく宣言した関数のパラメータの型の列
                               ;; 型が一致するかどうか
                               (if (and (equal? old-type new-type) (equal? old-paratypelist new-paratypelist))
                                   ;; 型が一致した場合は環境書き込みnameをnew-objで置き換える
                                   (let ((new-obj (decl name lev 'fun type)))
                                     (begin (set! env (extend-delta env name new-obj)) new-obj))
                                   ;; 型が一致しない場合はエラー
                                   (let ((msg "~a:~a: ~a: conflicting types for '~a'"))
                                   (raise (name-resolve-error (format msg nline ncol ename name)
                                                              (current-continuation-marks)))))))
                            ;; 既にその名前が関数として定義されている場合
                            ((equal? (decl-kind obj) 'fun)
                             ;; 二重宣言なので無条件でエラー
                             (let ((msg "~a:~a: ~a: redifinition of '~a'"))
                             (raise (name-resolve-error (format msg nline ncol ename name)
                                                        (current-continuation-marks)))))
                            ;; 変数として宣言されている場合
                            ((equal? (decl-kind obj) 'var)
                             ;; 登録された変数のレベルをチェック
                             (if (equal? (decl-lev obj) lev)
                                 ;; 大域変数(レベルが等しい)ならば二重宣言なのでエラー
                                 (let ((msg "~a:~a: ~a: redifinition of '~a' as different kind of symbol"))
                                 (raise (name-resolve-error (format msg nline ncol ename name)
                                                            (current-continuation-marks))))
                                 ;; レベルが異なれば環境にnew-objを追加しnameをnew-objで置き換える
                                 (let ((new-obj (decl name lev 'fun type)))
                                   (begin (set! env (extend-delta env name new-obj)) new-obj)))))
                          ;; 環境になければ環境にnew-objを追加しnameをnew-objで置き換える
                          (let ((new-obj (decl name lev 'fun type)))
                            (begin (set! env (extend-delta env name new-obj)) new-obj)))
                      ;; パラメータ
                      (p-and-b-para (parse->sem (p-and-b parms body) env (+ lev 1)))
                      ;;body Lvはparameter-Lv+1
                      (p-and-b-body (parse->sem (p-and-b parms body) env (+ lev 1)))
                      tpos
                      npos)))
      ;; パラメータと関数本体を一緒に見るための分岐
      ((p-and-b? parse)
       (p-and-b
        (map (lambda (x)
               (let* ((type (make-pointer-type (caar x)))
                      (name (cadr x))
                      (tpos (cdar x))
                      (npos (cddr x))
                      (obj (lookup-env env name))
                      (msg "~a:~a: ~a: redefinition of parameter '~a'")
                      (line (position-line npos))
                      (col (position-col npos)))
                 (if obj
                     (if (equal? 'parm (decl-kind obj))
                         ;; paraなら二重宣言なのでエラーを投げる
                         (raise (name-resolve-error (format msg line col ename name)
                                                    (current-continuation-marks)))
                         ;; paraでなければ二重宣言ではないので環境に書き込みnameをnew-objに置き換える
                         (let ((new-obj (decl name lev 'parm type)))
                           (begin (set! env (extend-delta env name new-obj))
                                  (cons (cons type tpos) (cons new-obj npos)))))
                     ;; 環境にない場合は環境に書き込みnameをnew-objに置き換える
                     (let ((new-obj (decl name lev 'parm type)))
                       (begin (set! env (extend-delta env name new-obj))
                              (cons (cons type tpos) (cons new-obj npos)))))))
             (p-and-b-para parse))
        ;; body。voidはプロトタイプ宣言用
        (if (void? (p-and-b-body parse)) (void)
            (stx:cmpd-stmt (parse->sem (p-and-b-body parse) env (+ lev 1))))))
      ;; if文
      ((stx:if-stmt? parse)
       (let ((test (stx:if-stmt-test parse))
             (tbody (stx:if-stmt-tbody parse))
             (ebody (stx:if-stmt-ebody parse))
             (pos (stx:if-stmt-pos parse)))
         (stx:if-stmt (make-sem test)
                      (parse->sem tbody env lev)
                      (parse->sem ebody env lev)
                      pos)))
      ((stx:while-stmt? parse)
       (let ((test (stx:while-stmt-test parse))
             (body (stx:while-stmt-body parse))
             (pos (stx:while-stmt-pos parse)))
         (stx:while-stmt (make-sem test)
                         (parse->sem body env lev)
                         pos)))
      ((stx:return-stmt? parse)
       (let ((exp (stx:return-stmt-exp parse))
             (pos (stx:return-stmt-pos parse)))
         (stx:return-stmt (make-sem exp)
                          pos)))
      ((stx:assign-exp? parse)
       (let* ((var (stx:assign-exp-var parse))
              (src (stx:assign-exp-src parse))
              (vpos (stx:assign-exp-vpos parse))
              (eqpos (stx:assign-exp-eqpos parse))
              (line (position-line vpos))
              (col (position-col vpos))
              (msg "~a:~a: ~a: expression is not assignable"))
         (if (or (stx:deref-exp? var) (stx:var-exp? var))
             ;; *または、変数名かつarrayでない場合はおｋ
             (stx:assign-exp (make-sem var)
                             (make-sem src)
                             vpos
                             eqpos)
             ;; それ以外はエラー
             (raise (name-resolve-error
                     (format msg line col ename)
                     (current-continuation-marks))))))
      ((stx:log-exp? parse)
       (let ((op (stx:log-exp-op parse))
             (left (stx:log-exp-left parse))
             (right (stx:log-exp-right parse))
             (pos (stx:log-exp-pos parse)))
         (stx:log-exp op
                      (make-sem left)
                      (make-sem right)
                      pos)))
      ((stx:rop-exp? parse)
       (let ((op (stx:rop-exp-op parse))
             (left (stx:rop-exp-left parse))
             (right (stx:rop-exp-right parse))
             (pos (stx:rop-exp-pos parse)))
         (stx:rop-exp op
                      (make-sem left)
                      (make-sem right)
                      pos)))
      ((stx:aop-exp? parse)
       (let ((op (stx:aop-exp-op parse))
             (left (stx:aop-exp-left parse))
             (right (stx:aop-exp-right parse))
             (pos (stx:aop-exp-pos parse)))
         (stx:aop-exp op
                      (make-sem left)
                      (make-sem right)
                      pos)))
      ;; *
      ((stx:deref-exp? parse)
       (let ((arg (stx:deref-exp-arg parse))
             (pos (stx:deref-exp-pos parse)))
         (stx:deref-exp (make-sem arg)
                        pos)))
      ;; &
      ((stx:addr-exp? parse)
       (let* ((var (stx:addr-exp-var parse))
              (pos (stx:addr-exp-pos parse))
              (line (position-line pos))
              (col (position-col pos))
              (type (type-inspection var))
              (msg "~a:~a: ~a: cannot take the address of an value of type '~a'"))
         (if (stx:var-exp? var)
             (stx:addr-exp (make-sem var)
                           pos)
             (raise (name-resolve-error (format msg line col ename type)
                                        (current-continuation-marks))))))
      ((stx:comma-exp? parse)
       (let ((left (stx:comma-exp-left parse))
             (right (stx:comma-exp-right parse))
             (pos (stx:comma-exp-pos parse)))
         (stx:comma-exp (make-sem left)
                        (make-sem right)
                        pos)))
      ((stx:var-exp? parse)
       (let* ((name (stx:var-exp-var parse))
              (pos (stx:var-exp-pos parse))
              (line (position-line pos))
              (col (position-col pos))
              (obj (lookup-env env name)))
             (stx:var-exp
              (if obj
                  ;; 環境に登録されている場合
                  (cond
                    ;; 変数または引数ならばobjを参照して置き換える
                    ((or (equal? (decl-kind obj) 'var) (equal? (decl-kind obj) 'parm))
                     obj)
                    ;; 関数ならば関数を変数として参照しているのでエラー
                    ((or (equal? (decl-kind obj) 'fun) (equal? (decl-kind obj) 'proto))
                     (let ((msg (string-append "~a:~a: ~a: cannot refer "
                                               "function name '~a' as a varb expression")))
                     (raise (name-resolve-error (format msg line col ename name)
                                                (current-continuation-marks))))))
                  ;; 環境に登録されていない場合未宣言の変数を参照しているのでエラー
                  (let ((msg (string-append "~a:~a: ~a: implicit declaration"
                                            " of varb '~a' is invalid")))
                  (raise (name-resolve-error (format msg line col ename name)
                                             (current-continuation-marks)))))
              pos)))
      ((stx:call-exp? parse)
       (let* ((name (stx:call-exp-tgt parse))
              (args (stx:call-exp-args parse))
              (npos (stx:call-exp-npos parse))
              (ppos (stx:call-exp-ppos parse))
              (line (position-line npos))
              (col (position-col npos))
              (obj (lookup-env env name)))
         (stx:call-exp           
          (if obj
              ;; 環境に登録されている場合
              (cond
                ;; 変数または引数ならば変数を関数として参照しているのでエラー
                ((or (equal? (decl-kind obj) 'var) (equal? (decl-kind obj) 'parm))
                 (let ((fulmsg (format
                          (string-append "~a:~a: ~a: called object '~a' is"
                                         " not a function or function pointer")
                          line col ename name)))
                 (raise
                  (name-resolve-error fulmsg (current-continuation-marks)))))
                ;; 関数ならばobjを参照して置き換える
                ((or (equal? (decl-kind obj) 'fun) (equal? (decl-kind obj) 'proto))
                 obj))
              ;; 環境に登録されていない場合未宣言の関数を参照しているのでエラー
              (let ((fulmsg (format
                          (string-append "~a:~a: ~a: implicit declaration"
                                         " of function '~a' is invalid")
                          line col ename name)))
              (raise
               (name-resolve-error fulmsg (current-continuation-marks)))))
          (map (lambda (x) (cons (make-sem (car x)) (cdr x))) args)
          npos
          ppos)))
      ((stx:print-stmt? parse)
       (let ((exp (stx:print-stmt-exp parse)))
         (stx:print-stmt (make-sem exp))))
      (else parse)))
  

   (cond ((stx:program? parse)
          (stx:program (map make-sem (stx:program-declrs parse))))
         ((stx:cmpd-stmt? parse)
          (stx:cmpd-stmt (map make-sem (stx:cmpd-stmt-stmts parse))))
         ((list? parse)
          (map make-sem parse))
         (else (make-sem parse))))



(define (type-inspection ast)
  (define make-star
    (lambda (x) (cond ((array? x)
                       (cons '* (make-star (array-type x))))
                      ((pointer? x)
                       (cons '* (make-star (pointer-type x))))
                      (else x))))
  ;; voidまたは
  ;;(list* * ... * 'intか'float)または
  ;;(pointer ... (pointer 'intか'float))または
  ;;(array ... (array 'intか'float))
  ;;-> #t
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
    
  (cond
    ((stx:program? ast)
     (andmap type-inspection (stx:program-declrs ast)))
    
    ((stx:cmpd-stmt? ast)
     (let* ((return-in '()))
       ;; 各文からreturnを集める
       ;;ここでreturn文以外も評価して検査する
       (for-each (lambda (x)
                   (if (returns? x)
                       (set! return-in
                             (reverse (set-union (reverse return-in) ;; for readable
                                                 (reverse (returns-type-set x)))))
                       (void)))
                 (map type-inspection (stx:cmpd-stmt-stmts ast)))
       ;; returnの集合のreturn構造体を返す
       (returns return-in)))
    ;; 変数宣言
    ((stx:declar? ast)
         (let* ((type (make-star (decl-type (stx:declar-dec ast))))
                (pos (stx:declar-pos ast))
                (line (position-line pos))
                (col (position-col pos))
                (msg "~a:~a: ~a: illigal type '~a' to verb expression"))
           ;; void関連の除外
           (if (typed? type)
               #t
               (raise (type-inspect-error (format msg line col etype type))))))
    ((stx:fun-prot? ast)
     (let* ((ftype (make-star (stx:fun-prot-type ast)))
            (params (map (lambda (x)
                           (cons (cons (make-star (caar x)) (cdar x))
                                 (cdr x)))
                         (stx:fun-prot-parms ast)))
            (fpos (stx:fun-prot-tpos ast))
            (fline (position-line fpos))
            (fcol (position-col fpos))
            (fmsg "~a:~a: ~a: illigal type '~a' to return type of function prototype"))
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
         (for-each (lambda (x)
                     (let* ((ptype (caar x))
                            (ppos (cdar x))
                            (pline (position-line ppos))
                            (pcol (position-col ppos))
                            (pmsg "~a:~a: ~a: illigal type '~a' to parameter"))
                       (if (and (not (equal? ptype 'void))
                                (typed? ptype))
                           #t
                           (raise (type-inspect-error
                                   (format pmsg pline pcol etype ptype)
                                   (current-continuation-marks))))))
                   params))))
    ;; 関数定義
    ((stx:fun-def? ast)
     (let* ((ftype (make-star (stx:fun-def-type ast)))
           (params (map (lambda (x)
                          (cons (cons (make-star (caar x)) (cdar x))
                                (cdr x)))
                        (stx:fun-def-parms ast)))
           (return-set (type-inspection (stx:fun-def-body ast)))
           (fpos (stx:fun-def-tpos ast))
           (fline (position-line fpos))
           (fcol (position-col fpos))
           (fmsg "~a:~a: ~a: illigal type '~a' to return type of function definition"))
       ;; エラーがなければ#tを返す
       (begin
         ;; 返り値の型検査
         (if (and
              ;; void関連の除外
              (andmap typed? params)
              ;; 関数の返り値はvoidも可
              (or (typed? ftype) (equal? ftype 'void)))
             #t
             (raise (type-inspect-error
                     (format fmsg fline fcol etype ftype)
                     (current-continuation-marks))))
         ;; 各パラメータから void 関連の検査
         (for-each (lambda (x)
                     (let* ((ptype (caar x))
                            (ppos (cdar x))
                            (pline (position-line ppos))
                            (pcol (position-col ppos))
                            (pmsg "~a:~a: ~a: illigal type '~a' in parameter"))
                       (if (and (not (equal? ptype 'void))
                                (typed? ptype))
                           #t
                           (raise (type-inspect-error
                                   (format pmsg pline pcol etype ptype)
                                   (current-continuation-marks))))))
                   params)
         
         ;; body の return の型があってるかどうかのチェックをしてから
         ;; #tを返す。この時他のbodyも評価する。
          (for-each (lambda (x)
                                  (let* ((rtype (car x))
                                         (rpos (cdr x))
                                         (rline (position-line rpos))
                                         (rcol (position-col rpos))
                                         (rmsg "~a:~a: ~a: missmatch return statement type for function ('~a' for '~a')"))
                                  (if (equal? ftype rtype) #t
                                      (raise (type-inspect-error
                                              (format rmsg rline rcol etype rtype ftype)
                                              (current-continuation-marks))))))
                    (returns-type-set return-set))　#t)))
    ;; if: testと２つのbodyを見る
    ((stx:if-stmt? ast)
     (let* ((test (type-inspection (stx:if-stmt-test ast)))
            (tbody (type-inspection (stx:if-stmt-tbody ast)))
            (ebody (type-inspection (stx:if-stmt-ebody ast)))
            (pos (stx:if-stmt-pos ast))
            (line (position-line pos))
            (col (position-col pos))
            (msg "~a:~a: ~a: illigal type '~a' in IF test expression\nexpected: 'int'"))
       (if (equal? test 'int)
           (returns (set-union (returns-type-set tbody)
                               (returns-type-set ebody)))
           (raise (type-inspect-error
                   (format msg line col etype test)
                   (current-continuation-marks))))))
    ;; while: testとbodyを見る
    ((stx:while-stmt? ast)
     (let* ((test (type-inspection (stx:while-stmt-test ast)))
            (body (type-inspection (stx:while-stmt-body ast)))
            (pos (stx:while-stmt-pos ast))
            (line (position-line pos))
            (col (position-col pos))
            (msg "~a:~a: ~a: illigal type '~a' in WHILE test expression\nexpected: 'int'"))
       (if (equal? test 'int)
           body
           (raise (type-inspect-error
                   (format msg line col etype test)
                   (current-continuation-marks))))))
    ;; リターン: returnを集める集合で返す（cdr部はpos）
    ((stx:return-stmt? ast)
     (let ((exp (type-inspection (stx:return-stmt-exp ast)))
           (pos (stx:return-stmt-pos ast)))
       (returns `(,(cons exp pos)))))
    ;; 出力処理: 常に正しい
    ((stx:print-stmt? ast) #t)
    ;; 変数参照式: decl-typeの型がつく。arrayはポインタとみなす。
    ((stx:var-exp? ast)
     (let ((type (decl-type (stx:var-exp-var ast))))
       (make-star type)))
    ;; コンマ: 右を取るのみ
    ((stx:comma-exp? ast)
     (let ((right (type-inspection (stx:comma-exp-right ast))))
           right))
    ;; 代入:
    ((stx:assign-exp? ast)
     (let* ((var (type-inspection (stx:assign-exp-var ast)))
           (src (type-inspection (stx:assign-exp-src ast)))
           (pos (stx:assign-exp-eqpos ast))
           (line (position-line pos))
           (col (position-col pos))
           (msg "~a:~a: ~a: assignment of distinct pointer types ('~a' into '~a')"))
       (if (equal? var src)
           var
           (raise (expression-form-error (format msg line col etype src var)
                                         (current-continuation-marks))))))
    
    ;; &&,||
    ((stx:log-exp? ast)
     (let* ((op (stx:log-exp-op ast))
            (left (type-inspection (stx:log-exp-left ast)))
            (right (type-inspection (stx:log-exp-right ast)))
            (pos (stx:log-exp-pos ast))
            (line (position-line pos))
            (col (position-col pos))
            (msg "~a:~a: ~a: comparison of distinct pointer types ('~a' and '~a')"))
       
       (cond
         ;; &&,||:leftとrightがともにint型なら、int型がつく
         ((and (equal? left 'int) (equal? right 'int))
          'int)
         ;; ともにintまたはfloatがつくなら、intがつく
         ;; 拡張>>float型があれば,両方floatにする
;         ((and (equal? left 'int) (equal? right 'float))
;          'int)
;         ((and (equal? left 'float) (equal? right 'int))
;          'int)
         (else
          (raise (type-inspect-error
                  (format msg line col etype left right)
                  (current-continuation-marks)))))))
    
    ((stx:rop-exp? ast)
     (let* ((op (stx:rop-exp-op ast))
            (left (type-inspection (stx:rop-exp-left ast)))
            (right (type-inspection (stx:rop-exp-right ast)))
            (pos (stx:rop-exp-pos ast))
            (line (position-line pos))
            (col (position-col pos))
            (msg "~a:~a: ~a: comparison of distinct pointer types ('~a' and '~a')"))
       ;; >,<,>=,<=：leftとrightにintかfloatがつくなら、int型がつく
       ;; 拡張＞＞float型があれば,両方floatにする
       (cond ((and (equal? left right) (typed? left))
              'int)
;             ((and (equal? left 'int) (equal? right 'float))
;              'int)
;             ((and (equal? left 'float) (equal? right 'int))
;              'int)
             (else
              (raise (type-inspect-error
                      (format msg line col etype left right)
                      (current-continuation-marks)))))))
    ;; 四則演算
    ((stx:aop-exp? ast)
     (let* ((op (stx:aop-exp-op ast))
            (left (type-inspection (stx:aop-exp-left ast)))
            (right (type-inspection (stx:aop-exp-right ast)))
            (pos (stx:aop-exp-pos ast))
            (line (position-line pos))
            (col (position-col pos))
            (msg "~a:~a: ~a: invalid operands to binary expression ('~a' and '~a')"))
       (cond ((equal? op '+)
              (cond ((equal? (cons '* left) right)
                     right)
                    ((equal? (cons '* right) left)
                     left)
                    ((equal? left right)
                     left)
                    (else (raise (type-inspect-error (format msg line col etype left right)
                                                     (current-continuation-marks))))))
             ((equal? op '-)
              (if (or (equal? right 'int) (equal? right 'float))
                  left
                  (raise (type-inspect-error (format msg line col etype left right)
                                             (current-continuation-marks)))))
             (else
              (if (and (equal? left 'int)
                       (equal? right 'int))
                  left
                  (error (format msg line col etype left right)))))))
    ;; アドレス参照 *
    ((stx:deref-exp? ast)
     (let* ((arg (make-star (type-inspection (stx:deref-exp-arg ast))))
            (pos (stx:deref-exp-pos ast))
            (line (position-line pos))
            (col (position-col pos))
            (msg "~a:~a: ~a: indirection requires pointer operand ('~a' invalid)"))
       (if (pair? arg) 
           (cdr arg)
           (raise (type-inspect-error (format msg line col etype arg)
                                      (current-continuation-marks))))))
    ;; アドレス取得 &
    ((stx:addr-exp? ast)
     (let ((var (stx:addr-exp-var ast)))
       (cons '* var)))
    ;; 関数呼び出し
    ((stx:call-exp? ast)
     (let* ((fun-para-types (fun-params (decl-type (stx:call-exp-tgt ast))))
            (call-arg-types (map (lambda (x) (cons (type-inspection (car x)) (cdr x)))
                                 (stx:call-exp-args ast)))
            (msg (string-append
                  "~a:~a: ~a: incompatible pointer to integer conversion passing '~a' "
                  "to parameter of type '~a'")))
       (cond
         ;; 引数の個数の違い
         ((> (length fun-para-types) (length call-arg-types))
          (let* ((expect (length fun-para-types))
                 (have (length call-arg-types))
                 (pos (if (null? call-arg-types)
                          (stx:call-exp-ppos ast)
                          (cdr (last call-arg-types))))
                 (line (position-line pos))
                 (col (+ (position-col pos) 1))
                 (msg "~a:~a: ~a: too few arguments to function call, expected ~a, have ~a"))
            (raise (type-inspect-error
                    (format msg line col etype expect have)
                    (current-continuation-marks)))))
         ((< (length fun-para-types) (length call-arg-types))
          (let* ((expect (length fun-para-types))
                 (have (length call-arg-types))
                 (pos (cdr (list-ref call-arg-types expect)))
                 (line (position-line pos))
                 (col (position-col pos))
                 (msg "~a:~a: ~a: too many arguments to function call, expected ~a, have ~a"))
            (raise (type-inspect-error
                    (format msg line col etype expect have)
                    (current-continuation-marks)))))
         (else
          (if (and (andmap equal? fun-para-types  (map car call-arg-types))
                   (andmap typed? call-arg-types))
              ;; 返り値の型を返す。
              (fun-type (decl-type (stx:call-exp-tgt ast)))
              ;;どこがあってないか探してエラー intとfloatの違いは警告を出す!!!(未実装)
              (for-each
               (lambda (x y pos)
                 (let ((line (position-line pos))
                       (col (position-col pos)))
                   (if (equal? x y) (void);型が合うかどうか
                       (raise (type-inspect-error (format msg line col etype y x)
                                                  (current-continuation-marks))))))
               fun-para-types
               (map car call-arg-types)
               (map cdr call-arg-types)))))))
    ;; 即値
    (else (cond ((exact-integer? ast) 'int)
                ((flonum? ast)        'float)
                (else ast)))))

  


(define (sem parse)
  (with-handlers ([name-resolve-error? (lambda (e) (begin (eprintf (exn-message e))))]
                  [type-inspect-error? (lambda (e) (begin (eprintf (exn-message e))))])
    (let ((sem-program
           (parse->sem (stx:program
            (append `(,(stx:fun-prot 'void
                                    'print
                                    (list (list* (cons 'int (position 0 0 0)) 'v (position 0 0 0)))
                                    (position 0 0 0)
                                    (position 0 0 0)))
                    (stx:program-declrs parse))) initial-delta 0)))
      (begin (type-inspection sem-program)
             (pretty-print-ast sem-program)))))

(define (parse)
   (pretty-print-ast
    (parse->sem
     (stx:program
      (append `(,(stx:fun-prot 'void
                               'print
                               (list (list* (cons 'int (position 0 0 0)) 'v (position 0 0 0)))
                               (position 0 0 0)
                               (position 0 0 0)))
              (stx:program-declrs (parse-file "testsort.c"))))
     initial-delta 0)))
(define (test) (if (type-inspection (parse)) (pretty-print-ast (parse)) (error "失敗")))