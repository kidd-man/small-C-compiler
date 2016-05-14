#lang racket
(require "parser.rkt"
         (prefix-in stx: "syntax.rkt")
         (prefix-in sms: "semsyntax.rkt")
         parser-tools/lex)

(define initial-delta (lambda (x) #f))
(define (extend-delta delta x data)
  (lambda (y) (if (equal? x y) data (delta y))))

(define ... 'voidだよ)
(struct decl    (name lev kind type) #:transparent)
(struct pointer (type)               #:transparent)
(struct array   (type size)          #:transparent)
(struct fun     (type params)        #:transparent)
(struct para    (plist)              #:transparent)

(define-struct (name-exception exn:fail:user) ())

  
(define (parse->sem parse env lev)
  (define (star-type->type list)
    (if (null? (cdr list))
        (car list)
        (pointer (star-type->type (cdr list)))))
  (define (find-array-name array)
    (if (stx:array-exp? (stx:array-exp-name array))
        (find-array-name (stx:array-exp-name array))
        (stx:array-exp-name array)))
  (define make-star
      (lambda (x) (cond ((pointer? x)
                         (cons '* (make-star (pointer-type x))))
                        ((array? x)
                         (cons '* (make-star (array-type x))))
                        ((stx:array-exp? x)
                         (cons '* (make-star (stx:array-exp-type x))))
                        (else x))))
    (define typed?
      (lambda (x) (if (boolean? x)
                      x
                      (cond ((pair? x)
                             (typed? (cdr x)))
                            ((pointer? x)
                             (typed? (pointer-type x)))
                            ((array? x)
                             (typed? (array-type x)))
                            (else (if (or (equal? x 'int)
                                          (equal? x 'float))
                                      #t #f))))))
  (define ename "name resolve error: ")
  ;; 処理本体
  (define (make-sem parse)
    (cond
      ((stx:declar? parse)
         ;; 要改名 タイプを返す関数
       (define (make-array-type arr)
         (if (stx:array-exp? arr)
             (if (stx:array-exp? (stx:array-exp-name arr))
                 (array (make-array-type (stx:array-exp-name arr)) (stx:array-exp-size arr))
                 (array (star-type->type (stx:array-exp-type arr)) (stx:array-exp-size arr)))
             (star-type->type (car arr))))
       (cons
        (stx:declar
         (map
          (lambda (x)
            (let* ((namepos (if (stx:array-exp? x) (cdr (find-array-name x)) (cddr x)))
                   (name (if (stx:array-exp? x) (car (find-array-name x)) (cadr x)))
                   (type (make-array-type x))
                   (obj (env name))
                   (line (number->string (position-line namepos)))
                   (col (number->string (position-col namepos)))
                   (msg (string-append line ":" col ": "
                                       ename "error: redeclaration of '"
                                       (symbol->string name) "'")))
              (cons type
                    (if obj
                        (cond
                          ;; 既にその名前が関数として定義されている場合
                          ((or (equal? (decl-kind obj) 'fun) (equal? (decl-kind obj) 'proto))
                           (if (equal? lev 0)
                               (begin (display (string-append msg " as different kind of symbol")
                                               (current-error-port))
                                      (newline (current-error-port)))
                               ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                               (let ((new-obj (decl name lev 'var type)))
                                 (begin (set! env (extend-delta env name new-obj)) new-obj))))
                          ;; すでにその名前が変数として定義されている場合
                          ((equal? (decl-kind obj) 'var)
                           (if (equal? lev (decl-lev obj))
                               (begin (display msg (current-error-port))
                                      (newline (current-error-port)))
                               ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                               (let ((new-obj (decl name lev 'var type)))
                                 (begin (set! env (extend-delta env name new-obj)) new-obj))))
                          ;; 既にその名前がパラメータとして宣言されている場合
                          ((equal? (decl-kind obj) 'parm)
                           (let ((new-obj (decl name lev 'var type)))
                             (begin (display msg (current-error-port)) ;;警告
                                    (set! env (extend-delta env name new-obj)) ;;環境に新しいオブジェクトを追加して
                                    new-obj)))) ;;nameをnew-objに置き換える
                        (let ((new-obj (decl name lev 'var type)))
                          ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                          (begin (set! env (extend-delta env name new-obj)) new-obj))))))
          (stx:declar-declrs parse))
         (stx:declar-pos parse))
        (let ((n-types (map (lambda (x) (make-star (make-array-type x))) (stx:declar-declrs parse)))
              (pos (stx:declar-pos parse)))
          ;; void関連の除外
          (if (andmap typed? n-types)
              #t (begin (display n-types)(error "www") #f)))))
      
      ((stx:fun-prot? parse)
       (let* ((name (stx:fun-prot-name parse))
              ;(namepos (cddr parse))
              (parms (stx:fun-prot-parms parse))
              (funtype (stx:fun-prot-type parse))
              (paratypes (map caar parms))
              (type (fun (star-type->type funtype)
                         (map star-type->type paratypes)))
              (n-funtype (make-star funtype))
              (n-paratypes (map make-star paratypes))
              (obj (env name))
              (pos (stx:fun-prot-pos parse)))
         (cons
          (stx:fun-prot (star-type->type (stx:fun-prot-type parse))
                        ;(decl name lev 'proto type)
                        (if obj
                            ;; エラー処理
                            (cond
                              ;; 既にその名前が関数としてプロトタイプ宣言されている場合
                              ((equal? (decl-kind obj) 'proto)
                               ;; 型が一致しているか確認
                               (let ((old-type (fun-type (decl-type obj))) ;登録されている関数の型
                                     (old-paratypelist (fun-params (decl-type obj))) ;登録されている関数のパラメータの型の列
                                     (new-type (star-type->type (stx:fun-prot-type parse))) ;新しく宣言した関数の型
                                     (new-paratypelist (map (lambda (x) (star-type->type (caar x)))
                                                            (stx:fun-prot-parms parse)))) ;新しく宣言した関数のパラメータの型の列
                                 ;; 型が一致するかどうか
                                 (if (and (equal? old-type new-type) (equal? old-paratypelist new-paratypelist))
                                     ;; 型が一致した場合は環境になにもせずnameをnew-objで置き換える
                                     (let ((new-obj (decl name lev 'proto type)))
                                       new-obj)
                                     ;; 型が一致しない場合はエラー
                                     (error "エラー!すでに宣言した同じ名前の関数と型が異なるよ!"))))
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
                                     (error "エラー!すでに宣言した同じ名前の関数と型が異なるよ!"))))
                              ;; すでにその名前が変数として定義されている場合
                              ((equal? (decl-kind obj) 'var)
                               (if (equal? lev (decl-lev obj))
                                   ;; レベルが同じ場合は二重宣言としてエラー
                                   (error "エラー!すでに同じ名前の変数が宣言されているよ!")
                                   ;; レベルが違う場合はオブジェクトを作成し環境に書き込みnameをnew-objで置き換える
                                   (let ((new-obj (decl name lev 'proto type)))
                                     (begin (set! env (extend-delta env name new-obj)) new-obj)))))
                            ;; 未宣言の場合はオブジェクトを作成し環境に書き込みnameをnew-objで置き換える
                            (let ((new-obj (decl name lev 'proto type)))
                              (begin (set! env (extend-delta env name new-obj)) new-obj)))
                        (parse->sem (para parms) env (+ lev 1)) ;;para構造体を作って再帰 この再帰に限ってペアを返さない
                        pos)
          (if (and
               ;; void関連の除外
               (andmap typed? paratypes)
               ;; 関数の返り値はvoidも可
               (or (typed? n-funtype) (equal? n-funtype 'void)))
              #t (begin (display pos) #f)))))
      
      ;; 関数定義
      ((stx:fun-def? parse)
       (let* ((name (stx:fun-def-name parse))
              (parms (stx:fun-def-parms parse))
              (funtype (stx:fun-def-type parse))
              (paratypes (map caar (stx:fun-def-parms parse)))
              (type (fun (star-type->type funtype)
                         (map star-type->type paratypes)))
              (n-funtype (make-star funtype))
              (n-paratypes (map make-star paratypes))
              (body (parse->sem (stx:fun-def-body parse) env (+ lev 2)))
              (obj (env name))
              (pos (stx:fun-def-pos parse)))
         (cons
          (stx:fun-def (star-type->type (stx:fun-def-type parse))
                       (if obj
                           ;; 既に名前が環境に登録されている場合
                           (cond
                             ;; 既にその名前が関数としてプロトタイプ宣言されている場合
                             ((equal? (decl-kind obj) 'proto)
                              (let ((old-type (fun-type (decl-type obj))) ;登録されている関数返り値の型
                                    (old-paratypelist (fun-params (decl-type obj))) ;登録されている関数のパラメータの型の列
                                    (new-type (star-type->type (stx:fun-def-type parse))) ;新しく宣言した関数返り値の型
                                    (new-paratypelist (map (lambda (x) (star-type->type (caar x)))
                                                           (stx:fun-def-parms parse)))) ;新しく宣言した関数のパラメータの型の列
                                ;; 型が一致するかどうか
                                (if (and (equal? old-type new-type) (equal? old-paratypelist new-paratypelist))
                                    ;; 型が一致した場合は環境書き込みnameをnew-objで置き換える
                                    (let ((new-obj (decl name lev 'proto type)))
                                      (begin (set! env (extend-delta env name new-obj)) new-obj))
                                    ;; 型が一致しない場合はエラー
                                    (begin (display old-paratypelist) (display new-paratypelist) (error "エラー!すでに宣言した同じ名前の関数と型が異なるよ!")))))
                             ;; 既にその名前が関数として定義されている場合
                             ((equal? (decl-kind obj) 'fun)
                              ;; 二重宣言なので無条件でエラー
                              (error "エラー!すでに同じ名前の関数が定義されているよ!"))
                             ;; 変数として宣言されている場合
                             ((equal? (decl-kind obj) 'var)
                              ;; 登録された変数のレベルをチェック
                              (if (equal? (decl-lev obj) lev)
                                  ;; 大域変数(レベルが等しい)ならば二重宣言なのでエラー
                                  (error "エラー!すでに同じ名前の変数が定義されているよ!")
                                  ;; レベルが異なれば環境にnew-objを追加しnameをnew-objで置き換える
                                  (let ((new-obj (decl name lev 'fun type)))
                                    (begin (set! env (extend-delta env name new-obj)) new-obj)))))
                           ;; 環境になければ環境にnew-objを追加しnameをnew-objで置き換える
                           (let ((new-obj (decl name lev 'fun type)))
                             (begin (display name) (display "いれたよ！") (set! env (extend-delta env name new-obj)) new-obj)))
                       (parse->sem (para parms) env (+ lev 1)) ;;para構造体を作って再帰 Lvは+1 この再帰に限ってペアを返さない
                       (car body) ;;bodyに対して再帰 Lvは+2
                       pos)
          (if (and
               ;; 複文すべてが#t（well-typed）かどうか
               (andmap (lambda (x) x)
                       (filter (lambda (x) (not (pair? x))) (cdr body)))
               ;; すべての返り値の型があってるかどうかの検証
               (andmap (lambda (x) (equal? n-funtype (cdr x)))
                       (filter pair? (cdr body)))
               ;; void関連の除外
               (and (andmap typed? n-paratypes)
                    (or (typed? n-funtype) (equal? n-funtype 'void))))
              #t (begin (display pos) #f)))))
      
      ((para? parse)
       (map (lambda (x)
              (let* ((type (star-type->type (caar x)))
                     (name (cadr x))
                     ;(pos (cddr x))
                     (obj (env name)))
                (if obj
                    (if (equal? 'parm (decl-type obj))
                        (error "エラー!すでに同じ名前の引数が宣言されているよ!")
                        ;; paraでなければ二重宣言ではないので環境に書き込みnameをnew-objに置き換える
                        (let ((new-obj (decl name lev 'parm type)))
                          (begin (set! env (extend-delta env name new-obj)) (cons type new-obj))))
                    ;; 環境にない場合は環境に書き込みnameをnew-objに置き換える
                    (let ((new-obj (decl name lev 'parm type)))
                      (begin (set! env (extend-delta env name new-obj)) (cons type new-obj))))))
            (para-plist parse)))
      ((stx:if-stmt? parse)
       (let ((test (parse->sem (stx:if-stmt-test parse) env lev))
             (tbody (parse->sem (stx:if-stmt-tbody parse) env (+ lev 1)))
             (ebody (parse->sem (stx:if-stmt-ebody parse) env (+ lev 1)))
             (pos (stx:if-stmt-pos parse)))
         (cons
          (stx:if-stmt (car test)
                       (car tbody)
                       (car ebody)
                       pos)
          (if (and (equal? (cdr test) 'int) (cdr tbody) (cdr ebody))
              #t (begin (display pos) #f)))))
      
      ((stx:while-stmt? parse)
       (let ((test (parse->sem (stx:while-stmt-test parse) env lev))
             (body (parse->sem (stx:while-stmt-body parse) env (+ lev 1)))
             (pos (stx:while-stmt-pos parse)))
         (cons
          (stx:while-stmt (car test)
                          (car body)
                          pos)
          (if (and (equal? (cdr test) 'int) (cdr body))
              #t (begin (display (cdr body)) #f)))))
      ((stx:return-stmt? parse)
       (let ((exp (parse->sem (stx:return-stmt-exp parse) env lev))
             (pos (stx:return-stmt-exp parse)))
         (cons
          (stx:return-stmt (car exp)
                           pos)
          (cons 'return (cdr exp)))))
      
      ((stx:assign-exp? parse)
       (let ((var (parse->sem (stx:assign-exp-var parse) env lev))
             (src (parse->sem (stx:assign-exp-src parse) env lev))
             (pos (stx:assign-exp-pos parse)))
         (cons
          (if (or (stx:deref-exp? (car var)) (stx:var-exp? (car var)))
              ;; *または、変数名かつarrayでない場合はおｋ
              (stx:assign-exp (car var)
                              (car src)
                              pos)
              ;; それ以外はエラー
              (error "エラー！代入式の左辺がおかしいよ！"))
          (if (equal? (cdr var) (cdr src))
              (cdr var)
              (error "エラー!代入演算のところの型がおかしいよ!")))))
      
      ((stx:log-exp? parse)
       (let ((op (stx:log-exp-op parse))
             (left (parse->sem (stx:log-exp-left parse) env lev))
             (right (parse->sem (stx:log-exp-right parse) env lev))
             (pos (stx:log-exp-pos parse)))
         (cons
          (stx:log-exp op
                       (car left)
                       (car right)
                       pos)
          (if (or (equal? op '&&) (equal? op '||))
              ;; &&,||:leftとrightがともにint型なら、int型がつく
              (if (and (equal? (cdr left) 'int) (equal? (cdr right) 'int))
                  'int
                  (error "エラー!&&,||の両辺はint型だよ!"))
              ;; ==,!=:leftとrightに同じ型がつくなら、int型がつく
              (if (equal? (cdr left) (cdr right))
                  'int
                  (error "エラー!比較演算の両辺は同じものだけだよ!"))))))
      
      ((stx:rop-exp? parse)
       (let ((op (stx:rop-exp-op parse))
             (left (parse->sem (stx:rop-exp-left parse) env lev))
             (right (parse->sem (stx:rop-exp-right parse) env lev))
             (pos (stx:rop-exp-pos parse)))
         (cons
          (stx:rop-exp op
                       (car left)
                       (car right)
                       pos)
          ;; >,<,>=,<=:leftとrightに同じ型がつくなら、int型がつく
          (if (equal? (cdr left) (cdr right))
               'int
               (error "エラー!比較演算の両辺は同じものだけだよ!")))))
      
      ((stx:aop-exp? parse)
       (let ((op (stx:aop-exp-op parse))
             (left (parse->sem (stx:aop-exp-left parse) env lev))
             (right (parse->sem (stx:aop-exp-right parse) env lev))
             (pos (stx:aop-exp-pos parse)))
         (cons
          (stx:aop-exp op
                       (car left)
                       (car right)
                       pos)          
          (cond ((equal? op '+)
                 (cond ((equal? (cons '* (cdr left)) (cdr right))
                        (cdr right))
                       ((equal? (cons '* (cdr right)) (cdr left))
                        (cdr left))
                       ((equal? (cdr left) (cdr right))
                        (cdr left))
                       (else (error "エラー!int型と**int型の足し算的なことをしているよ!"))))
                ((equal? op '-)
                 (if (equal? (cdr right) 'int)
                     (cdr left)
                     (error "エラー!マイナスの右辺はint型だけだよ!")))
                (else
                 (if (and (equal? (cdr left) 'int)
                          (equal? (cdr right) 'int))
                     (cdr left)
                     (error "エラー!*と/の両辺はint型だけだよ!")))))))
      ;; *
      ((stx:deref-exp? parse)
       (let ((arg (parse->sem (stx:deref-exp-arg parse) env lev))
             (pos (stx:deref-exp-pos parse)))
         (cons
          (stx:deref-exp (car arg)
                         pos)
          (if (pair? (cdr arg))
              (cdr (cdr arg))
              (error "エラー!*の後ろがポインタじゃないよ!")))))
      ;; &
      ((stx:addr-exp? parse)
       (let ((var (parse->sem (stx:addr-exp-var parse) env lev))
             (pos (stx:addr-exp-pos parse)))
         (cons
          (if (stx:var-exp? (car var))
              (stx:addr-exp (car var)
                            pos)
              (error "エラー！＆演算子の後ろはメモリ上の場所を表すような式でないといけないよ！"))
          (cons '* (cdr var)))))
      ((stx:comma-exp? parse)
       (let ((left (parse->sem (stx:comma-exp-left parse) env lev))
             (right (parse->sem (stx:comma-exp-right parse) env lev))
             (pos (stx:comma-exp-pos parse)))
         (cons
          (stx:comma-exp (car left)
                         (car right)
                         pos)
          (if (and (typed? (cdr left)) (typed? (cdr right)))
              (cdr right)
              (error "エラー!コンマ演算のどこかに型がついてないものがあるよ!")))))
      ((stx:var-exp? parse)
       (let* ((name (stx:var-exp-var parse))
              (pos (stx:var-exp-pos parse))
              (obj (env name))
              (var-decl
               (if obj
                   ;; 環境に登録されている場合
                   (cond
                     ;; 変数または引数ならばobjを参照して置き換える
                     ((or (equal? (decl-kind obj) 'var) (equal? (decl-kind obj) 'parm))
                      obj)
                     ;; 関数ならば関数を変数として参照しているのでエラー
                     ((or (equal? (decl-kind obj) 'fun) (equal? (decl-kind obj) 'proto))
                      (error "エラー!関数を変数として参照しているよ!")))
                   ;; 環境に登録されていない場合未宣言の変数を参照しているのでエラー
                   (begin (display name) (error "エラー!宣言していない変数を参照しているよ!"))))
              (type (make-star (decl-type var-decl))))
         (cons
          (stx:var-exp var-decl pos)
          type)))
      ((stx:call-exp? parse)
       (let* ((name (stx:call-exp-tgt parse))
              (args (parse->sem (stx:call-exp-args parse) env lev))
              (pos (stx:call-exp-pos parse))
              (obj (env name))
              (func-decl
               (if obj
               ;; 環境に登録されている場合
               (cond
                 ;; 変数または引数ならば変数を関数として参照しているのでエラー
                 ((or (equal? (decl-kind obj) 'var) (equal? (decl-kind obj) 'parm))
                  (error "エラー!変数を関数として参照しているよ!"))
                 ;; 関数ならばobjを参照して置き換える
                 ((or (equal? (decl-kind obj) 'fun) (equal? (decl-kind obj) 'proto))
                  obj))
               ;; 環境に登録されていない場合未宣言の変数を参照しているのでエラー
               (error "エラー!宣言していない関数を参照しているよ!")))
              (func-paratypes (fun-params (decl-type func-decl)))
              (call-argtypes (cdr args))
              (func-type (fun-type (decl-type func-decl))))
         (cons
          (stx:call-exp
           func-decl
           (car args)
           pos)
         (if (equal? (length func-paratypes)
                     (length call-argtypes))
             (if (and (andmap equal? func-paratypes  call-argtypes)
                      (andmap typed? call-argtypes))
                 ;; voidならwell-typedだから#tを返す。それ以外ならintかfloatだからそれを返す。
                 (if (equal? func-type 'void)
                     #t
                     func-type)
                 (error "エラー!関数呼び出しの引数の型が違うよ!"))
             (error "エラー!関数呼び出しの引数の数があってないよ!")))))
      
      ((stx:print-stmt? parse)
       (let ((exp (parse->sem (stx:print-stmt-exp parse) env lev)))
         (cons (stx:print-stmt (car exp))
               #t)))

      (else (cons parse
                  (if (number? parse)
                  'int
                  '???)))))
  
   (cond ((list? parse)
          (cons
           (map (lambda (x) (car (make-sem x))) parse)
           (map (lambda (x) (cdr (make-sem x))) parse)))
         ((stx:program? parse)
          (cons
           (stx:program (map (lambda (x) (car (make-sem x))) (stx:program-stmts parse)))
           (andmap (lambda (x) (cdr (make-sem x))) (stx:program-stmts parse))))
         ((stx:cmpd-stmt? parse)
          (cons
           (stx:cmpd-stmt (map (lambda (x) (car (make-sem x))) (stx:cmpd-stmt-stmts parse)))
           (map (lambda (x) (cdr (make-sem x))) (stx:cmpd-stmt-stmts parse))))
         (else (make-sem parse))))



  
  ;;;;;;;; typeに関する意味解析
;  (define (type-inspection ast)
;    (define make-star
;      (lambda (x) (cond ((pointer? x)
;                         (cons '* (make-star (pointer-type x))))
;                        ((array? x)
;                         (cons '* (make-star (array-type x))))
;                        (else x))))
;    (define typed?
;      (lambda (x) (if (boolean? x)
;                      x
;                      (cond ((pair? x)
;                             (typed? (cdr x)))
;                            ((pointer? x)
;                             (typed? (pointer-type x)))
;                            ((array? x)
;                             (typed? (array-type x)))
;                            (else (if (or (equal? x 'int)
;                                          (equal? x 'float))
;                                      #t #f))))))
;  (cond ((list? ast)
;         (andmap typed? (map type-inspection ast)))
;        ((stx:declar? ast)
;         (let ((types (map (lambda (x) (make-star (car x))) (stx:declar-declrs ast)))
;               (pos (stx:declar-pos ast)))
;           ;; void関連の除外
;           (if (andmap typed? types)
;               #t (begin (error pos) #f))))
        
;        ((stx:fun-prot? ast)
;         (let ((type (make-star (stx:fun-prot-type ast)))
;               (parms (map (lambda (x) (make-star (car x))) (stx:fun-prot-parms ast)))
;               (pos (stx:fun-prot-pos ast)))
;           (if (and
;                ;; void関連の除外
;                (andmap typed? parms)
;                ;; 関数の返り値はvoidも可
;                (or (typed? type) (equal? type 'void)))
;               #t (begin (display pos) #f))))
;        ((stx:fun-def? ast)
;         (let ((type (make-star (stx:fun-def-type ast)))
;               (params (map (lambda (x) (make-star (car x))) (stx:fun-def-parms ast)))
;               (body (map type-inspection (stx:fun-def-body ast)))
;               (pos (stx:fun-def-pos ast)))
;           (if (and
;                ;;
;                (andmap (lambda (x) x)
;                        (filter (lambda (x) (not (pair? x))) body))
;                ;;
;                (andmap (lambda (x) (equal? type (cdr x)))
;                        (filter pair? body))
;                ;; void関連の除外
;                (and (andmap typed? params)
;                     (or (typed? type) (equal? type 'void))))
;               #t (begin (display pos) #f))))
;        ((stx:if-stmt? ast)
;         (let ((test (type-inspection (stx:if-stmt-test ast)))
;               (tbody (type-inspection (stx:if-stmt-tbody ast)))
;               (ebody (type-inspection (stx:if-stmt-ebody ast)))
;               (pos (stx:if-stmt-pos ast)))
;           (if (and (equal? test 'int) tbody ebody)
;               #t (begin (display pos) #f))))
        
;        ((stx:while-stmt? ast)
;         (let ((test (type-inspection (stx:while-stmt-test ast)))
;               (body (type-inspection (stx:while-stmt-body ast)))
;               (pos (stx:while-stmt-pos ast)))
;           (if (and (equal? test 'int) body)
;               #t (begin (display body) #f))))
        
        ;; 関数定義から見えるようにペアを返す(car部はどうでもいい。)
;        ((stx:return-stmt? ast)
;         (let ((exp (type-inspection (stx:return-stmt-exp ast))))
;           (cons 'return exp)))
        
;        ((stx:print-stmt? ast) #t)
        
        ;; 変数参照式はdecl-typeの型がつく。arrayはポインタとみなす。
;        ((stx:var-exp? ast)
;         (let ((type (decl-type (stx:var-exp-var ast))))
;             (make-star type)))
        
;        ((stx:comma-exp? ast)
;         (let ((left (type-inspection (stx:comma-exp-left ast)))
;               (right (type-inspection (stx:comma-exp-right ast))))
;           (if (and (typed? left) (typed? right))
;               right
;               (error "エラー!コンマ演算のどこかに型がついてないものがあるよ!"))))
        
;        ((stx:assign-exp? ast)
;         (let ((var (type-inspection (stx:assign-exp-var ast)))
;               (src (type-inspection (stx:assign-exp-src ast))))
;           (if (equal? var src)
;               var
;               (error "エラー!代入演算のところの型がおかしいよ!"))))
        
;        ((stx:log-exp? ast)
;         (let ((op (stx:log-exp-op ast))
;               (left (type-inspection (stx:log-exp-left ast)))
;               (right (type-inspection (stx:log-exp-right ast))))
;           (if (or (equal? op '&&) (equal? op '||))
;               ;; &&,||:leftとrightがともにint型なら、int型がつく
;               (if (and (equal? left 'int) (equal? right 'int))
;                   'int
;                   (error "エラー!&&,||の両辺はint型だよ!"))
;               ;; ==,!=:leftとrightに同じ型がつくなら、int型がつく
;               (if (equal? left right)
;               'int
;               (error "エラー!比較演算の両辺は同じものだけだよ!")))))
        
;        ((stx:rop-exp? ast)
;         (let ((op (stx:rop-exp-op ast))
;               (left (type-inspection (stx:rop-exp-left ast)))
;               (right (type-inspection (stx:rop-exp-right ast))))
;           ;; >,<,>=,<=:leftとrightに同じ型がつくなら、int型がつく
;           (if (equal? left right)
;               'int
;               (error "エラー!比較演算の両辺は同じものだけだよ!"))))
        
;        ((stx:aop-exp? ast)
;         (let ((op (stx:aop-exp-op ast))
;               (left (type-inspection (stx:aop-exp-left ast)))
;               (right (type-inspection (stx:aop-exp-right ast))))
;         (cond ((equal? op '+)
;                (cond ((equal? (cons '* left) right)
;                       right)
;                      ((equal? (cons '* right) left)
;                       left)
;                      ((equal? left right)
;                       left)
;                      (else (error "エラー!int型と**int型の足し算的なことをしているよ!"))))
;               ((equal? op '-)
;                (if (equal? right 'int)
;                    left
;                    (error "エラー!マイナスの右辺はint型だけだよ!")))
;               (else
;                (if (and (equal? left 'int)
;                         (equal? right 'int))
;                    left
;                    (error "エラー!*と/の両辺はint型だけだよ!"))))))
        ; *
;        ((stx:deref-exp? ast)
;         (let ((arg (type-inspection (stx:deref-exp-arg ast))))
;           (if (pair? arg)
;               (cdr arg)
;               (error "エラー!*の後ろがポインタじゃないよ!"))))
        ; &
;        ((stx:addr-exp? ast)
;         (let ((var (stx:addr-exp-var ast)))
;               (cons '* var)))
        
;        ((stx:call-exp? ast)
;         (let ((fun-para-types (fun-params (decl-type (stx:call-exp-tgt ast))))
;               (call-arg-types (map type-inspection (stx:call-exp-args ast))))
;         (if (equal? (length fun-para-types)
;                     (length call-arg-types))
;             (if (and (andmap equal? fun-para-types  call-arg-types)
;                      (andmap typed? call-arg-types))
;                 ;; voidならwell-typedだから#tを返す。それ以外ならintかfloatだからそれを返す。
;                 (if (equal? (fun-type (decl-type (stx:call-exp-tgt ast))) 'void)
;                     #t
;                     (fun-type (decl-type (stx:call-exp-tgt ast))))
;                 (error "エラー!関数呼び出しの引数の型が違うよ!"))
;             (error "エラー!関数呼び出しの引数の数があってないよ!"))))
;        (else (if (number? ast)
;                  'int
;                  '???))))

;(with-handlers ((name-exception? (lambda (e) 'a)))
  


;  )


(define (sem parse)

  (car (parse->sem parse
;        (stx:program
;         (append `(,(stx:fun-def '(void)
;                                 'print
;                                 (list (list* (cons '(int) (void)) 'v (void)))
;                                 (stx:cmpd-stmt `(,(stx:print-stmt (stx:var-exp 'v (void)))))
;                                 (void)))
;                 (stx:program-stmts parse)))
        initial-delta
        0)))