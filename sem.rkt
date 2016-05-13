#lang racket
(require "parser.rkt"
         (prefix-in stx: "syntax.rkt")
         (prefix-in sms: "semsyntax.rkt"))

(define initial-delta (lambda (x) #f))
(define (extend-delta delta x data)
  (lambda (y) (if (equal? x y) data (delta y))))

(define ... 'voidだよ)
(struct decl    (name lev kind type) #:transparent)
(struct pointer (type)               #:transparent)
(struct array   (type size)          #:transparent)
(struct fun     (type params)        #:transparent)
(struct para    (plist)              #:transparent)

(define (parse->sem parse env lev)
  (define (star-type->type list)
    (if (null? (cdr list))
        (car list)
        (pointer (star-type->type (cdr list)))))
  (define (find-array-name array)
    (if (stx:array-exp? (stx:array-exp-name array))
        (find-array-name (stx:array-exp-name array))
        (car (stx:array-exp-name array))))
  (define (make-array-type arr)
    (if (stx:array-exp? (stx:array-exp-name arr))
        (array (make-array-type (stx:array-exp-name arr)) (stx:array-exp-size arr))
        (array (star-type->type (stx:array-exp-type arr)) (stx:array-exp-size arr))))
  ;; 処理本体
  (define (make-sem parse)
    (cond
      ((stx:declar? parse)
       (sms:declar
        (map
         (lambda (x)
           (let* (;(namepos (cdr (stx:array-exp-name x)))
                  (name (if (stx:array-exp? x) (find-array-name x) (cadr x)))
                  (type (if (stx:array-exp? x) (make-array-type x) (star-type->type (car x))))
                  (obj (env name)))
             (cons type
                   (if obj
                       (cond
                         ;; 既にその名前が関数として定義されている場合
                         ((or (equal? (decl-kind obj) 'fun) (equal? (decl-kind obj) 'proto))
                          (if (equal? lev 0)
                              (error "エラー!すでに同じ名前の関数が宣言されているよ!")
                              ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                              (let ((new-obj (decl name lev 'var type)))
                                (begin (set! env (extend-delta env name new-obj)) new-obj))))
                         ;; すでにその名前が変数として定義されている場合
                         ((equal? (decl-kind obj) 'var)
                          (if (equal? lev (decl-lev obj))
                              (error "エラー!すでに同じ名前の変数が宣言されているよ!")
                              ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                              (let ((new-obj (decl name lev 'var type)))
                                (begin (set! env (extend-delta env name new-obj)) new-obj))))
                         ;; 既にその名前がパラメータとして宣言されている場合
                         ((equal? (decl-kind obj) 'parm)
                          (let ((new-obj (decl name lev 'var type)))
                            (begin (display "けいこく!パラメータと変数名が被ってるよ!") ;;警告
                                   (set! env (extend-delta env name new-obj)) ;;環境に新しいオブジェクトを追加して
                                   new-obj)))) ;;nameをnew-objに置き換える
                       (let ((new-obj (decl name lev 'var type)))
                         ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                         (begin (set! env (extend-delta env name new-obj)) new-obj))))))
         (stx:declar-declrs parse))))
      ((stx:fun-prot? parse)
       (let* ((name (stx:fun-prot-name parse))
              ;(namepos (cddr parse))
              (parms (stx:fun-prot-parms parse))
              (type (fun (star-type->type (stx:fun-prot-type parse))
                         (map (lambda (x) (star-type->type (caar x))) (stx:fun-prot-parms parse))))
              (obj (env name)))
         (sms:fun-prot (stx:fun-prot-type parse)
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
                       (parse->sem (para parms) env (+ lev 1))))) ;;para構造体を作って再帰
      
      ;; 関数定義
      ((stx:fun-def? parse)
       (let* ((name (stx:fun-def-name parse))
              (parms (stx:fun-def-parms parse))
              (type (fun (star-type->type (stx:fun-def-type parse))
                         (map (lambda (x) (star-type->type (caar x))) (stx:fun-def-parms parse))))
              (body (parse->sem (stx:fun-def-body parse) env (+ lev 1)))
              (obj (env name)))
         (sms:fun-def (star-type->type (stx:fun-def-type parse))
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
                            (begin (set! env (extend-delta env name new-obj)) new-obj)))
                      (parse->sem (para parms) env (+ lev 1)) ;;para構造体を作って再帰 Lvは+1
                      (parse->sem body env (+ lev 2))))) ;;bodyに対して再帰 Lvは+2
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
       (let ((test (stx:if-stmt-test parse))
             (tbody (stx:if-stmt-tbody parse))
             (ebody (stx:if-stmt-ebody parse)))
         (sms:if-stmt (parse->sem test  env lev)
                      (parse->sem tbody env lev)
                      (parse->sem ebody env lev))))
      ((stx:while-stmt? parse)
       (let ((test (stx:while-stmt-test parse))
             (body (stx:while-stmt-body parse)))
         (sms:while-stmt (parse->sem test env lev)
                         (parse->sem body env lev))))
      ((stx:return-stmt? parse)
       (let ((exp (stx:return-stmt-exp parse)))
         (sms:return-stmt (parse->sem exp env lev))))
      ((stx:assign-exp? parse)
       (let ((var (stx:assign-exp-var parse))
             (src (stx:assign-exp-src parse)))
         (if (or (stx:deref-exp? var) (stx:var-exp? var))
             ;; *または、変数名かつarrayでない場合はおｋ
             (sms:assign-exp (parse->sem var env lev) (parse->sem src env lev))
             ;; それ以外はエラー
             (error "エラー！代入式の左辺がおかしいよ！"))))
      ((stx:log-exp? parse)
       (let ((op (stx:log-exp-op parse))
             (left (stx:log-exp-left parse))
             (right (stx:log-exp-right parse)))
         (sms:log-exp op (parse->sem left env lev) (parse->sem right env lev))))
      ((stx:rop-exp? parse)
       (let ((op (stx:rop-exp-op parse))
             (left (stx:rop-exp-left parse))
             (right (stx:rop-exp-right parse)))
         (sms:rop-exp op (parse->sem left env lev) (parse->sem right env lev))))
      ((stx:aop-exp? parse)
       (let ((op (stx:aop-exp-op parse))
             (left (stx:aop-exp-left parse))
             (right (stx:aop-exp-right parse)))
         (sms:aop-exp op (parse->sem left env lev) (parse->sem right env lev))))
      ;; *
      ((stx:deref-exp? parse)
       (let ((arg (stx:deref-exp-arg parse)))
         (sms:deref-exp (parse->sem arg env lev))))
      ;; &
      ((stx:addr-exp? parse)
       (let ((var (stx:addr-exp-var parse)))
         (if (stx:var-exp? var)
             (sms:addr-exp (parse->sem var env lev))
             (error "エラー！＆演算子の後ろはメモリ上の場所を表すような式でないといけないよ！"))))
      ((stx:comma-exp? parse)
       (let ((left (stx:comma-exp-left parse))
             (right (stx:comma-exp-right parse)))
         (sms:comma-exp (parse->sem left env lev)
                        (parse->sem right env lev))))
      ((stx:var-exp? parse)
       (let* ((name (stx:var-exp-var parse))
              (pos (stx:var-exp-pos parse))
              (obj (env name)))
         (if (equal? name 'print)
             'printdesuyoooooooooo
             (sms:var-exp
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
                  (error "エラー!宣言していない変数を参照しているよ!"))))))
      ((stx:call-exp? parse)
       (let* ((name (stx:call-exp-tgt parse))
              (args (stx:call-exp-args parse))
              (obj (env name)))
         (sms:call-exp           
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
              (error "エラー!宣言していない関数を参照しているよ!"))
          (parse->sem args env lev))))
      ((sms:print-stmt? parse)
       (let ((exp (sms:print-stmt-exp parse)))
         (sms:print-stmt (parse->sem exp env lev))))
      (else parse)))
  
  (if (list? parse)
      (map make-sem parse)
      (make-sem parse)))

(define (type-inspection ast)
  (define make-star
    (lambda (x) (cond ((pointer? x)
                       (cons '* (make-star (pointer-type x))))
                      ((array? x)
                       (cons '* (make-star (array-type x))))
                      (else x))))
  (define typed?
    (lambda (x) (if (array? x)
                    (typed? (array-type x))
                    (if (or (equal? x 'int) (equal? x 'float))
                        #t #f))))
  (cond ((list? ast) (andmap type-inspection ast))
        ((sms:declar? ast)
         (let ((types (map (lambda (x) (make-star (decl-type x))))))
           ;; void関連の除外
           (if (andmap typed? types)
               #t #f)))
        ((sms:fun-prot? ast)
         (let ((type (make-star (sms:fun-prot-type)))
               (args (map (lambda (x) (make-star (fun-params (decl-type x)))))))
           (if (and
                ;; void関連の除外
                (andmap typed? args)
                ;; 関数の返り値はvoidも可
                (or (typed? type) (equal? type 'void)))
               #t #f)))
        ((sms:fun-def? ast)
         (let ((type (make-star (sms:fun-def-type)))
               (args (map (lambda (x) (make-star (fun-params (decl-type x))))))
               (body (map type-inspection (sms:fun-def-body ast))))
           (if (and
                ;;
                (andmap (lambda (x) x)
                        (filter (lambda (x) (not (pair? x))) body))
                ;;
                (andmap (lambda (x) (equal? type (cdr x)))
                        (filter pair? body))
                ;; void関連の除外
                (and (andmap typed? args)
                     (or (typed? type) (equal? type 'void))))
               #t #f)))
        ((sms:if-stmt? ast)
         (let ((test (type-inspection (sms:if-stmt-test ast)))
               (tbody (type-inspection (sms:if-stmt-tbody ast)))
               (ebody (type-inspection (sms:if-stmt-ebody ast))))
           (if (and (equal? test 'int) tbody ebody)
               #t #f)))
        ((sms:while-stmt? ast)
         (let ((test (type-inspection (sms:while-stmt-test ast)))
               (body (type-inspection (sms:while-stmt-body ast))))
           (if (and (equal? test 'int) body)
               #t #f)))
        ;; 関数定義から見えるようにペアを返す（car部はどうでもいい。）
        ((sms:return-stmt? ast)
         (let ((exp (type-inspection (sms:return-stmt-exp ast))))
           (cons 'return exp)))
        ((sms:print-stmt? ast) #t)
        ;; 変数参照式はdecl-typeの型がつく。arrayはポインタとみなす。
        ((sms:var-exp? ast)
         (let ((type (decl-type (sms:var-exp-var ast))))
             (make-star type)))
        
        ((sms:comma-exp? ast)
         (let ((left (type-inspection (sms:comma-exp-left ast)))
               (right (type-inspection (sms:comma-exp-right ast))))
           (if (and (typed? left) (typed? right))
               right
               (error "エラー！コンマ演算のどこかに型がついてないものがあるよ！"))))
        
        ((sms:assign-exp? ast)
         (let ((var (type-inspection (sms:assign-exp-var ast)))
               (src (type-inspection (sms:assign-exp-src ast))))
           (if (equal? var src)
               var
               (error "エラー！代入演算のところの型がおかしいよ！"))))
        
        ((sms:log-exp? ast)
         (let ((op (sms:rop-exp-op ast))
               (left (type-inspection (sms:rop-exp-left ast)))
               (right (type-inspection (sms:rop-exp-right ast))))
           (if (or (equal? op '&&) (equal? '||))
               ;; &&,||：leftとrightがともにint型なら、int型がつく
               (if (and (equal? left 'int) (equal? right 'int))
                   'int
                   (error "エラー！&&,||の両辺はint型だよ！"))
               ;; ==,!=：leftとrightに同じ型がつくなら、int型がつく
               (if (equal? left right)
               'int
               (error "エラー!比較演算の両辺は同じものだけだよ!")))))
        
        ((sms:rop-exp? ast)
         (let ((op (sms:rop-exp-op ast))
               (left (type-inspection (sms:rop-exp-left ast)))
               (right (type-inspection (sms:rop-exp-right ast))))
           ;; >,<,>=,<=：leftとrightに同じ型がつくなら、int型がつく
           (if (equal? left right)
               'int
               (error "エラー！比較演算の両辺は同じものだけだよ！"))))
        
        ((sms:aop-exp? ast)
         (let ((op (sms:aop-exp-op ast))
               (left (type-inspection (sms:aop-exp-left ast)))
               (right (type-inspection (sms:aop-exp-right ast))))
         (cond ((equal? op '+)
                (cond ((equal? (cons '* left) right)
                       right)
                      ((equal? (cons '* right) left)
                       left)
                      ((equal? left right)
                       left)
                      (else (error "エラー!int型と**int型の足し算的なことをしているよ!"))))
               ((equal? op '-)
                (if (equal? right 'int)
                    left
                    (error "エラー!マイナスの右辺はint型だけだよ!")))
               (else
                (if (and (equal? left 'int)
                         (equal? right 'int))
                    left
                    (error "エラー!*と/の両辺はint型だけだよ!"))))))
        ; *
        ((sms:deref-exp? ast)
         (let ((arg (sms:deref-exp-arg ast)))
         (if (pair? arg)
             (cdr arg)
             (error "エラー！*の後ろがポインタじゃないよ！"))))
        ; &
        ((sms:addr-exp? ast)
         (let ((var (sms:addr-exp-var ast)))
               (cons '* var)))
        ((sms:call-exp? ast)
         (let ((fun-para-types (fun-params (decl-type (sms:call-exp-tgt ast))))
               (call-arg-types (map type-inspection (sms:call-exp-args ast))))
         (if (equal? (length fun-para-types)
                     (length call-arg-types))
             (if (and (andmap equal? fun-para-types  call-arg-types)
                      (andmap typed? call-arg-types))
                 (fun-type (decl-type (sms:call-exp-tgt ast)))
                 (error "エラー！関数呼び出しの引数の型が違うよ！"))
             (error "エラー！関数呼び出しの引数の数があってないよ！"))))
        (else (if (number? ast)
                  'int
                  '???))))

(define (sem parse)
  (parse->sem
   (append `(,(stx:fun-def '(void)
                           'print
                           (list (list* (cons '(int) void) 'v void))
                           (list (sms:print-stmt 'v))
                           void))
           parse)
   initial-delta
   0))