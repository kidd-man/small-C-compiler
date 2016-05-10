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
  (cond ((list? parse) (map (lambda (x) (parse->sem x env 0)) parse))
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
                               (error "エラー！すでに同じ名前の関数が宣言されているよ！")
                               ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                               (let ((new-obj (decl name lev 'var type)))
                                 (begin (set! env (extend-delta env name new-obj)) new-obj))))
                          ;; すでにその名前が変数として定義されている場合
                          ((equal? (decl-kind obj) 'var)
                           (if (equal? lev (decl-lev obj))
                               (error "エラー！すでに同じ名前の変数が宣言されているよ！")
                               ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                               (let ((new-obj (decl name lev 'var type)))
                                 (begin (set! env (extend-delta env name new-obj)) new-obj))))
                          ;; 既にその名前がパラメータとして宣言されている場合
                          ((equal? (decl-kind obj) 'parm)
                           (let ((new-obj (decl name lev 'var type)))
                             (begin (display "けいこく！パラメータと変数名が被ってるよ！") ;;警告
                                    (set! env (extend-delta env name new-obj)) ;;環境に新しいオブジェクトを追加して
                                    new-obj)))) ;;nameをnew-objに置き換える
                        (let ((new-obj (decl name lev 'var type)))
                          ;; 環境に新しいオブジェクトを追加してnameをnew-objに置き換える
                          (begin (set! env (extend-delta env name new-obj)) new-obj))))))
          (stx:declar-declrs parse))))
        ((stx:fun-prot? parse)
         (let* ((name (stx:fun-prot-name parse))
                ;(namepos (cddr parse))
                (parms (map (lambda (x) (decl (cadr x) lev 'para (caar x))) (stx:fun-prot-parms parse)))
                (type (fun (star-type->type (stx:fun-prot-type parse))
                           (map (lambda (x) (star-type->type (caar x))) (stx:fun-prot-parms parse))))
                (obj (env name)))
           (sms:fun-prot (stx:fun-prot-type parse)
                         ;(decl name lev 'proto type)
                         (if obj
                             ;; エラー処理
                             (cond
                               ;; 既にその名前が関数として定義されている場合
                               ((or (equal? (decl-kind obj) 'fun) (equal? (decl-kind obj) 'proto))
                                ;; 型が一致しているか確認
                                (let ((old-type (fun-type (decl-type obj))) ;登録されている関数の型
                                      (old-paratypelist (fun-params (decl-type obj))) ;登録されている関数のパラメータの型の列
                                      (new-type (stx:fun-prot-type parse)) ;新しく宣言した関数の型
                                      (new-paratypelist (map caar (stx:fun-prot-parms parse)))) ;新しく宣言した関数のパラメータの型の列
                                  ;; 型が一致するかどうか
                                  (if (and (equal? old-type new-type) (equal? old-paratypelist new-paratypelist))
                                      ;; 型が一致した場合はオブジェクトを作成し環境に書き込みnameをnew-objで置き換える
                                      (let ((new-obj (decl name lev 'proto type)))
                                        (begin (set! env (extend-delta env name new-obj)) new-obj))
                                      ;; 型が一致しない場合はエラー
                                      (error "エラー！すでに宣言した同じ名前の関数と型が異なるよ！"))))
                               ;; すでにその名前が変数として定義されている場合
                               ((equal? (decl-kind obj) 'var)
                                (if (equal? lev (decl-lev obj))
                                    ;; レベルが同じ場合は二重宣言としてエラー
                                    (error "エラー!すでに同じ名前の変数が宣言されているよ!")
                                    ;; レベルが違う場合はオブジェクトを作成し環境に書き込みnameをnew-objで置き換える
                                    (let ((new-obj (decl name lev 'proto type)))
                                      (begin (set! env (extend-delta env name new-obj)) new-obj)))))
                             ;; 宣言名が初めて現れる場合は環境に書き込みnameをnew-objで置き換える
                             (let ((new-obj (decl name lev 'proto type)))
                               (begin (set! env (extend-delta env name new-obj)) new-obj)))
                         (parse->sem (para parms) (+ lev 1) env)))) ;;para構造体を作って再帰
        ((stx:fun-def? parse)
         (let* ((name (stx:fun-def-name parse))
               (parms (map (lambda (x) (decl (cadr x) (+ lev 1) 'para (caar x))) (stx:fun-def-parms parse)))
               (type (fun (star-type->type (stx:fun-def-type parse))
                          (map (lambda (x) (star-type->type (caar x))) (stx:fun-def-parms parse))))
               (body (parse->sem (stx:fun-def-body parse) env (+ lev 1))))
         (sms:fun-def (star-type->type (stx:fun-def-type parse))
                                  (decl name lev 'fun type)
                                  (parse->sem (para parms) (+ lev 1) env) ;;para構造体を作って再帰 Lvは+1
                                  (parse->sem body (+ lev 2) env)))) ;;bodyに対して再帰 Lvは+2
        ((para? parse)
         (map (lambda (x)
                (let* ((type (car x))
                       (name (cadr x))
                       ;(pos (cddr x))
                       (obj (env name)))
                  ))
              parse))
        ;; 入会届！！！！！！！！！！！！！
        ((stx:if-stmt? parse) ...)
        ((stx:while-stmt? parse) ...)
        ((stx:return-stmt? parse) ...)
        ((stx:assign-exp? parse) ...)
        ((stx:log-exp? parse) ...)
        ((stx:rop-exp? parse) ...)
        ((stx:aop-exp? parse) ...)
        ((stx:deref-exp? parse) ...)
        ((stx:addr-exp? parse) ...)
        ((stx:array-exp? parse) (array (parse->sem (stx:array-exp-type parse) env lev) (parse->sem (stx:array-exp-size parse) env lev)))
        ((stx:call-exp? parse) ...)
        ((stx:comma-exp? parse) ...)
        (else parse)))

(define (sem parse) (parse->sem parse initial-delta 0))