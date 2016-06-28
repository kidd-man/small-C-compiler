#lang racket
(provide (all-defined-out))
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (prefix-in stx: "syntax.rkt")
         parser-tools/yacc)
 
(define-tokens tokens-with-value
  (NUM ID))

(define-empty-tokens tokens-without-value
  (+ - * /
   < <= > >= == != += -=
   & && || = ++ --
   SEMI LPAR RPAR COMMA RETURN 
   LBRA RBRA LBBRA RBBRA
   INT FLOAT VOID
   IF ELSE DO WHILE FOR
   EOF))

(define-lex-trans uinteger
  (syntax-rules () ((_ d) (:* d))))

(define-lex-abbrevs
  (digit            (char-range "0" "9"))
  (digit-non-zero   (char-range "1" "9"))
  (number  (:or "0"
                (:: digit-non-zero
                    (uinteger digit))))
  (floatnumber (:: (uinteger digit)
                   "."
                   (uinteger digit)))
  (identifier-char (:or (char-range "a" "z")
                        (char-range "A" "Z")))
  (identifier (:: identifier-char
                  (:* (:or identifier-char digit "_")))))
 
(define small-c-lexer
  (lexer-src-pos
   ("+"        (token-+))
   ("-"        (token--))
   ("*"        (token-*))
   ("/"        (token-/))
   ("<"        (token-<))
   ("<="       (token-<=))
   (">"        (token->))
   (">="       (token->=))
   ("=="       (token-==))
   ("!="       (token-!=))
   ("+="       (token-+=))
   ("-="       (token--=))
   ("&"        (token-&))
   ("&&"       (token-&&))
   ("||"       (token-||))
   ("="        (token-=))
   ("++"       (token-++))
   ("--"       (token---))
   (";"        (token-SEMI))
   ("("        (token-LPAR))
   (")"        (token-RPAR))
   ("{"        (token-LBRA))
   ("}"        (token-RBRA))
   ("["        (token-LBBRA))
   ("]"        (token-RBBRA))
   (","        (token-COMMA))
   ("return"   (token-RETURN))
   ("if"       (token-IF))
   ("else"     (token-ELSE))
   ("do"       (token-DO))
   ("while"    (token-WHILE))
   ("for"      (token-FOR))
   ("int"      (token-INT))
   ("float"    (token-FLOAT))
   ("void"     (token-VOID))
   (number     (token-NUM (string->number lexeme)))
   (floatnumber (token-NUM (string->number lexeme)))
   (identifier (token-ID (string->symbol lexeme)))
   (whitespace (return-without-pos (small-c-lexer input-port)))
   ((eof)      (token-EOF))))

;; 一時的に使用する構造体

;; 前置インクリメント・ディクリメント: ++<exp> / --<exp>
(struct front-inct-exp (op var pos) #:transparent)
;; 後置インクリメント・ディクリメント: <exp>++ / <exp>--
(struct back-inct-exp  (op var pos) #:transparent)


(define small-c-parser
  (parser
   (start program)
   (end EOF)
   (src-pos)
   ;;(debug "small-c-parser.tbl")
   (suppress)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (error (format "parse error:~a,~a: ~a"
                           (position-line start-pos)
                           (position-col start-pos)
                           (if tok-value tok-value tok-name)))))
   (tokens tokens-with-value tokens-without-value)
   (grammar
    ;; プログラム
    (program
     ; <external-declaration>
     ((external-declaration)
      (if (list? $1)
          (stx:program `(,@$1))
          (stx:program `(,$1))))
     ; <program> <external-declaration>
     ((program external-declaration)
      (if (list? $2)
          (stx:program (append (stx:program-declrs $1) `(,@$2)))
          (stx:program (append (stx:program-declrs $1) `(,$2))))))
    ;; 大域宣言
    (external-declaration
     ; <declaration>
     ((declaration) $1)
     ; <function-prototype>
     ((function-prototype) $1)
     ; <function-definition>
     ((function-definition) $1))
    ;; 宣言
    (declaration
     ; <type-specifier> <declarator-list> 
     ((type-specifier declarator-list SEMI)
      ;; 配列・ポインタは構造が反転しているので
      ;; 正しい構造に直して
      ;; 型と構造木のペアに変換する
      (begin
        ;; 埋もれているIDを見つける関数
        (define (find-id str)
          (cond ((stx:array-exp? str)
                 (find-id (stx:array-exp-name str)))
                ((list? str)
                 (find-id (car str)))
                (else str)))
        ;; id や (id * ... *) ならば#tを返す関数
          (define (id? hoge)
            (cond ((stx:array-exp? hoge) #f)
                  ((list? hoge)
                   (if (id? (car hoge)) #t #f))
                  (else #t)))
        ;; 構造を反転させる関数
        ;; 多次元のarrayのtypeは'undefを取る(意味解析で調べる)
        (define (reverse-parse str init)
          (cond ((stx:array-exp? str)
                 (let ([name (stx:array-exp-name str)]
                       [size (stx:array-exp-size str)]
                       [pos (stx:array-exp-pos str)])
                 (if (id? init)
                     ;; nameにidを含む配列の場合
                     (if (list? init)
                         ;; *が付いている場合
                         (reverse-parse
                          name
                          (stx:array-exp (append (filter (lambda (x)
                                                           (equal? x '*))
                                                         init)
                                                 `(,$1))
                                     (last init) size pos))
                         ;; *が付いていない場合
                         (reverse-parse
                          name
                          (stx:array-exp $1 init size pos)))
                     ;; nameにidを含まない場合
                     (reverse-parse
                      name
                      (stx:array-exp 'undef init size pos)))))
                ((list? str)
                     (reverse-parse
                      (car str)
                      (append (cdr str) `(,init))))
                (else init)))
        ;; 宣言リストの本体
        (map (lambda (declr)
               (stx:declar
                (cons
                 ;; 宣言の型: 配列はundef(意味解析で調べる)
                 (if (id? declr)
                     (if (list? declr)
                         (append (cdr declr) $1)
                         $1)
                     'undef)
                 (if (id? declr)
                     (find-id declr)
                     (reverse-parse declr (find-id declr))))
                $1-start-pos))
             $2))))
    ;; 宣言変数のリスト
    (declarator-list
     ; <declarator>
     ((declarator) `(,$1))
     ; <declarator-list> , <declarator>
     ((declarator-list COMMA declarator)
      (append $1 `(,$3))))
    ;; 宣言変数
    (declarator
     ; <direct-declarator>
     ((direct-declarator) $1)
     ; * <declarator>
     ((* declarator) (if (list? $2)
                         (append $2 '(*))
                         (cons $2 '(*))))
     ; ( <declarator> )
     ((LPAR declarator RPAR)  $2))
    ;; 宣言変数そのもの(ポインタを除いたID)
    (direct-declarator
     ; <identifier>
     ((ID) (cons $1 $1-start-pos))
     ; <direct-declarator> [ <constant> ]
     ((declarator LBBRA NUM RBBRA) (stx:array-exp 'undef $1 $3 $3-start-pos)))
    ;; 関数プロトタイプ宣言
    (function-prototype
     ; <type-specifier> <function-declarator> ;
     ((type-specifier function-declarator SEMI)
      (let ((id-param (list-ref $2 (- (length $2) 1))))
        (stx:fun-prot (append (filter (lambda (x) (eq? x '*)) $2)
                                       $1)
                      (caar id-param)
                      (cdr id-param)
                      $1-start-pos
                      (cdar id-param)))))
    ;; 関数名と引数の宣言
    (function-declarator
     ; <identifier> ( <parameter-type-list-opt> )
     ((ID LPAR parameter-type-list-opt RPAR) `(,(cons (cons $1 $1-start-pos) $3)))
     ; * <function-declarator>
     ((* function-declarator) (append '(*) $2)))
    ;; 関数の定義
    (function-definition
     ; <type-specifier> <function-declarator> <compound-statement>
     ((type-specifier function-declarator compound-statement)
      (let ((id-param (list-ref $2 (- (length $2) 1))))
        (stx:fun-def
         (append (filter (lambda (x) (eq? x '*)) $2) $1)
         (caar id-param)
         (cdr id-param)
         $3
         $1-start-pos
         (cdar id-param)))))
    ;; 引数の宣言のリストのオプショナル
    (parameter-type-list-opt
     ;
     (() '())
     ; <parameter-type-list>
     ((parameter-type-list) $1))
    ;; 引数の宣言のリスト
    (parameter-type-list
     ; <parameter-declaration>
     ((parameter-declaration) `(,$1))
     ; <parameter-type-list> , <parameter-declaration>
     ((parameter-type-list COMMA parameter-declaration)
      (append $1 `(,$3))))
    ;; 引数の宣言
    (parameter-declaration
     ; <type-specifier> <parameter-declarator>
     ((type-specifier parameter-declarator)
      (cons (cons (append (filter (lambda (x) (eq? x '*)) $2) $1)
                  $1-start-pos)
            (list-ref $2 (- (length $2) 1)))))
    ;; 引数の名前
    (parameter-declarator
     ; <identifier>
     ((ID) `(,(cons $1 $1-start-pos)))
     ; * <parameter-declarator>
     ((* parameter-declarator) (append '(*) $2)))
    ;; 型
    (type-specifier
     ; int
     ((INT) 'int)
     ; float
     ((FLOAT) 'float)
     ; void
     ((VOID) 'void))
    ;; 文
    (statement
     ; ;
     ((SEMI) '())
     ; <expression>
     ((expression SEMI) $1)
     ; <compound-statement>
     ((compound-statement) $1)
     ; if ( <expression> ) <statement>
     ((IF LPAR expression RPAR statement)
      (stx:if-stmt $3
                   (if (stx:cmpd-stmt? $5) $5
                       (stx:cmpd-stmt $5))
                   (stx:cmpd-stmt '())
                   $1-start-pos))
     ; if ( <expression> ) <statement> else <statement>
     ((IF LPAR expression RPAR statement ELSE statement)
      (stx:if-stmt $3
                   (if (stx:cmpd-stmt? $5) $5
                       (stx:cmpd-stmt $5))
                   (if (stx:cmpd-stmt? $7) $7
                       (stx:cmpd-stmt $7))
                   $3-start-pos))
     ; while ( <expression> ) <statement>
     ((WHILE LPAR expression RPAR statement)
      (stx:while-stmt $3 $5 $3-start-pos))
     ; do statement while ( expression );
     ((DO statement WHILE LPAR expression RPAR SEMI)
      (stx:cmpd-stmt $2 (stx:while-stmt $5 $2 $5-start-pos)))
     ; for ( <expression-opt> ; <expression-opt> ; <expression-opt> ) <statement>
     ((FOR LPAR expression-opt SEMI expression-opt
           SEMI expression-opt RPAR statement)
      (stx:cmpd-stmt (list $3 (stx:while-stmt $5 (stx:cmpd-stmt (append (if (stx:cmpd-stmt? $9) (stx:cmpd-stmt-stmts $9) `(,$9)) `(,$7))) $1-start-pos))))
     ; return <expression-opt> ;
     ((RETURN expression-opt SEMI) (stx:return-stmt $2 $1-start-pos)))
    ;; 複文
    (compound-statement
     ; { <declaration-list-opt> <statement-list-opt> }
     ((LBRA declaration-list-opt statement-list-opt RBRA)
      (stx:cmpd-stmt (append (stx:cmpd-stmt-stmts $2) (stx:cmpd-stmt-stmts $3)))))
    ;; 宣言のリストのオプショナル
    (declaration-list-opt
     ;
     (() (stx:cmpd-stmt '()))
     ; <declaration-list>
     ((declaration-list) $1))
    ;; 宣言のリスト
    (declaration-list
     ; <declaration>
     ((declaration)
      (stx:cmpd-stmt (if (list? $1) `(,@$1) `(,$1))))
     ; <declaration-list> <declaration>
     ((declaration-list declaration)
      (stx:cmpd-stmt
       (if (list? $2)
           (append (stx:cmpd-stmt-stmts $1) `(,@$2))
           (append (stx:cmpd-stmt-stmts $1) `(,$2))))))
    ;; 文のリストのオプショナル
    (statement-list-opt
     ;
     (() (stx:cmpd-stmt '()))
     ; <statement-list>
     ((statement-list) $1))
    ;; 文のリスト
    (statement-list
     ; <statement>
     ((statement) (stx:cmpd-stmt (if (stx:cmpd-stmt? $1) (stx:cmpd-stmt-stmts $1) `(,$1)))) ;; `(,@(...))
     ; <statement-list> <statement>
     ((statement-list statement)
      (stx:cmpd-stmt (if (stx:cmpd-stmt? $2)
                         (append (stx:cmpd-stmt-stmts $1) (stx:cmpd-stmt-stmts $2)) ;; `(,@(...))
                         (append (stx:cmpd-stmt-stmts $1) `(,$2))))))
    ;; 式のオプショナル
    (expression-opt
     ; 
     (() '())
     ; <expression>
     ((expression) $1))
    ;; 式
    (expression
     ; <assign-expr>
     ((assign-expr) $1)
     ; <assign-expr> , <assign-expr>
     ((expression COMMA assign-expr)
      (stx:comma-exp $1 $3 $2-start-pos)))
    ;; 代入演算
    (assign-expr
     ; <logical-or-expr>
     ((logical-or-expr)
      (cond ((front-inct-exp? $1)
             (let ((inctop (front-inct-exp-op $1))
                   (inctvar (front-inct-exp-var $1))
                   (inctpos (front-inct-exp-pos $1)))
               (stx:assign-exp inctvar (stx:aop-exp inctop inctvar 1 inctpos) $1-start-pos inctpos)))
            ((back-inct-exp? $1)
             (let ((inctop (back-inct-exp-op $1))
                   (inctvar (back-inct-exp-var $1))
                   (inctpos (back-inct-exp-pos $1)))
               (stx:assign-exp inctvar (stx:aop-exp inctop inctvar 1 inctpos) $1-start-pos inctpos)))
            (else $1)))
     ; <logical-or-expr> = <assign-expr>
     ((logical-or-expr = assign-expr)
      (cond ((front-inct-exp? $3)
             (let ((inctop (front-inct-exp-op $3))
                   (inctvar (front-inct-exp-var $3))
                   (inctpos (front-inct-exp-pos $3)))
               `(,(stx:assign-exp inctvar (stx:aop-exp inctop inctvar 1 inctpos) $1-start-pos inctpos)
                 ,(stx:assign-exp $1 inctvar $1-start-pos $2-start-pos))))
            ((back-inct-exp? $3)
             (let ((inctop (back-inct-exp-op $3))
                   (inctvar (back-inct-exp-var $3))
                   (inctpos (back-inct-exp-pos $3)))
               `(,(stx:assign-exp $1 inctvar $1-start-pos $2-start-pos)
                 ,(stx:assign-exp inctvar (stx:aop-exp inctop inctvar 1 inctpos) $1-start-pos inctpos))))
            (else (stx:assign-exp $1 $3 $1-start-pos $2-start-pos))))
     ; <logical-or-expr> += <assign-expr>
     ((logical-or-expr += assign-expr)
      (cond ((front-inct-exp? $3)
             (let ((inctop (front-inct-exp-op $3))
                   (inctvar (front-inct-exp-var $3))
                   (inctpos (front-inct-exp-pos $3)))
               `(,(stx:assign-exp inctvar (stx:aop-exp inctop inctvar 1 inctpos) $1-start-pos inctpos)
                 ,(stx:assign-exp $1 (stx:aop-exp '+ $1 inctvar $2-start-pos) $1-start-pos $2-start-pos))))
            ((back-inct-exp? $3)
             (let ((inctop (back-inct-exp-op $3))
                   (inctvar (back-inct-exp-var $3))
                   (inctpos (back-inct-exp-pos $3)))
             `(,(stx:assign-exp $1 (stx:aop-exp '+ $1 inctvar $2-start-pos) $1-start-pos $2-start-pos)
               ,(stx:assign-exp inctvar (stx:aop-exp inctop inctvar 1 inctpos) $1-start-pos inctpos))))
            (else (stx:assign-exp $1 (stx:aop-exp '+ $1 $3 $2-start-pos) $1-start-pos $2-end-pos))))
     ; <logical-or-expr> -= <assign-expr>
     ((logical-or-expr -= assign-expr)
      (cond ((front-inct-exp? $3)
             (let ((inctop (front-inct-exp-op $3))
                   (inctvar (front-inct-exp-var $3))
                   (inctpos (front-inct-exp-pos $3)))
               `(,(stx:assign-exp inctvar (stx:aop-exp inctop inctvar 1 inctpos) $1-start-pos inctpos)
                 ,(stx:assign-exp $1 (stx:aop-exp '- $1 inctvar $2-start-pos) $1-start-pos $2-start-pos))))
            ((back-inct-exp? $3)
             (let ((inctop (back-inct-exp-op $3))
                   (inctvar (back-inct-exp-var $3))
                   (inctpos (back-inct-exp-pos $3)))
               `(,(stx:assign-exp $1 (stx:aop-exp '- $1 inctvar $2-start-pos) $1-start-pos $2-start-pos)
                 ,(stx:assign-exp inctvar (stx:aop-exp inctop inctvar 1 inctpos) $1-start-pos inctpos))))
            (else (stx:assign-exp $1 (stx:aop-exp '- $1 $3 $2-start-pos) $1-start-pos $2-end-pos)))))
    ;; 論理和演算
    (logical-or-expr
     ; <logical-and-expr>
     ((logical-and-expr) $1)
     ; <logical-or-expr> || <logical-and-expr>
     ((logical-or-expr || logical-and-expr) (stx:log-exp '|| $1 $3 $2-start-pos)))
    ;; 論理積演算
    (logical-and-expr
     ; <equality-expr>
     ((equality-expr) $1)
     ; <logical-and-expr> && <equality-expr>
     ((logical-and-expr && equality-expr) (stx:log-exp '&& $1 $3 $2-start-pos)))
    ;; 比較演算(イコール)
    (equality-expr
     ; <relational-expr>
     ((relational-expr) $1)
     ; <equality-expr> == <relational-expr>
     ((equality-expr == relational-expr) (stx:rop-exp '== $1 $3 $2-start-pos))
     ; <equality-expr> != <relational-expr>
     ((equality-expr != relational-expr) (stx:rop-exp '!= $1 $3 $2-start-pos)))
    ;; 比較演算(不等式)
    (relational-expr
     ; <add-expr>
     ((add-expr) $1)
     ; <relational-expr> < <add-expr>
     ((relational-expr < add-expr) (stx:rop-exp '< $1 $3 $2-start-pos))
     ; <relational-expr> > <add-expr>
     ((relational-expr > add-expr) (stx:rop-exp '> $1 $3 $2-start-pos))
     ; <relational-expr> <= <add-expr>
     ((relational-expr <= add-expr) (stx:rop-exp '<= $1 $3 $2-start-pos))
     ; <relational-expr> >= <add-expr>
     ((relational-expr >= add-expr) (stx:rop-exp '>= $1 $3 $2-start-pos)))
    ;; 可算・減算
    (add-expr
     ; <mult-expr>
     ((mult-expr) $1)
     ; <add-expr> + <mult-expr>
     ((add-expr + mult-expr) (stx:aop-exp '+ $1 $3 $2-start-pos))
     ; <add-expr> - <multi-expr>
     ((add-expr - mult-expr) (stx:aop-exp '- $1 $3 $2-start-pos)))
    ;; 乗算・除算
    (mult-expr
     ; <unary-expr>
     ((unary-expr) $1)
     ; <mult-expr> * <unary-expr>
     ((mult-expr * unary-expr) (stx:aop-exp '* $1 $3 $2-start-pos))
     ; <mult-expr> / <unary-expr>
     ((mult-expr / unary-expr) (stx:aop-exp '/ $1 $3 $2-start-pos)))
    ;; 単項演算
    (unary-expr
     ; <postfix-expr>
     ((postfix-expr) $1)
     ; - <unary-expr>
     ((- unary-expr) (stx:aop-exp '- 0 $2 $1-start-pos))
     ; & <unary-expr>
     ((& unary-expr) (if (stx:deref-exp? $2)
                         (stx:deref-exp-arg $2)
                         (stx:addr-exp $2 $1-start-pos)))
     ; * <unary-expr>
     ((* unary-expr) (stx:deref-exp $2 $1-start-pos))
     ; ++ <unary-expr>
     ((++ unary-expr) (front-inct-exp '+ $2 $1-start-pos))
     ; -- <unary-expr>
     ((-- unary-expr) (front-inct-exp '- $2 $1-start-pos))
     ((LPAR type-specifier RPAR unary-expr) (stx:cast-exp $2 $4 $1-start-pos)))
    ;; 接尾演算
    (postfix-expr
     ; <primary-expr>
     ((primary-expr) $1)
     ; <postfix-expr> [ <expression> ]
     ((postfix-expr LBBRA expression RBBRA)
      (stx:deref-exp (stx:aop-exp '+ $1 $3 $3-start-pos) $1-start-pos))
     ; <identifier> ( <argument-expression-list-opt> )
     ((ID LPAR argument-expression-list-opt RPAR)
      (stx:call-exp $1 $3 $1-start-pos $4-start-pos))
     ; <postfix-expr> ++
     ((postfix-expr ++) (back-inct-exp '+ $1 $2-start-pos))
     ; <postfix-expr> --
     ((postfix-expr --) (back-inct-exp '- $1 $2-start-pos)))
    ;; ID・即値・(式)
    (primary-expr
     ; <identifier>
     ((ID) (stx:var-exp $1 $1-start-pos))
     ; <constant>
     ((NUM) $1)
     ; ( <expression> )
     ((LPAR expression RPAR) $2))
    ;;引数の式のリストのオプショナル
    (argument-expression-list-opt
     ;
     (() '())
     ; <argument-expression-list>
     ((argument-expression-list) $1))
    ;;引数の式のリスト
    (argument-expression-list
     ; <assign-expr>
     ((assign-expr) `(,(cons $1 $1-start-pos)))
     ; <argument-expression> , <assign-expr>
     ((argument-expression-list COMMA assign-expr)
      (append $1 `(,(cons $3 $3-start-pos))))))))

(define (parse-port port)
  (port-count-lines! port)
  (small-c-parser (lambda () (small-c-lexer port))))

;; 文字列を受け取って構文解析
(define (parse-string str)
  (parse-port (open-input-string str)))

;; ファイルを受け取って構文解析
(define (parse-file fname)
  (parse-port (open-input-file fname)))

;; 抽象構文木(実は任意のRacketデータ)を見やすく表示
(define (pretty-print-ast ast)
  (pretty-print ast))
