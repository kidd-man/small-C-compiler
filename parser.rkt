#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (prefix-in stx: "syntax.rkt")
         parser-tools/yacc
         )
(define ... (void)) ;; indicates a part to be implemented

 
(define-tokens tokens-with-value
  (NUM ID))

(define-empty-tokens tokens-without-value
  (+ - * /
   < <= > >= == !=
   & && || =
   SEMI LPAR RPAR COMMA RETURN 
   LBRA RBRA LBBRA RBBRA
   INT VOID
   IF ELSE WHILE FOR
   EOF))

(define-lex-trans uinteger
  (syntax-rules () ((_ d) (:* d))))

(define-lex-abbrevs
  (digit            (char-range "0" "9"))
  (digit-non-zero   (char-range "1" "9"))
  (number  (:or "0"
                (:: digit-non-zero
                    (uinteger digit))))
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
   ("&"        (token-&))
   ("&&"       (token-&&))
   ("||"       (token-||))
   ("="        (token-=))
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
   ("while"    (token-WHILE))
   ("for"      (token-FOR))
   ("int"      (token-INT))
   ("void"     (token-VOID))
   (number     (token-NUM (string->number lexeme)))
   (identifier (token-ID (string->symbol lexeme)))
   (whitespace (return-without-pos (small-c-lexer input-port)))
   ((eof)      (token-EOF))))
 
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
    (program
     ((external-declaration) `(,$1)) ;====
     ((program external-declaration) (append $1 `(,$2)))) ;====
    (external-declaration
     ((declaration) $1) ;====
     ((function-prototype) $1) ;====
     ((function-definition) $1)) ;====
    (declaration
     ((type-specifier declarator-list SEMI)
      (map (lambda (type_declr)
             (if (cddr type_declr)
                 (stx:declar `(* ,$1) (cadr type_declr))
                 (stx:declar $1       (cadr type_declr))))
           (map (lambda (declr-elem) (cons $1 declr-elem)) $2)))) ;====
    (declarator-list
     ((declarator) `(,$1)) ;====
     ((declarator-list COMMA declarator)
      (append $1 `(,$3)))) ;====
    (declarator
     ((direct-declarator) (cons $1 #f)) ;====
     ((* direct-declarator) (cons (cons $2 $1-start-pos) #t))) ;====
    (direct-declarator
     ((ID) $1) ;====
     ((ID LBBRA NUM RBBRA) (stx:deref-exp (stx:aop-exp '+ $1 $3 $3-start-pos) $1-start-pos))) ;====
    (function-prototype
     ((type-specifier function-declarator SEMI) (stx:function-prottype (cons (if (cdr $2) `(* ,$1) $1) $2-start-pos) (caar $2) (cdar $2)))) ;====
    (function-declarator
     ((ID LPAR parameter-type-list-opt RPAR) (cons (cons (cons $1 $1-start-pos) $3) #f)) ;====
     ((* ID LPAR parameter-type-list-opt RPAR) (cons (cons (cons $2 $2-start-pos) $4) #t))) ;====
    (function-definition
     ((type-specifier function-declarator compound-statement)
      (stx:function-definition (cons (if (cdr $2) `(* ,$1) $1) $2-start-pos) (caar $2) (cdar $2) $3))) ;====
    (parameter-type-list-opt
     (() ...) ;====
     ((parameter-type-list) $1)) ;====
    (parameter-type-list
     ((parameter-declaration) `(,$1)) ;====
     ((parameter-type-list COMMA parameter-declaration)
      (append $1 `(,$3)))) ;====
    (parameter-declaration
     ((type-specifier parameter-declarator) (cons (cons (if (cdr $2) `(* ,$1) $1) $1-start-pos) (car $2)))) ;====
    (parameter-declarator
     ((ID) (cons $1 #f)) ;====
     ((* ID) (cons $2 #t))) ;====
    (type-specifier
     ((INT) 'int) ;====
     ((VOID) 'void)) ;====
    (statement
     ((SEMI) ...) ;====
     ((expression SEMI) $1) ;==== 
     ((compound-statement) $1) ;====
     ((IF LPAR expression RPAR statement)
      (stx:if-stmt $3 $5 (void) $1-start-pos)) ;====
     ((IF LPAR expression RPAR statement ELSE statement)
      (stx:if-stmt $3 $5 $7 $1-start-pos)) ;====
     ((WHILE LPAR expression RPAR statement)
      (stx:while-stmt $3 $5 $1-start-pos)) ;====
     ((FOR LPAR expression-opt SEMI expression-opt
           SEMI expression-opt RPAR statement)
      `(,$3 ,(stx:while-stmt $5 (append (if (list? $9) $9 `(,$9)) `(,$7)) $1-start-pos))) ;====
     ((RETURN expression-opt SEMI) (stx:return-stmt $2 $1-start-pos))) ;====
    (compound-statement
     ((LBRA declaration-list-opt statement-list-opt RBRA)
      (append $2 $3))) ;====
    (declaration-list-opt
     (() '()) ;====
     ((declaration-list) $1)) ;====
    (declaration-list
     ((declaration) `(,$1)) ;====
     ((declaration-list declaration) (append $1 `(,$2)))) ;====
    (statement-list-opt
     (() '()) ;====
     ((statement-list) $1)) ;====
    (statement-list
     ((statement) `(,$1))
     ((statement-list statement) (append $1 `(,$2)))) ;====
    (expression-opt
     (() ...) ;====
     ((expression) $1)) ;====
    (expression
     ((assign-expr) $1) ;====
     ((expression COMMA assign-expr) (append $1 `(,$3)))) ;====
    (assign-expr
     ((logical-or-expr) $1) ;====
     ((logical-or-expr = assign-expr) (stx:assign-exp $1 $3 $2-start-pos))) ;====
    (logical-or-expr
     ((logical-and-expr) $1) ;====
     ((logical-or-expr || logical-and-expr) (stx:log-exp '|| $1 $3 $2-start-pos))) ;====
    (logical-and-expr
     ((equality-expr) $1) ;====
     ((logical-and-expr && equality-expr) (stx:log-exp '&& $1 $3 $2-start-pos))) ;====
    (equality-expr
     ((relational-expr) $1) ;====
     ((equality-expr == relational-expr) (stx:log-exp '== $1 $3 $2-start-pos)) ;====
     ((equality-expr != relational-expr) (stx:log-exp '!= $1 $3 $2-start-pos))) ;====
    (relational-expr
     ((add-expr) $1) ;====
     ((relational-expr < add-expr) (stx:rop-exp '< $1 $3 $2-start-pos)) ;====
     ((relational-expr > add-expr) (stx:rop-exp '> $1 $3 $2-start-pos)) ;====
     ((relational-expr <= add-expr) (stx:rop-exp '<= $1 $3 $2-start-pos));====
     ((relational-expr >= add-expr) (stx:rop-exp '>= $1 $3 $2-start-pos))) ;====
    (add-expr
     ((mult-expr) $1) ;====
     ((add-expr + mult-expr) (stx:aop-exp '+ $1 $3 $2-start-pos)) ;====
     ((add-expr - mult-expr) (stx:aop-exp '- $1 $3 $2-start-pos))) ;====
    (mult-expr
     ((unary-expr) $1) ;====
     ((mult-expr * unary-expr) (stx:aop-exp '* $1 $3 $2-start-pos)) ;====
     ((mult-expr / unary-expr) (stx:aop-exp '/ $1 $3 $2-start-pos))) ;====
    (unary-expr
     ((postfix-expr) $1);====
     ((- unary-expr) (stx:aop-exp '- 0 $2 $1-start-pos)) ;====
     ((& unary-expr) (if (stx:deref-exp? $2) (stx:deref-exp-arg $2) (stx:addr-exp $2 $1-start-pos))) ;====
     ((* unary-expr) (if (stx:addr-exp? $2)  (stx:addr-exp-var $2)  (stx:deref-exp $2 $1-start-pos)))) ;====
    (postfix-expr
     ((primary-expr) $1) ;====
     ((postfix-expr LBBRA expression RBBRA)
      (stx:deref-exp (stx:aop-exp '+ $1 $3 $3-start-pos) $1-start-pos)) ;====
     ((ID LPAR argument-expression-list-opt RPAR)
      (stx:call-exp $1 $3 $1-start-pos))) ;====
    (primary-expr
     ((ID) $1) ;====
     ((NUM) $1) ;====
     ((LPAR expression RPAR) $2)) ;====
    (argument-expression-list-opt
     (() ...) ;====
     ((argument-expression-list) $1)) ;====
    (argument-expression-list
     ((assign-expr) `(,$1)) ;====
     ((argument-expression-list COMMA assign-expr)
      (append $1 `(,$3))))))) ;====

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
