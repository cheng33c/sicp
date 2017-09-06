from sicp.readthedocs.io
          +------------------------------+
global -> |                              |
env       |  x                           |
          +--|---------------------------+
             |           ^
             |           |
             |        +----------+
             |  E1 -> | x: 1     |
             |        | y: 2     |
             |        |          |
             |        | set-x! -----> ...
             |        | set-y! -----> ...
             +--------->dispatch ---> parameters: m
                      |          |    body: (cond ((eq? m 'car) 'car)
                      +----------+                ((eq? m 'cdr) 'cdr)
                                                  ((eq? m 'set-car!) 'set-car!)
                                                  ((eq? m 'set-cdr!) 'set-cdr!)
                                                  (else
                                                    (error "..." m)))

以下是执行定义 (define z (cons x x)) 之后的环境图：

          +-------------------------------------------------------+
global -> |                                                       |
env       |  z                           x                        |
          +--|---------------------------|------------------------+
             |           ^               |          ^
             |           |               |          |
             |           |               |      +----------+
             |           |               |      | x: 1     |
             |           |               |      | y: 2     |
             |           |               |      |          |
             |           |               |      | set-x! -----> ...
             |           |               |      | set-y! -----> ...
             |           |               +------->dispatch ---> parameters: m
             |           |                      |  ^ ^     |    body: ...
             |           |                      +--|-|-----+
             |        +----------+                 | |
             |  E2 -> | x: ------------------------+ |
             |        | y: --------------------------+
             |        |          |
             |        | set-x! -----> ...
             |        | set-y! -----> ...
             +--------->dispatch ---> parameters: m
                      |          |    body: (cond ((eq? m 'car) 'car)
                      +----------+                ((eq? m 'cdr) 'cdr)
                                                  ((eq? m 'set-car!) 'set-car!)
                                                  ((eq? m 'set-cdr!) 'set-cdr!)
                                                  (else
                                                    (error "..." m)))

执行表达式 (set-car! (cdr z) 17) 有以下两个步骤：

    执行 (cdr z) ，返回 x
    执行 (set-car! x 17) ，引发表达式 ((x 'set-car!) 17) 的执行,然后又引发 (set-x! 17) 的执行

最终， x 的 car 部分的值被设置为 17 。

以下是相应的环境图：

          +-------------------------------------------------------+
global -> |                                                       |
env       |  z                           x                        |
          +--|---------------------------|------------------------+
             |           ^               |          ^
             |           |               |          |
             |           |               |      +----------+
             |           |               |      | x: 17    |
             |           |               |      | y: 2     |
             |           |               |      |          |
             |           |               |      | set-x! -----> ...
             |           |               |      | set-y! -----> ...
             |           |               +------->dispatch ---> parameters: m
             |           |                      |  ^ ^     |    body: ...
             |           |                      +--|-|-----+
             |        +----------+                 | |
             |  E2 -> | x: ------------------------+ |
             |        | y: --------------------------+
             |        |          |
             |        | set-x! -----> ...
             |        | set-y! -----> ...
             +--------->dispatch ---> parameters: m
                      |          |    body: (cond ((eq? m 'car) 'car)
                      +----------+                ((eq? m 'cdr) 'cdr)
                                                  ((eq? m 'set-car!) 'set-car!)
                                                  ((eq? m 'set-cdr!) 'set-cdr!)
                                                  (else
                                                    (error "..." m)))

整个求值过程如下：

1 ]=> (load "20-pair.scm")

;Loading "20-pair.scm"... done
;Value: set-cdr!

1 ]=> (define x (cons 1 2))

;Value: x

1 ]=> (define z (cons x x))

;Value: z

1 ]=> (set-car! (cdr z) 17)

;Value: 1                       ; 使用 set! 设置变量时会返回变量的旧值

1 ]=> (car x)

;Value: 17

