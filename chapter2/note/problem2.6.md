## one  
通过展开 (add-1 zero) 来获得：  
<pre><code>
(add-1 zero)

(add-1 (lambda (f)
           (lambda (x)
               x)))

((lambda (n)                    ; add-1
     (lambda (f)
         (lambda (x)
             (f ((n f) x)))))
 (lambda (f)                    ; zero
     (lambda (x)
         x)))

(lambda (f)
    (lambda (x)
        (f (
            ((lambda (f)        ; zero
                 (lambda (x)
                     x))
             f)
            x))))

(lambda (f)
    (lambda (x)
        (f ((lambda (x) x)
            x))))

(lambda (f)
    (lambda (x)
        (f x)))
</code></pre>
经过展开得出 one 的定义为：

<pre><code>
(define one
    (lambda (f)
        (lambda (x)
            (f x))))
</code></pre>

## two

可以继续使用 add-1 和刚得到的 one 定义来得到 two 的定义，展开 (add-1 one) 的调用即可：
<pre><code>
(add-1 one)

(add-1 (lambda (f)
           (lambda (x)
               (f x))))

((lambda (n)                    ; add-1
     (lambda (f)
         (lambda (x)
             (f ((n f) x)))))
 (lambda (f)                    ; one
     (lambda (x)
         (f x))))

(lambda (f)
    (lambda (x)
        (f ((
             (lambda (f)        ; one
                 (lambda (x)
                     (f x)))
             f)
            x))))

(lambda (f)
    (lambda (x)
        (f ((lambda (x)
                (f x))
            x))))

(lambda (f)
    (lambda (x)
        (f (f x))))
</code></pre>

经过展开得出 two 的定义为：
<pre><code>
(define two
    (lambda (f)
        (lambda (x)
            (f (f x)))))
</code></pre>

## 加法函数  

通过对比 zero 、 one 和 two 的定义，我们可以发现，它们都接受两个参数 f 和 x   
不同的地方在于函数体内调用 f 的次数：

<pre><code>
(define zero
    (lambda (f)
        (lambda (x)
            x)))            ; 没有 f

(define one
    (lambda (f)
        (lambda (x)
            (f x))))        ; 一个 f 调用

(define two
    (lambda (f)
        (lambda (x)
            (f (f x)))))    ; 两个 f 调用
</code></pre>


因此，我们有理由相信， three 和 four 的定义很可能是：
<pre><code>
(define three
    (lambda (f)
        (lambda (x)
            (f (f (f x))))))        ; 三个 f 调用

(define four
    (lambda (f)
        (lambda (x)
            (f (f (f (f x)))))))    ; 四个 f 调用
</code></pre>  

继续推广这一规则，我们就得出了 Church 计数表示(非负)整数的一般规则：从 zero 的定义开始，每次数值加一时，函数体内都会增加一个(嵌套的) f 函数的调用；当两个 Chruch 数相加时，它们的和就等于累积起两个过程中的 f 调用。

比如说， (+ 3 2) 的计算过程可以展开为：
<pre><code>
(+ 3 2)

(+ (lambda (f)
       (lambda (x)
           (f (f (f x)))))
   (lambda (f)
       (lambda (x)
           (f (f x)))))

; ...

(lambda (f)
    (lambda (x)
        (f (f (f (f (f x)))))))
</code></pre>

根据这个规则，可以写出相应的 Church 计数的加法函数：

<pre><code>
(define +
    (lambda (m)
        (lambda (n)
            (lambda (f)
                (lambda (x)
                    (m f (n f x)))))))
</code></pre>

加法函数接受两个参数 m 和 n ，然后返回一个接受两个参数 f 和 x 的函数，加法函数的函数体内， n 的函数体被表达式 (n f x) 取了出来，然后又在表达式 (m f (n f x)) 中作为函数 m 的第二个函数被调用，从而将 m 和 n 函数体内的 f 调用累积起来（如果有的话），从而形成加法效果。
一个详细的加法展开例子

<pre><code>
(+ 2 1)

((lambda (m)                        ; 展开 +
     (lambda (n)
        (lambda (f)
            (lambda (x)
                (m f (n f x))))))
 2
 1)

((lambda (n)                        ; 将 2 应用到参数 m
     (lambda (f)
        (lambda (x)
            (2 f (n f x)))))
 1)

(lambda (f)                         ; 将 1 应用到参数 n
    (lambda (x)
        (2 f (1 f x))))

(lambda (f)                         ; 展开 1
    (lambda (x)
        (2 f (
              (lambda (f)           ; 1
                  (lambda (x)
                      (f x)))
             f
             x))))

(lambda (f)                         ; 将 f 应用到 参数 f
    (lambda (x)
        (2 f (
              (lambda (x)
                  (f x))
              x))))

(lambda (f)                         ; 将 x 应用到参数 x
    (lambda (x)
        (2 f (f x))))

(lambda (f)                         ; 展开 2
    (lambda (x)
        (
         (lambda (f)                ; 2
             (lambda (x)
                 (f (f x))))
         f
         (f x))))

(lambda (f)                         ; 将 f 应用到 参数 f
    (lambda (x)
        (
         (lambda (x)
            (f (f x)))
         (f x))))

(lambda (f)                         ; 将 (f x) 应用到参数 x ，计算完成
    (lambda (x)
        (f (f (f x)))))
</code></pre>

调用 (+ 2 1) 的计算结果和前面列出的定义 three 完全相同，证明我们定义的 + 函数是正确的。
