#lang planet neil/sicp

;; 取了 `''` 所以输出了 `quote`
(car '' abracadabra)

;;  比如这个例子 返回了 `quote`
(car '' "")

;; 这个例子就返回了 `(mcons 'quote (mcons "" '()))`
'' ""