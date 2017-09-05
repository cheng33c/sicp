From sicp.readthedoc.io
                +---------------------------------+
global env -->  |                                 |
                |                                 |
                +---------------------------------+
                   ^
            (f 6)  |
                   |
                +------+
                |      |
          E1 -> | n: 6 |
                |      |
                +------+

              (* 6 (f 5))

                +---------------------------------+
global env -->  |                                 |
                |                                 |
                +---------------------------------+
                   ^               ^
            (f 6)  |        (f 5)  |
                   |               |
                +------+        +------+
                |      |        |      |
          E1 -> | n: 6 |  E2->  | n: 5 |
                |      |        |      |
                +------+        +------+

             (* 6 (f 5))      (* 5 (f 4))

                +--------------------------------------------+
global env -->  |                                            |
                |                                            |
                +--------------------------------------------+
                   ^               ^              ^
            (f 6)  |        (f 5)  |       (f 4)  |
                   |               |              |
                +------+        +------+        +------+
                |      |        |      |        |      |
          E1 -> | n: 6 |  E2->  | n: 5 |  E3 -> | n: 4 |
                |      |        |      |        |      |
                +------+        +------+        +------+

             (* 6 (f 5))      (* 5 (f 4))     (* 4 (f 3))

                +----------------------------------------------------------+
global env -->  |                                                          |
                |                                                          |
                +----------------------------------------------------------+
                   ^               ^              ^                ^
            (f 6)  |        (f 5)  |       (f 4)  |         (f 3)  |
                   |               |              |                |
                +------+        +------+        +------+         +------+
                |      |        |      |        |      |         |      |
          E1 -> | n: 6 |  E2->  | n: 5 |  E3 -> | n: 4 |  E4 ->  | n: 3 |
                |      |        |      |        |      |         |      |
                +------+        +------+        +------+         +------+

             (* 6 (f 5))      (* 5 (f 4))     (* 4 (f 3))      (* 3 (f 2))

                +--------------------------------------------------------------------------+
global env -->  |                                                                          |
                |                                                                          |
                +--------------------------------------------------------------------------+
                   ^               ^              ^                ^               ^
            (f 6)  |        (f 5)  |       (f 4)  |         (f 3)  |        (f 2)  |
                   |               |              |                |               |
                +------+        +------+        +------+         +------+        +------+
                |      |        |      |        |      |         |      |        |      |
          E1 -> | n: 6 |  E2->  | n: 5 |  E3 -> | n: 4 |  E4 ->  | n: 3 |  E5 -> | n: 2 |
                |      |        |      |        |      |         |      |        |      |
                +------+        +------+        +------+         +------+        +------+

             (* 6 (f 5))      (* 5 (f 4))     (* 4 (f 3))      (* 3 (f 2))     (* 2 (f 1))


                +------------------------------------------------------------------------------------------+
global env -->  |                                                                                          |
                |                                                                                          |
                +------------------------------------------------------------------------------------------+
                   ^               ^              ^                ^               ^               ^
            (f 6)  |        (f 5)  |       (f 4)  |         (f 3)  |        (f 2)  |        (f 1)  |
                   |               |              |                |               |               |
                +------+        +------+        +------+         +------+        +------+        +------+
                |      |        |      |        |      |         |      |        |      |        |      |
          E1 -> | n: 6 |  E2->  | n: 5 |  E3 -> | n: 4 |  E4 ->  | n: 3 |  E5 -> | n: 2 |  E6 -> | n: 1 |
                |      |        |      |        |      |         |      |        |      |        |      |
                +------+        +------+        +------+         +------+        +------+        +------+

(* 6 (f 5))      (* 5 (f 4))     (* 4 (f 3))      (* 3 (f 2))     (* 2 (f 1))        1










iter:
         +----------+
global   |          |
env -->  |          |
         |          |
         +----------+
            ^
      (f 6) |
            |
        +-------+
        |       |
  E1 -> | n: 6  |
        |       |
        +-------+
        (i 1 1 6)

         +---------------------------+
global   |                           |
env -->  |                           |
         |                           |
         +---------------------------+
            ^               ^
      (f 6) |     (i 1 1 6) |
            |               |
        +-------+        +-------+
        |       |        | p: 1  |
  E1 -> | n: 6  |  E2 -> | c: 1  |
        |       |        | m: 6  |
        +-------+        +-------+
        (i 1 1 6)        (i 1 2 6)


...... 中间部分省略


         +-----------------------------------------------------------------------------------------------------------------------------+
global   |                                                                                                                             |
env -->  |                                                                                                                             |
         |                                                                                                                             |
         +-----------------------------------------------------------------------------------------------------------------------------+
            ^               ^                 ^                ^               ^                 ^                  ^               ^
      (f 6) |     (i 1 1 6) |       (i 1 2 6) |      (i 2 3 6) |     (i 6 4 6) |      (i 24 5 6) |      (i 120 6 6) |   (i 720 7 6) |
            |               |                 |                |               |                 |                  |               |
        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+
        |       |        | p: 1  |        | p: 1  |        | p: 2  |        | p: 6  |        | p: 24 |        | p:120 |        | p:720 |
  E1 -> | n: 6  |  E2 -> | c: 1  |  E3 -> | c: 2  |  E4 -> | c: 3  |  E5 -> | c: 4  |  E6 -> | c: 5  |  E7 -> | c: 6  |  E8 -> | c: 7  |
        |       |        | m: 6  |        | m: 6  |        | m: 6  |        | m: 6  |        | m: 6  |        | m: 6  |        | m: 6  |
        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+
        (i 1 1 6)        (i 1 2 6)        (i 2 3 6)        (i 6 4 6)       (i 24 5 6)       (i 120 6 6)      (i 720 7 6)       720
