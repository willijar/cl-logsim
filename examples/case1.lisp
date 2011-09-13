;; (make-instance
;;   'source
;;   :name 'PQ
;;   :outputs '(P Q)
;;   :sequence '((2 . #*00) (1 . #*10) (1 . #*11) (1 . #*10) (1 . #*00)))

(make-instance
 'source
 :name 'PQ
 :outputs '(P Q)
 :sequence '((1 . #*11) (1 . #*01) (3 . #*00) (1 . #*11) (1 . #*10)
            (2 . #*00) (1 . #*01) (1 . #*11) (1 . #*10) (1 . #*01) (2 . #*00)))

(make-instance 'trace-monitor :name 'trace)
(make-clock)

(connect '(#{CLK CLK }       #{PQ P }    #{PQ Q })
         '(#{model CLK } #{model P } #{model Q }))

(connect '(#{CLK CLK } #{model X2 } #{model X1 } #{model X0 }
           #{PQ P } #{PQ Q } #{model Y })
         #{trace })

(schedule (duration #{PQ }) #'stop-simulation)
(start-simulation)


