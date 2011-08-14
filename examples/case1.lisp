(make-instance
 'source
 :name 'PQ
 :outputs '(P Q)
 :sequence ((2.5 . #*00) (1 . #*10) (1 . #*11) (1 . #*10) (1 . #*00)))

(make-instance
 'source
 :name 'PQb
 :outputs '(P Q)
 :sequence ((2.5 . #*00) (1 . #*01) (1 . #*11) (1 . #*10) (1 . #*00)))

(make-instance 'trace-monitor :name 'trace)
(make-clock)

(connect '(#{CLK }       #{PQ P }    #{PQ Q })
         '(#{model CLK } #{model P } #{model Q }))

(connect '(#{CLK } #{model X2 } #{model X1 } #{model X0 }
           #{P } #{Q } #{model Y })
         #{trace })

(schedule 8 #'stop-simulation)
;(start-simulation)


