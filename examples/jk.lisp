(in-package :logsim)

(make-instance 'jk-flip-flop :name 'jkff)

(make-instance 'source :name 'signals :outputs '(J K)
               :sequence '((2 . #*00) (2 . #*01) (2 . #*10) (2 . #*11)))

(make-instance 'source :name 'reset :outputs '(R S)
               :sequence '((0.1 . #*01) (0.9 . #*00) (0.1 . #*10) (0.9 . #*00))
               :periodic t)

(make-instance 'trace-monitor :name 'trace)
(make-clock :name 'CLK)

(connect '(#{CLK CLK } #{signals J } #{signals K } #{reset R } #{reset S } #{jkff Q })
         #{trace })

(connect '(#{signals J } #{signals K } #{CLK CLK } #{reset S } #{reset R })
         '(#{jkff J } #{jkff K } #{jkff CLK } #{jkff s } #{jkff r } ))

(schedule 10 #'stop-simulation)
