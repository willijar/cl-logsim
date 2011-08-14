(in-package :logsim)

(build-logic-block
 'fsm
 '((=> X1 (d-flip-flop
        CLK CLK S 0 R RESET
        D (and (not S) (or (and X1 X0) (and (not X0) D)))))
   (=> X0 (d-flip-flop
        CLK CLK S 0 R RESET
        D (or S (and D (not X1)))))
   (=> dormant (and (not X1) (not X0)))
   (=> hit (and X0 (not X1)))
   (=> miss (and (not X0) X1))))

(load-example "case2" :reset nil)

(make-instance 'source :name 'reset :outputs '(R)
               :sequence '((0.1 . 1) (1 . 0)))

(connect #{reset R } #{fsm RESET })

(connect '(#{CLK CLK } #{reset R }) #{trace })