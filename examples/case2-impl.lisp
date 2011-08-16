(in-package :logsim)

(build-logic-block
 'fsm
 '((=> X1 (d-flip-flop CLK CLK S 0 R RESET
            D (and (not S) (or (and X1 X0) (and (not X0) D)))))
   (=> X0 (d-flip-flop CLK CLK S 0 R RESET
            D (or S (and D (not X1)))))
   (=> dormant (and (not X1) (not X0)))
   (=> hit (and X1 (not X0)))
   (=> miss (and (not X1) X0))))

(load-example "case2" :reset nil)

(make-instance 'source :name 'reset :outputs '(R)
               :sequence '((0.4 . 1) (1 . 0)))

(connect #{reset R } #{fsm RESET })
