(in-package :logsim)

(build-logic-block
 'model
 '((=> Y (and X2 X1 (not X0) P (not Q)))
   (=> X0 (jk-flip-flop CLK CLK S 0 R 0
           J (and (not X0) (not P) (not Q))
           K (or (and (not (xor X2 X1)) (or P Q)) (and X0 P Q)) ))
   (=> X1 (jk-flip-flop CLK CLK S 0 R 0
           J (and X0 (not P) (not Q))
           K (or (and (not X2) X0 P Q) (and X2 (not (and X0 P Q))))  ))
   (=> X2 (jk-flip-flop CLK CLK S 0 R 0
           J (and X1 X0 (xor P Q))
           K (not (and X0 P Q)) ))))

(load-example "example1" :reset nil)
(start-simulation)

