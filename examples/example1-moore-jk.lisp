(in-package :logsim)

(build-logic-block
 'model
 '((=> Y (and X2 (not X1) X0))
   (=> X0 (jk-flip-flop CLK CLK S 0 R 0
           J (or (and (not X2) (not X1) (not X0) (not P) (not Q))
                 (and X2 (or P Q)))
           K  (or (and X2 (or P Q))
                  (or (and X1 P Q) (and (not X1) (or P Q))) )))
   (=> X1 (jk-flip-flop CLK CLK S 0 R 0
           J (and (not X2) (not X1) X0 (not P) (not Q))
           K (or (and (not X2) P Q) (and X2 (nand X1 X0 P Q)))  ))
   (=> X2 (jk-flip-flop CLK CLK S 0 R 0
           J (and X1 (xor P Q))
           K (nand X1 P (not (xor X0 Q)))))))

(load-example "example1" :reset nil)
(start-simulation)

