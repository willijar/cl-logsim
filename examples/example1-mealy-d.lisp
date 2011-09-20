(in-package :logsim)

(build-logic-block
 'model
 '((=> Y (and X2 X1 (not X0) P (not Q)))
   (=> X0 (d-flip-flop CLK CLK S 0 R 0
           D
           (or (=> T1 (and (not X2) X1 X0 (nand P Q)))
               (and (not X2) (not X1) (not P) (not Q))
               (and X2 X1 (not P) (not Q)))))
   (=> X1 (d-flip-flop CLK CLK S 0 R 0
           D (or T1 (and (not X2) (not X1) X0 (not P) (not Q))
                 (=> T2 (and X2 X1 X0 P Q)))))
   (=> X2 (d-flip-flop CLK CLK S 0 R 0
           D (or T2  (and (not X2) X1 X0 (xor P Q)))))))

(load-example "case1" :reset nil)
(start-simulation)

