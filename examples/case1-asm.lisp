(make-instance
 'asm-model
 :name 'model
 :no-state-bits 3
 :inputs '(P Q)
 :outputs '(Y)
 :state-data
 '((#*000 () (? (or P Q) #*000 #*001))
   (#*001 () (? (or P Q) #*000 #*011))
   (#*011 () (? (xor P Q) #*111 (? (or P Q) #*000 #*001)))
   (#*111 () (? (and P Q) #*110 (? (or P Q) #*000 #*001)))
   (#*110 () (? (and P (not Q)) ((Y) #*000) (? Q #*000 #*001)))))

;(load-example "case-1" :reset nil)

