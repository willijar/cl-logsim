(make-instance
 'mealy-model
 :name 'model
 :no-state-bits 3
 :inputs '(P Q)
 :outputs '(Y)
 :state-data
 '((#*000
    (#*000 (#*01 . #*0) (#*11 . #*0) (#*10 . #*0))
    (#*001 (#*00 . #*0)))
   (#*001
    (#*000 (#*01 . #*0) (#*11 . #*0) (#*10 . #*0))
    (#*011 (#*00 . #*0)))
   (#*011
    (#*000 (#*11 . #*0))
    (#*011 (#*00 . #*0))
    (#*111 (#*01 . #*0) (#*10 . #*0)))
   (#*111
    (#*000 (#*01 . #*0) (#*10 . #*0))
    (#*001 (#*00 . #*0))
    (#*110 (#*11 . #*0)))
   (#*110
    (#*000 (#*01 . #*0) (#*11 . #*0) (#*10 . #*1))
    (#*001 (#*00 . #*0)))))

(load-example "case1" :reset nil)

