(in-package :logsim)

(make-instance
 'moore-model
 :name 'fsm
 :no-state-bits 2
 :inputs '(D S)
 :outputs '(DORMANT HIT MISS)
 :state-data
 '((#*00 #*100 ;; DORMANT state
    (#*01 #*01 #*11)
    (#*11 #*10)
    (#*00 #*00))
   (#*01 #*001 ;; MISS state
    (#*01 #*01 #*11 #*10)
    (#*00 #*00))
   (#*11 #*000 ;; single detect after dormant
    (#*01 #*01 #*11)
    (#*10 #*10 #*00))
   (#*10 #*010 ;; HIT state
    (#*01 #*01 #*11)
    (#*10 #*10)
    (#*00 #*00))))

(load-example "case2" :reset nil)