(in-package :logsim)


(defvar input-stream '(#*00 #*01 #*00 #*00 #*10 #*00 #*00 #*01 #*10 #*00 #*00 #*11 #*00 #*00 #*10 #*01 #*00 #*00 #*01 #*10 #*10 #*00 #*10 #*01 #*01 #*00 #*10 #*01 #*10 #*00 #*00 #*10 #*00 #*01 #*01 #*10 #*00))

(defvar expected-output '(#*100 #*001 #*100 #*100 #*000 #*010 #*100 #*001 #*001 #*100 #*100 #*001 #*100 #*100 #*000 #*001 #*100 #*100 #*001 #*001 #*001 #*100 #*000 #*001 #*001 #*100 #*000 #*001 #*001 #*100 #*100 #*000 #*010 #*001 #*001 #*001 #*100))

(make-instance 'source :name 'signals :outputs '(D S) :sequence input-stream)
(make-instance 'trace-monitor :name 'trace)
(make-clock :name 'CLK)

(connect '( #{fsm X1 } #{fsm X0 }
           #{fsm dormant } #{fsm hit } #{fsm miss })
         #{trace })

(connect '(#{signals D } #{signals S } #{CLK CLK })
         '(#{fsm D } #{fsm S } #{fsm CLK }))

(schedule 38 #'stop-simulation)
