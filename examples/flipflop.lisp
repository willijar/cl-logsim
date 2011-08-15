(in-package :logsim)

(make-instance 'd-flip-flop :name 'dff)

(make-instance 'source :name 'signals :outputs '(D)
               :sequence (loop :for i :from 0 :upto 20 :collect (random 2)))

(make-instance 'source :name 'reset :outputs '(R)
               :sequence '((0.5 . 1) (1 . 0)))
(make-instance 'trace-monitor :name 'trace)
(make-clock :name 'CLK)

(connect '(#{CLK CLK } #{signals D } #{reset R } #{dff Q } #{dff QBAR })
         #{trace })

(connect '(#{signals D } #{CLK CLK } 0 #{reset R })
         '(#{dff D } #{dff CLK } #{dff s } #{dff r } ))

(schedule 21 #'stop-simulation)
