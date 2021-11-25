(defsystem "euler-cl"
  :version "0.1.0"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("str"
               "iterate"
               "gtwiwtg"
               "cl-punch")
  :components ((:module "src"
                :components
                ((:file "main")
                 ;(:file "problem")
                  )))
  :description "Solving Project Euler Problems in CL"
  :in-order-to ((test-op (test-op "euler-cl/tests"))))

(defsystem "euler-cl/tests"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("euler-cl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for euler-cl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
