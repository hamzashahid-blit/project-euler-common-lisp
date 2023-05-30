(asdf:defsystem "euler-cl"
  :version "0.1.0"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("alexandria"
               "str"
               "iterate"
               "gtwiwtg"
               "cl-punch")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "main2"))))
  :description "Solving Project Euler Problems in CL"
  :in-order-to ((asdf:test-op (asdf:test-op "euler-cl/tests"))))

(asdf:defsystem "euler-cl/tests"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("euler-cl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for euler-cl"
  :perform (asdf:test-op (op c) (symbol-call :rove :run c)))
