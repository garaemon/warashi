(defsystem warashi
  :depends-on (#:cl-ppcre)
  :components ((:module "src"
                        :components ((:file "warashi")))))
