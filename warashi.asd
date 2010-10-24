(defsystem warashi
  :depends-on (#:cl-ppcre)
  :components ((:module "src"
                        :components ((:file "warashi")
                                     (:file "package-util"
                                            :depends-on ("warashi"))
                                     (:file "util"
                                            :depends-on ("warashi"))
                                     (:file "documentation"
                                            :depends-on ("util" "package-util"))
                                     (:file "api"
                                            :depends-on ("documentation"
                                                         "util"))))))
