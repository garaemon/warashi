;;================================================
;; warashi.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(defpackage :warashi
  (:use #:common-lisp))

(in-package :warashi)

;; implemntation compatibility functions
;; copy from chimi

(defun getenv (str)
  "returns environment variable's value as string.

   ;;; (getenv \"HOME\") -> \"/path/to/your/home/directory\""
  #+sbcl
  (sb-posix:getenv str)
  #+allegro
  (sys:getenv str))

(defun main-arg ()
  #+sbcl
  *posix-argv*)

;; global settings
;; directory hierarchy
;;   $HOME/.warashi
;;        +--config
;;        +--source/
;;        |    +--hoge.source
;;        |    +--fuga.source
;;        +--packages/
;;                +--src/
;;                |   +--package-a/
;;                |   |     +--version-1/
;;                |   |     +--version-2/
;;                |   +--package-b/
;;                |         +--version-1/
;;                |         +--version-2/
;;                +--binary/
;;                         +--sbcl/
;;                         +--acl/
;;
;; directories
(defconstant +base-directory+
  (format nil "~A/.warashi" (getenv "HOME")))

(defconstant +package-root-directory+
  (format nil "~A/packages" +base-directory+))

(defconstant +package-src-root-directory+
  (format nil "~A/src" +package-root-directory+))

(defconstant +package-binary-root-directory+
  (format nil "~A/binary" +package-root-directory+))

(defconstant +config-file+
  (format nil "~A/config" +base-directory+))

(defconstant +source-directory+
  (format nil "~A/source" +base-directory+))

;; commands
(defconstant +install+ "install")
(defconstant +uninstall+ "uninstall")
(defconstant +add-source+ "add-source")
(defconstant +del-source+ "del-source")
(defconstant +show+ "show")
(defconstant +help+ "help")

(defconstant +command-list+
  (list +install+
        +uninstall+
        +add-source+
        +del-source+
        +show+
        +help+))




;; parse args
(defun split-args-options/commands (args)
  
  )


(defun parse-args (args)
  (multiple-value-bind (options commands)
      (split-args-options/commands args)
    ))

(defun main-exec (args)
  (let ((params (parse-args args)))
    (param-case
     (params)
     (:help
      )
     )
    ))

;; help functions
(defun usage ()
  (format t "Usage: warashi [options] command~%")
  (format t "~%")
  (format t "Commands:~%")
  (format t "   install - Install new packages~%")
  (format t "   uninstall - Uninstall the packages~%")
  (format t "   show - Show configuration~%")
  (format t "   add-source - Add source repository~%")
  (format t "   del-source - Delete source repository~%")
  (format t "   help - print details about command~%")
  )

;; main
(defun main ()
  (let ((arg (cdr (main-arg))))
    (if (null arg)
        (usage)
        (main-exec arg))))
