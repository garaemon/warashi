;;================================================
;; warashi-exec.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

#+sbcl
(require :sb-posix)

(require :warashi "./warashi.lisp")

(defpackage :warashi-exec
  (:use #:common-lisp))

(in-package :warashi-exec)

(defun symbol->keyword (sym)
  "convert a symbol to keyword.
   this function is very slow, because it uses read-from-string.
  ;;; (symbol->keyword 'hoge) -> :hoge"
  (declare (type symbol sym))
  (read-from-string (concatenate 'string ":" (string sym))))

(defun string->symbol (str)
  "convert string to symbol.
   this function is very slow, because it uses read-from-string.
  ;;; (string->symbol \"hoge\") -> hoge"
  (declare (type string str))
  (read-from-string str))

(defmacro defclass* (class-name supers slots &rest args)
  "defclass* is a rich wrapper of defclass.
   defclass* automatically define accessor and initarg.

   ;;; (defclass* <hoge> () ((a 1) (b 2))) -> <hoge>
   ;;; (defclass* <fuga> () ((a 100) (B 'piyo))
   ;;;     (:documentation .....))
   "
  `(defclass ,class-name
       ,supers
     ,(mapcar
       #'(lambda (x)
	   (if (atom x)
	       (list x :initform nil
		     :initarg (symbol->keyword x)
		     :accessor (string->symbol
                                (concatenate 'string
                                             (string x)
                                             "-of")))
	       (list (car x) :initform (cadr x)
		     :initarg (symbol->keyword (car x))
		     :accessor (string->symbol
                                (concatenate 'string
                                             (string (car x))
                                             "-of")))))
       slots)
     ,@args))

(defun flatten (lst)
  "flatten a list.

   ;;; (flatten '((1 2) (3 4))) -> (1 2 3 4)"
  (cond ((null lst)
         nil)
        ((atom lst)
         (list lst))
        (t                              ;lst = list
         (append (flatten (car lst))
                 (flatten (cdr lst))))))

(defun main-arg ()
  #+sbcl
  sb-ext:*POSIX-ARGV*)

(defvar *command-list* nil)

;; commands
(defclass* <command>
    ()
  ((name nil)
   (simple-help nil)
   (args-help nil)
   (described-help nil)
   (exec-func nil)))

(defgeneric format-usage (c))
(defmethod format-usage ((c <command>))
  (format t "   ~A - ~A~%"
          (name-of c)
          (simple-help-of c)))

(defgeneric format-description (c))
(defmethod format-description ((c <command>))
  (format t "Command: ~A~%" (name-of c))
  (format t "~%")
  (format t "Arguments:~% ~A~%" (args-help-of c))
  (format t "~%")
  (format t "Description:~% ~A~%" (described-help-of c)))

;; help functions
(defun usage ()
  (format t "Usage: warashi [options] command~%")
  (format t "~%")
  (format t "Commands:~%")
  (dolist (c *command-list*)
    (format-usage c)))

(defmacro defcommand (sym
                      name
                      simple-description
                      args-help
                      description
                      &optional (exec-func nil))
  `(progn
     (defconstant ,sym
       (make-instance '<command>
                      :name ,name
                      :simple-help ,simple-description
                      :args-help ,args-help
                      :described-help ,description
                      :exec-func ,exec-func))
     (push ,sym *command-list*)
     ,sym))

(defcommand +install+ "install"
  "Install new packages"
  "package-a package-b ..."
  "Install the new packages. The packages are searched from source.
You can specify version of the packages by -v option."
  )

(defcommand +uninstall+ "uninstall"
  "Uninstall the packages"
  "package-a package-b ..."
  "Remove the installed packages.
You can specify version of the packages by -v option."
  )
  

(defcommand +add-source+ "add-source"
  "Add source repository"
  "source-url"
  "Add another source repository.")

(defcommand +del-source+ "del-source"
  "Delete source repository"
  "source-url"
  "Delete source repository.")

(defcommand +show+ "show"
  "Show configuration"
  "source|installed|list|"
  "")

;; help
(defun help-command (command &rest args)
  (declare (ignore args))
  ;;command = string
  (let ((target-command (find-if
                         #'(lambda (c)
                             (string= (name-of c) command))
                         *command-list*)))
    (if (null target-command)
        (progn
          (format t "command<~A> is not supported~%" command)
          (usage))
        (format-description target-command))))

(defcommand +help+ "help"
  "print details about command"
  "command"
  "print details about command"
  #'help-command)

;; parse args
(defun decompose-options (option)
  "option := \"-abcd\" => (\"a\" \"b\" \"c\" \"d\")
   option := \"--help\" => (\"help\")"
  (let ((double-minus-p (char= (elt option 1) #\-)))
    (let ((strs (string-left-trim "-" option)))
      (if double-minus-p
          strs
          (mapcar #'(lambda (x) (format nil "~a" x)) (coerce strs 'cons))))))

(defun split-args-options/commands (args)
  (labels ((optionp (arg)
             (if (= (length arg) 0)
                          nil
                          (char= (elt arg 0) #\-))))
    (let ((options (remove-if-not #'optionp args))
          (commands (remove-if #'optionp args)))
      (values (remove-duplicates (flatten (mapcar #'decompose-options options))
                                 :test #'string=)
              commands))))

(defun setup-options (options)
  (format t "options:=> ~A~%" options)
  (format t ";; currently not supported options~%"))

(defun main-exec (args)
  (multiple-value-bind (options commands)
      (split-args-options/commands args)
    (setup-options options)
    (let ((arg (cdr commands))
          (command (car commands)))
      (dolist (c *command-list*)
        (when (string= command
                     (name-of c))
          (if (exec-func-of c)
              (apply (exec-func-of c) arg)
              (progn
                (format t "not implemented~%")))
          (return-from main-exec t)))
      ;; error
      (usage))))

;; main
(defun main ()
  (let ((arg (cdr (main-arg))))
    (if (null arg)
        (usage)
        (main-exec arg))))

(main)

