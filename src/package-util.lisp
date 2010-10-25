;; package-util.lisp
;; 
;; wrriten by R.Ueda (garaemon)

(in-package #:warashi)

(defun enumerate-all-symbols (package)
  "enumerate the all symbols defined in package"
  (let ((ret nil))                      ;mutable
    (do-symbols (s package)
      (multiple-value-bind (sym status)
          (find-symbol (symbol-name s) package)
        (if (or (eq status :internal)
                (eq status :external))
            (push s ret))))
    (reverse ret)))

(defun enumerate-external-symbols (package)
  "enumerate the all externed symbols defined in package"
  (let ((ret nil))                      ;mutable
    (do-symbols (s package)
      (multiple-value-bind (sym status)
          (find-symbol (symbol-name s) package)
        (if (eq status :external)
            (push s ret))))
    (reverse ret)))

(defun enumerate-internal-symbols (package)
  "enumerate the all internal symbols defined in package"
  (let ((ret nil))                      ;mutable
    (do-symbols (s package)
      (multiple-value-bind (sym status)
          (find-symbol (symbol-name s) package)
        (if (eq status :internal)
            (push s ret))))
    (reverse ret)))

(defun enumerate-symbols (package
                          enumerate-function
                          &key
                          (filter nil))
  (let ((syms (funcall enumerate-function package)))
    (if filter
        (remove-if-not filter syms)
        syms)))

(defun enumerate-all-variables (package)
  "enumerate the all variables defined in package"
  (enumerate-symbols package #'enumerate-all-symbols :filter #'boundp))

(defun enumerate-external-variables (package)
  "enumerate the all external variables defined in package"
  (enumerate-symbols package #'enumerate-external-macros :filter #'boundp))

(defun enumerate-internal-variables (package)
  "enumerate the all internal variables defined in package"
  (enumerate-symbols package #'enumerate-internal-macros :filter #'boundp))

(defun enumerate-all-functions (package)
  "enumerate the all functions defined in package"
  (enumerate-symbols package #'enumerate-all-symbols
                     :filter #'(lambda (s)
                                 (and (fboundp s) (not (macro-function s))))))

(defun enumerate-external-functions (package)
  "enumerate the all external variables defined in package"
  (enumerate-symbols package #'enumerate-external-symbols
                     :filter #'(lambda (s)
                                 (and (fboundp s) (not (macro-function s))))))

(defun enumerate-internal-functions (package)
  "enumerate the all internal variabled defined in package"
  (enumerate-symbols package #'enumerate-internal-symbols
                     :filter #'(lambda (s)
                                 (and (fboundp s) (not (macro-function s))))))

(defun enumerate-all-macros (package)
  "enumerate the all macrod defined in package"
  (enumerate-symbols package #'enumerate-all-symbols
                     :filter #'(lambda (s)
                                 (and (fboundp s) (macro-function s)))))

(defun enumerate-external-macros (package)
  "enumerate the all external macrod defined in package"
  (enumerate-symbols package #'enumerate-external-symbols
                     :filter #'(lambda (s)
                                 (and (fboundp s) (macro-function s)))))

(defun enumerate-internal-macros (package)
  "enumerate the all internal macrod defined in package"
  (enumerate-symbols package #'enumerate-internal-symbols
                     :filter #'(lambda (s)
                                 (and (fboundp s) (macro-function s)))))
