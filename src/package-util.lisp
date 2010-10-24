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

(defun enumerate-all-variables (package)
  "enumerate the all variables defined in package"
  (let ((all-symbols (enumerate-all-symbols package)))
    (remove-if-not #'boundp all-symbols)))

(defun enumerate-external-variables (package)
  "enumerate the all external variables defined in package"
  (let ((external-symbols (enumerate-external-symbols package)))
    (remove-if-not #'boundp external-symbols)))

(defun enumerate-internal-variables (package)
  "enumerate the all internal variables defined in package"
  (let ((internal-symbols (enumerate-internal-symbols package)))
    (remove-if-not #'boundp #'internal-symbols)))

(defun enumerate-all-functions (package)
  "enumerate the all functions defined in package"
  (let ((all-symbols (enumerate-all-symbols package)))
    (remove-if-not #'(lambda (s)
                       (and (fboundp s) (not (macro-function s))))
                   all-symbols)))

(defun enumerate-external-functions (package)
  "enumerate the all external variables defined in package"
  (let ((external-symbols (enumerate-external-symbols package)))
    (remove-if-not #'(lambda (s)
                         (and (fboundp s) (not (macro-function s))))
                   external-symbols)))

(defun enumerate-internal-functions (package)
  "enumerate the all internal variabled defined in package"
  (let ((internal-symbols (enumerate-internal-symbols package)))
    (remove-if-not #'(lambda (s)
                       (and (fboundp s) (not (macro-function s))))
                   internal-symbols)))

(defun enumerate-all-macros (package)
  "enumerate the all macrod defined in package"
  (let ((all-symbols (enumerate-all-symbols package)))
    (remove-if-not
     #'(lambda (s)
         (and (fboundp s) (macro-function s)))
     all-symbols)))

(defun enumerate-external-macros (package)
  "enumerate the all external macrod defined in package"
  (let ((external-symbols (enumerate-external-symbols package)))
    (remove-if-not
     #'(lambda (s)
         (and (fboundp s) (macro-function s)))
     external-symbols)))

(defun enumerate-internal-macros (package)
  "enumerate the all internal macrod defined in package"
  (let ((internal-symbols (enumerate-internal-symbols package)))
    (remove-if-not
     #'(lambda (s)
         (and (fboundp s) (macro-function s)))
     internal-symbols)))
