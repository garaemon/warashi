;; api.lisp
;;
;; this file provides the toplevel APIs to generate a documentation
;; written by R.Ueda (garaemon)

(in-package :warashi)

(defun package->rsts (package outputdir)
  "generate reStructuredTexts from a package.

 example::

  (package->rsts :warashi \"tmp\")"
  ;; verificate outputdir exists or not
  (unless (probe-directory outputdir)
    (error "~A is not a directory" outputdir))
  ;; force to use pathname
  (let ((outputdir (if (pathnamep outputdir)
                       outputdir
                     (pathname outputdir))))
    (let ((top-rst (merge-pathnames "top.rst" outputdir))
          (functions-rst (merge-pathnames "functions.rst" outputdir))
          (macros-rst (merge-pathnames "macros.rst" outputdir))
          (variables-rst (merge-pathnames "variables.rst" outputdir)))
      (generate-top-documentation top-rst)
      (generate-functions-documentation functions-rst package)
      ;;(generate-macros-documentation macros-rst)
      ;;(generate-variables-documentation variables-rst)
      t)))
