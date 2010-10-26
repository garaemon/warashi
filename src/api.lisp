;; api.lisp
;;
;; this file provides the toplevel APIs to generate a documentation
;; written by R.Ueda (garaemon)

(in-package :warashi)

(defun package->rsts (package outputdir
                      &key (sphinx-conf-py nil)
                           (appendix-rst nil))
  "generate reStructuredTexts from a package.

 example::

  (package->rsts :warashi \"tmp\")"
  ;; verificate outputdir exists or not
  (unless (probe-directory outputdir)
    (error "~A is not a directory" outputdir))
  
  ;; force to use pathname
  (let ((outputdir (if (pathnamep outputdir)
                       outputdir (pathname outputdir))))
    (if sphinx-conf-py
        (let ((sphinx-conf (merge-pathnames "conf.py" outputdir)))
          (generate-sphinx-conf-py sphinx-conf package)))
    (let ((top-rst (merge-pathnames "top.rst" outputdir))
          (functions-rst (merge-pathnames "functions.rst" outputdir))
          (macros-rst (merge-pathnames "macros.rst" outputdir))
          (variables-rst (merge-pathnames "variables.rst" outputdir)))
      (generate-top-documentation top-rst package appendix-rst)
      (generate-functions-documentation functions-rst package)
      (generate-macros-documentation macros-rst package)
      (generate-variables-documentation variables-rst package)
      (values top-rst functions-rst macros-rst variables-rst))))
