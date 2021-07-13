(in-package :cl-gdal)

(cffi:defcenum cpl-err
  (:CE_None 0)
  (:CE_Debug 1)
  (:CE_Warning 2)
  (:CE_Failure 3)
  (:CE_Fatal 4))

(cffi:defcfun ("CPLGetLastErrorMsg" cpl-get-last-error-msg) :string)

(define-condition gdal-error (simple-error) ())

(defmacro handling-cpl-err (&body body)
  (let ((cpl (gensym "CPLERR")))
    `(let ((,cpl (progn ,@body)))
       (unless (eql ,cpl :ce_none)
         (error 'gdal-error :format-control "Error of type ~A: ~A" :format-arguments (list ,cpl (cpl-get-last-error-msg) ))))))

(cffi:defcfun ("CPLFree" cpl-free) :void
  (ref :pointer))
(export 'cpl-free)
