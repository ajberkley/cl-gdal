;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

(cffi:define-foreign-library libgdal
    (:unix (:or "libgdal.so.20" "libgdal.so"))
  (t (:default "libgdal")))

(cffi:use-foreign-library libgdal)

;; --------------------------------------------------------

(defpackage :cl-gdal
  (:nicknames :gdal)
  (:use :cl)
  (:export #:gdal-all-register
           #:geotransform-type
           #:with-gdal-file
           #:set-geo-transform
           #:get-geo-transform
           #:+out-of-source-range+))

;; EOF
