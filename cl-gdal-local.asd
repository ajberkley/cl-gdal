;;; -*- mode: lisp; -*-
(defsystem :cl-gdal-local
    :version "2020.12.21"      ; YYYY.MM.DD -- digits to suit the ASDF
    :licence "BSD"
    :description "CL-GDAL-LOCAL is a collection of new features for CL-GDAL"
    :author "Andrew Berkley <ajberkley@gmail.com>"
    :long-description
    "Some additions on top of cl-gdal"
    :serial t
    :components
    ((:file "cl-gdal-local"))
    :depends-on (:cl-gdal
                 :cl-ogr
                 :alexandria
                 :iterate
                 :cffi
		 :trivial-garbage))

;; EOF
