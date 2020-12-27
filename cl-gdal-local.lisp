(in-package :cl-gdal)

;; DO NOT FORGET TO CALL MAYBE-INITIALIZE-GDAL-OGR

(defparameter *initialized* nil)

(defmacro with-gdal-file ((filevar filename &optional (access (or :ga_readonly :ga_update))) &body body)
  "Bind filevar to the result of gdal-open on filename.  Close on unwind / end of block.  Use of
   the returned gdal-dataset-h must be done single threaded."
  `(let (,filevar)
     (unwind-protect
	  (progn
            (maybe-initialize-gdal-ogr)
            (sb-int:with-float-traps-masked (:invalid) ;; needed for some files!
              (setf ,filevar (gdal-open ,filename ,access)))
	    (assert (not (cffi::null-pointer-p ,filevar)) nil "Failed to open file")
	    ,@body)
       (when (not (cffi::null-pointer-p ,filevar)) (gdal-close ,filevar)))))

(defun gdal-get-raster-statistics* (raster-band &optional (b-approx-ok 0) (bforce 1))
  (cffi:with-foreign-object (pdfMin :double)
    (cffi:with-foreign-object (pdfMax :double)
      (cffi:with-foreign-object (pdfMean :double)
	(cffi:with-foreign-object (pdfStdDev :double)
	  (gdal-get-raster-statistics& raster-band b-approx-ok bforce pdfmin pdfmax pdfmean pdfstddev)
	  (list :min (cffi:mem-ref pdfMin :double)
		:max (cffi:mem-ref pdfMax :double)
		:mean (cffi:mem-ref pdfMean :double)
		:stddev (cffi:mem-ref pdfStdDev :double)))))))

(defun gdal-get-block-size* (hband)
  (cffi:with-foreign-object (x-blk-size :int)
    (cffi:with-foreign-object (y-blk-size :int)
      (gdal-get-block-size hband x-blk-size y-blk-size)
      (list (cffi:mem-ref x-blk-size :int) (cffi:mem-ref y-blk-size :int)))))

(defun gdal-get-actual-block-size* (hband x-blk-idx y-blk-idx)
  (cffi:with-foreign-object (x-blk-size :int)
    (cffi:with-foreign-object (y-blk-size :int)
      (gdal-get-actual-block-size hband x-blk-idx y-blk-idx x-blk-size y-blk-size)
      (list (cffi:mem-ref x-blk-size :int) (cffi:mem-ref y-blk-size :int)))))
  
(defun gdal-read-all-data (hband)
  (assert (eq (gdal-get-raster-data-type hband) :gdt_float64))
  (let* ((x-size (gdal-get-raster-band-x-size hband))
	 (y-size (gdal-get-raster-band-y-size hband)))
    (destructuring-bind (x-blk-size y-blk-size)
	(gdal-get-block-size* hband)
      (assert (= x-blk-size x-size))
      (let ((array (make-array (list (* y-blk-size (ceiling y-size y-blk-size)) x-size)
                               :element-type 'double-float)))
        ;;(assert (zerop (nth-value 1 (truncate y-size y-blk-size))))
        ;;(sb-ext::with-pinned-objects (array)
        (cffi:with-pointer-to-vector-data (ptr (sb-kernel:%array-data-vector array))
          (loop :for y-offset :from 0 :below y-size :by y-blk-size
             :do
             (gdal-read-block hband 0 (floor y-offset y-blk-size)
                              (cffi:inc-pointer ptr (* 8 x-size y-offset)))))
        array))))

(defun gdal-get-raster-no-data-value* (hband)
  (cffi:with-foreign-object (psuccess :int)
    (let ((result (cl-gdal::gdal-get-raster-no-data-value hband psuccess)))
      (assert (= (cffi:mem-ref psuccess :int) 1))
      result)))

(declaim (inline array-map))
(defun array-map (output-type function &rest arrays)
  "maps the function over the arrays.
   Assumes that all arrays are of the same dimensions.
   Returns a new result array of the same dimension."
  (declare (optimize (speed 3)))
  (declare (type function function))
  (labels ((make-displaced-array (array)
	     (make-array (reduce #'* (array-dimensions array))
			 :displaced-to array :element-type (array-element-type array))))
    (let* ((displaced-arrays (mapcar #'make-displaced-array arrays)))
      (if output-type
	  (let* ((result-array (make-array (array-dimensions (first arrays)) :element-type output-type))
		 (displaced-result-array (make-displaced-array result-array)))
	    (apply #'map-into displaced-result-array function displaced-arrays)
	    result-array)
	  (map nil function displaced-arrays)))))

(defun gdal-get-statistics* (hband)
  ;; Agrees with (gdal-get-raster-statistics* uband))
  ;; stupid slow, but just a lazy check
  (let ((data (gdal-read-all-data hband))
	(res))
    (array-map nil (lambda (x) (push x res) (values)) data)
    (iterate:iter (iterate:for x in res)
		  (iterate:minimizing x into min)
		  (iterate:maximizing x into max)
		  (iterate:summing x into sum)
		  (iterate:finally (return (list :min min :max max :mean (/ sum (length res))))))))

(defun maybe-initialize-gdal-ogr ()
  (when (not *initialized*)
    (gdal-all-register)
    (cl-ogr:ogr-register-all)
    (setf *initialized* t)))



;; (defun create-ogr-layer (OGRDataSourceH name OGRSpatialReferenceH eType &optional (options (cffi:null-pointer)))
;;   (ogr-ds-create-layer OGRDataSourceH name OGRSpatialReferenceH etype options))

(cffi:defcfun ("OSRNewSpatialReference" osr-new-spatial-reference) cl-ogr::ogr-spatial-reference-h
  (wkt :string))

(cffi:defctype ogr-coordinate-transformation :pointer "ogr-coordinate-transformation")

(cffi:defcfun ("OCTNewCoordinateTransformation" make-coordinate-transformation) ogr-coordinate-transformation
  (source cl-ogr::ogr-spatial-reference-h)
  (dest cl-ogr::ogr-spatial-reference-h))

(cffi:defcfun ("OCTTransform" octt-transform-array) :int
  (htransform ogr-coordinate-transformation)
  (count :int)
  (x :pointer)
  (y :pointer)
  (z :pointer))

(defparameter *xa* (make-array 1 :element-type 'double-float))
(defparameter *ya* (make-array 1 :element-type 'double-float))
(defparameter *za* (make-array 1 :element-type 'double-float))

(declaim (type (simple-array double-float (1)) *xa* *ya* *za*))

(declaim (inline transform-point-unthreadsafe))
(defun transform-point-unthreadsafe (coordinate-transformation x y)
  "This is a faster version of transform-point*, but not thread safe"
  (declare (optimize (speed 3)))
 
  (setf (aref *xa* 0) x)
  (setf (aref *ya* 0) y)
  (sb-int::with-pinned-objects (*xa* *ya* *za*)
    (octt-transform-array coordinate-transformation
                          1
                          (sb-kernel::vector-sap *xa*)
                          (sb-kernel::vector-sap *ya*)
                          (sb-kernel::vector-sap *za*))
    (cons (aref *xa* 0) (aref *ya* 0))))

(declaim (inline transform-point-unthreadsafe*))
(defun transform-point-unthreadsafe* (coordinate-transformation x y)
  "This is a faster version of transform-point*, but not thread safe"
  (declare (optimize (speed 3)))
 
  (setf (aref *xa* 0) x)
  (setf (aref *ya* 0) y)
  (sb-int::with-pinned-objects (*xa* *ya* *za*)
    (octt-transform-array coordinate-transformation
                          1
                          (sb-kernel::vector-sap *xa*)
                          (sb-kernel::vector-sap *ya*)
                          (sb-kernel::vector-sap *za*))
    (values (aref *xa* 0) (aref *ya* 0))))

;; No need to pin these objects then... they are immobile
;; (sb-alien:define-alien-variable *xa* (array double-float 1))
;; (sb-alien:define-alien-variable *ya* (array double-float 1))
;; (sb-alien:define-alien-variable *za* (array double-float 1))
;; (defun transform-point-unthreadsafe (coordinate-transformation x y)
;;   "This is a faster version of transform-point*, but not thread safe"
;;   (declare (optimize (speed 3))
;;            (type double-float x y)
;;            (inline octt-transform-array))
;;   (setf (sb-alien:deref *xa* 0) x)
;;   (setf (sb-alien:deref *ya* 0) y)
;;   (octt-transform-array coordinate-transformation
;;                         1 *xa* *ya* *za*)
;;   (cons (sb-alien:deref *xa* 0) (sb-alien:deref *ya* 0)))

(declaim (inline transform-point-array))
(defun transform-point-array (coordinate-transformation x y &optional (z (make-array (length x) :element-type 'double-float)))
  (declare (type (simple-array double-float (*)) x y z))
  (sb-int::with-pinned-objects (x y z)
    (octt-transform-array coordinate-transformation
                          (length x)
                          (sb-kernel::vector-sap x)
                          (sb-kernel::vector-sap y)
                          (sb-kernel::vector-sap z)))
  (values x y)) ;; do we need the zs?


(defun transform-point* (coordinate-transformation x y &optional (z 0d0 z-p))
  (let ((xa (make-array 1 :element-type 'double-float :initial-element x))
	(ya (make-array 1 :element-type 'double-float :initial-element y))
	(za (make-array 1 :element-type 'double-float :initial-element z)))
    (sb-int::with-pinned-objects (xa ya za)
      (octt-transform-array coordinate-transformation
			    1
			    (sb-kernel::vector-sap xa)
			    (sb-kernel::vector-sap ya)
			    (sb-kernel::vector-sap za))
      (if z-p
	  (list (aref xa 0) (aref ya 0) (aref za 0))
	  (list (aref xa 0) (aref ya 0))))))

(cffi:defcfun ("OGR_DS_CreateLayer" ogr-ds-create-layer) cl-ogr::ogr-layer-h
  "This function attempts to create a new layer on the data source with
 the indicated name, coordinate system, geometry type.

 The papszOptions argument can be used to control driver specific
 creation options. These options are normally documented in the format
 specific documentation.

 This function is the same as the C++ method
 OGRDataSource::CreateLayer().

Parameters:	hDS 	The dataset handle.
	pszName 	the name for the new layer. This should ideally not match any existing layer on the datasource.
	hSpatialRef 	handle to the coordinate system to use for the new layer, or NULL if no coordinate system is available.
	eType 	the geometry type for the layer. Use wkbUnknown if there are no constraints on the types geometry to be written.
	papszOptions 	a StringList of name=value options. Options are driver specific, and driver information can be found at the following url: http://www.gdal.org/ogr/ogr_formats.html

 @return{NULL is returned on failure, or a new OGRLayer handle on success.}

Example:
...
@begin{pre}
        OGRLayerH *hLayer;
        char     **papszOptions;

        if( OGR_DS_TestCapability( hDS, ODsCCreateLayer ) )
        (
            ...
        )

        papszOptions = CSLSetNameValue( papszOptions, \"DIM\", \"2\" );
        hLayer = OGR_DS_CreateLayer( hDS, \"NewLayer\", NULL, wkbUnknown,
                                     papszOptions );
        CSLDestroy( papszOptions );

        if( hLayer == NULL )
        (
            ...
        )
@end{pre}"
  (hDS :pointer)			; OGRDataSourceH
  (pszName :string)			; const char *
  (hSpatialRef :pointer)		; OGRSpatialReferenceH
  (eType cl-ogr::ogr-wkb-geometry-type)				; OGR-wkb-Geometry-Type ;; typo here
  (papszOptions (:pointer :string)))

;; (defun test-ogr (fname)
;;   (maybe-initialize-gdal-ogr)
;;   (with-gdal-file (handle "/home/ajb/canadarasp/continental-test/output.grib2")
;;     (let ((projectionref (gdalGetProjectionRef handle)))
;;       (print (list 'pref projectionref))
;;       (let* ((outdriver (cl-ogr:ogr-get-driver-by-name "GEOJson"))
;; 	     (ds (cl-ogr::ogr_dr_createdatasource outdriver fname (cffi:null-pointer))))
;; 	(print (list 'outdriver outdriver))
;; 	(print (list 'ds ds))
;; 	(when (cffi:null-pointer-p ds)
;; 	  (print (cpl-get-last-error-msg)))
;; 	(let* ((spatial-reference (osr-new-spatial-reference projectionref))) ;; ok
;; 	  (print (list 'sr spatial-reference)) ;; success
;; 	  (let ((layer (create-ogr-layer ds "myarrow" spatial-reference :wkb-line-string)))
;; 	    ;; not sure if I should do wkb_multi_line_string
;; 	    (print (list 'layer layer))
;; 	    (let ((feature (ogr-f-create (ogr-l-get-layer-defn layer))))
;; 	      (print (list 'feature feature))
;; 	      (ogr-f-set-field-string feature (ogr-f-get-field-index feature "Name") "MyName")
;; 	      (let ((geometry (ogr-g-create-geometry :wkb-line-string)))
;; 		(print (list 'geometry geometry))
;; 		(ogr-g-set-point-2d geometry 0 0d0 0d0)
;; 		(ogr-g-set-point-2d geometry 1 1d0 1d0)
;; 		(ogr-f-set-geometry feature geometry)
;; 		;;(ogr-g-destroy-geometry geometry)
;; 		(ogr-l-create-feature layer feature)
;; 		(ogr-f-destroy feature)
;; 		(ogr-ds-destroy ds)))))))))

