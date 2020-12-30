(in-package :cl-gdal)

(cffi:define-foreign-type geotransform-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser geotransform))

(deftype geotransform () `(simple-array double-float (6)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl ;; sbcl with-pointer-to-vector-data works with simple-array types not just ub8
  (defmethod cffi:expand-to-foreign-dyn (value var body (type geotransform-type))
    `(progn
       (locally
           (declare (type geotransform ,value))
         (cffi:with-pointer-to-vector-data (,var ,value)
           ,@body))))

  #-sbcl ;; copy version (easier with the normal generic interface, but whatever)
  (defmethod cffi:expand-to-foreign-dyn (value var body (type geotransform-type))
    (let ((foreign-array (gensym "GEOTRANSFORM")))
      `(progn
         (locally 
             (declare (type geotransform ,value))
           (cffi:with-foreign-object (,foreign-array :double 6)
             (dotimes (idx 6)
               (setf (cffi:mem-aref ,foreign-array :double idx) (aref ,value idx)))
             (let ((,var (cffi:mem-aptr ,foreign-array :double 0)))
               (multiple-value-prog1
                   ,@body
                 (dotimes (idx 6)
                   (setf (aref ,value idx) (cffi:mem-aref ,foreign-array :double idx)))))))))))

(cffi:defcfun ("GDALGetGeoTransform" GDALGetGeoTransform) cpl-err
  "Fetch the affine transformation coefficients."
  (hDS gdal-dataset-h)
  (padfTransform geotransform))

(cffi:defcfun ("GDALSetGeoTransform" GDALSetGeoTransform) cpl-err
  "Set the affine transformation coefficients."
  (hDS gdal-dataset-h)
  (padfTransform geotransform))

(define-condition gdal-error (simple-error) ())

(defmacro handling-cpl-err (&body body)
  (let ((cpl (gensym "CPLERR")))
    `(let ((,cpl (progn ,@body)))
       (unless (eql ,cpl :ce_none)
         (error 'gdal-error :format-control "Error of type ~A: ~A" :format-arguments (list ,cpl (cpl-get-last-error-msg) ))))))

(defun set-geo-transform (hdriver array)
  "GDALSetGeoTransform copies the coefficients."
  (declare (type geotransform array))
  (GDALSetGeoTransform hdriver array))

(defun get-geo-transform (dshandle)
  (let ((a (make-array 6 :element-type 'double-float)))
    (handling-cpl-err (gdalgetgeotransform dshandle a))
    a))

(defun make-geo-transformer (hband)
  "For a raster band HBAND, generate a function that converts from pixel indices
   (where x,y = 0,0 in the upper left of the raster band), return a lambda which will return
   the equivalent pixel location in source geo ref units (could be lat, lon, etc)."
  (let ((gt (get-geo-transform hband)))
    (declare (type geotransform gt))
    (values 
     (lambda (x-idx y-idx) ;; pixel indices, from 0
       (let ((center-x (+ x-idx 0.5))
	     (center-y (+ y-idx 0.5)))
	 (list (+ (aref gt 0) (* (aref gt 1) center-x) (* (aref gt 2) center-y))
	       (+ (aref gt 3) (* (aref gt 4) center-x) (* (aref gt 5) center-y))))))))

(alexandria:define-constant +google-map-wkt+
  "GEOGCS[\"WGS 84\",
    DATUM[\"WGS_1984\",
        SPHEROID[\"WGS 84\",6378137,298.257223563,
            AUTHORITY[\"EPSG\",\"7030\"]],
        AUTHORITY[\"EPSG\",\"6326\"]],
    PRIMEM[\"Greenwich\",0,
        AUTHORITY[\"EPSG\",\"8901\"]],
    UNIT[\"degree\",0.01745329251994328,
        AUTHORITY[\"EPSG\",\"9122\"]],
    AUTHORITY[\"EPSG\",\"4326\"]]" :test #'string=)

(deftype pixelidx () 'fixnum)

(defun generate-lon-lat-to-pixel-transform-opt (gdal-dataset-h)
  (declare (optimize (speed 3)))
  "Returns (lambda (lon lat) -> pixelx, pixely)

   Remember pixel 0 0 is the upper left of the image... so the bottom
   right of a 100x100 pixel image is actually at pixel 100x100 (the
   bottom right of the last pixel at (99,99))."
  (let* ((x-size (gdal-get-raster-band-x-size (gdal-get-raster-band gdal-dataset-h 1)))
	 (y-size (gdal-get-raster-band-y-size (gdal-get-raster-band gdal-dataset-h 1)))
	 (my-projection (osr-new-spatial-reference (gdalGetProjectionRef gdal-dataset-h)))
	 (google-projection (osr-new-spatial-reference +google-map-wkt+))
	 (itransform (make-coordinate-transformation google-projection my-projection))
	 (ftransform (make-coordinate-transformation my-projection google-projection))
	 (source-geo-transform (get-geo-transform gdal-dataset-h))
	 (ulx (aref source-geo-transform 0))
	 (lry (+ (aref source-geo-transform 3) (* x-size (aref source-geo-transform 4)) (* y-size (aref source-geo-transform 5))))
	 (lrx (+ (aref source-geo-transform 0) (* x-size (aref source-geo-transform 1)) (* y-size (aref source-geo-transform 2))))
	 (uly (aref source-geo-transform 3)))
    (declare (type fixnum x-size y-size)
	     (type double-float ulx lry lrx uly)
	     (type geotransform source-geo-transform))
    (labels (;; (from-source-pixel-to-source-geo-ref (pixelx pixely)
             ;;   (declare (type pixelidx pixelx pixely))
	     ;;   (list (+ (aref source-geo-transform 3) (* pixelx (aref source-geo-transform 4)) (* pixely (aref source-geo-transform 5)))
	     ;;         (+ (aref source-geo-transform 0) (* pixelx (aref source-geo-transform 1)) (* pixely (aref source-geo-transform 2)))))
	     (from-source-geo-ref-to-source-pixel (x y)
               (declare (type double-float x y))
	       (let ((xpixel (* x-size (/ (- x ulx) (- lrx ulx)))) ;; can precompute lrx - ulx lry - uly
		     (ypixel (* y-size (/ (- y uly) (- lry uly)))))
                 (declare (type (double-float -1d16 1d16) xpixel ypixel))
		 (values (round xpixel) (round ypixel))))
	     ;; (from-source-geo-ref-to-lon-lat (georefx georefy)
	     ;;   (transform-point* ftransform georefy georefx))
	     ;; (from-source-pixel-to-lon-lat (pixelx pixely)
	     ;;   (apply #'from-source-geo-ref-to-lon-lat (from-source-pixel-to-source-geo-ref pixelx pixely)))
	     (from-google-geo-ref-to-source-geo-ref (a b)
               (declare (type double-float a b))
	       (apply #'from-source-geo-ref-to-source-pixel
		(multiple-value-list (transform-point-unthreadsafe* itransform a b)))))
      (destructuring-bind (ullon ullat)
          (transform-point* ftransform ulx uly)
        (destructuring-bind (lrlon lrlat)
            (transform-point* ftransform lrx lry)
          (format t "Corner Coordinates of source file:~%")
          (format t "Upper Left (~,4f, ~,4f) (LON ~,6f, LAT ~,6f) ~%" ulx uly ullon ullat)
          (format t "Lower Right (~,4f, ~,4f) (LON ~,6f, LAT ~,6f)~%" lrx lry lrlon lrlat)
          (format t "Upper Left is ~A~%" (from-source-geo-ref-to-source-pixel ulx uly))
          (format t "Lower Right is ~A~%" (from-source-geo-ref-to-source-pixel lrx lry))))
      (values #'from-google-geo-ref-to-source-geo-ref ;; #'from-source-pixel-to-lon-lat
              ))))

(defun generate-lon-lat-to-pixel-transform (gdal-dataset-h)
  ;;(declare (optimize (speed 3)))
  "Returns (lambda (lon lat) -> pixelx, pixely)

   Remember pixel 0 0 is the upper left of the image... so the bottom
   right of a 100x100 pixel image is actually at pixel 100x100 (the
   bottom right of the last pixel at (99,99))."
  (let* ((x-size (gdal-get-raster-band-x-size (gdal-get-raster-band gdal-dataset-h 1)))
	 (y-size (gdal-get-raster-band-y-size (gdal-get-raster-band gdal-dataset-h 1)))
	 (my-projection (osr-new-spatial-reference (gdalGetProjectionRef gdal-dataset-h)))
	 (google-projection (osr-new-spatial-reference +google-map-wkt+))
	 (itransform (make-coordinate-transformation google-projection my-projection))
	 (ftransform (make-coordinate-transformation my-projection google-projection))
	 (source-geo-transform (get-geo-transform gdal-dataset-h))
	 (ulx (aref source-geo-transform 0))
	 (lry (+ (aref source-geo-transform 3) (* x-size (aref source-geo-transform 4)) (* y-size (aref source-geo-transform 5))))
	 (lrx (+ (aref source-geo-transform 0) (* x-size (aref source-geo-transform 1)) (* y-size (aref source-geo-transform 2))))
	 (uly (aref source-geo-transform 3)))
    (declare (type fixnum x-size y-size)
	     (type double-float ulx lry lrx uly)
	     (type geotransform source-geo-transform))
    (labels ((from-source-pixel-to-source-geo-ref (pixelx pixely)
               (declare (type pixelidx pixelx pixely))
	       (list (+ (aref source-geo-transform 3) (* pixelx (aref source-geo-transform 4)) (* pixely (aref source-geo-transform 5)))
		     (+ (aref source-geo-transform 0) (* pixelx (aref source-geo-transform 1)) (* pixely (aref source-geo-transform 2)))))
	     (from-source-geo-ref-to-source-pixel (ref)
	       (let ((xpixel (* x-size (/ (- (the double-float (car ref)) ulx) (- lrx ulx))))
		     (ypixel (* y-size (/ (- (the double-float (cdr ref)) uly) (- lry uly)))))
		 (list (round xpixel) (round ypixel))))
	     (from-source-geo-ref-to-lon-lat (georefx georefy)
	       (transform-point* ftransform georefy georefx))
	     (from-source-pixel-to-lon-lat (pixelx pixely)
	       (apply #'from-source-geo-ref-to-lon-lat (from-source-pixel-to-source-geo-ref pixelx pixely)))
	     (from-google-geo-ref-to-source-geo-ref (a b)
	       (from-source-geo-ref-to-source-pixel
		(transform-point-unthreadsafe itransform (coerce a 'double-float) (coerce b 'double-float)))))
      (destructuring-bind (ullon ullat)
          (transform-point* ftransform ulx uly)
        (destructuring-bind (lrlon lrlat)
            (transform-point* ftransform lrx lry)
          (format t "Corner Coordinates of source file:~%")
          (format t "Upper Left (~,4f, ~,4f) (LON ~,6f, LAT ~,6f) ~%" ulx uly ullon ullat)
          (format t "Lower Right (~,4f, ~,4f) (LON ~,6f, LAT ~,6f)~%" lrx lry lrlon lrlat)
          (format t "Upper Left is ~A~%" (from-source-geo-ref-to-source-pixel (cons ulx uly)))
          (format t "Lower Right is ~A~%" (from-source-geo-ref-to-source-pixel (cons lrx lry)))))
      (values #'from-google-geo-ref-to-source-geo-ref #'from-source-pixel-to-lon-lat))))

(defconstant +out-of-source-range+ (1- (ash 1 16)))

(declaim (inline angle))
(defun angle (ux uy vx vy)
  (declare (type double-float ux uy vx vy))
  (let ((cross-product (- (* ux vy) (* vx uy)))
        (theta (acos (the (double-float -1d0 1d0) (/ (+ (* ux vx) (* uy vy)) (* (sqrt (+ (* ux ux) (* uy uy))) (sqrt (+ (* vx vx) (* vy vy)))))))))
    (if (plusp cross-product) theta (- theta))))

(defun generate-lon-lat-to-pixel-transform-array (x-size y-size source-spatial-reference source-geo-transform)
  (declare (optimize (debug 3))
           (type fixnum x-size y-size))
  "Returns (lambda (lon-array lat-array) -> pixelx-array, pixely-array, angle-array)
   Remember pixel 0 0 is the upper left of the image... so the bottom
   right of a 100x100 pixel image is actually at pixel 100x100 (the
   bottom right of the last pixel at (99,99)).  The angle-array returns the angle of"
  (let* ((x-size-df (float x-size 0d0))
	 (y-size-df (float y-size 0d0))
	 (my-projection (osr-new-spatial-reference source-spatial-reference))
	 (google-projection (osr-new-spatial-reference +google-map-wkt+))
	 (itransform (make-coordinate-transformation google-projection my-projection))
	 (ftransform (make-coordinate-transformation my-projection google-projection))
	 (ulx (aref source-geo-transform 0))
	 (lry (+ (aref source-geo-transform 3) (* x-size-df (aref source-geo-transform 4)) (* y-size-df (aref source-geo-transform 5))))
	 (lrx (+ (aref source-geo-transform 0) (* x-size-df (aref source-geo-transform 1)) (* y-size-df (aref source-geo-transform 2))))
	 (uly (aref source-geo-transform 3))
         (source-width (- lrx ulx))   ;; (+ (* x-size-df (aref source-geo-transform 1)) (* y-size-df (aref source-geo-transform 2)))
         (source-height (- lry uly))) ;; (+ (* x-size-df (aref source-geo-transform 4)) (* y-size-df (aref source-geo-transform 5)))
    (declare (type double-float x-size-df y-size-df ulx lry lrx uly source-width source-height)
	     (type geotransform source-geo-transform) (type (unsigned-byte 16) x-size y-size))
    (format t "xsize is ~A, source-width is ~A, ysize is ~A, source-height is ~A~%" x-size source-width y-size source-height)
    (labels (;; (from-source-pixel-to-source-geo-ref (pixelx pixely)
	     ;;   (list (+ (aref source-geo-transform 3) (* pixelx (aref source-geo-transform 4)) (* pixely (aref source-geo-transform 5)))
	     ;;         (+ (aref source-geo-transform 0) (* pixelx (aref source-geo-transform 1)) (* pixely (aref source-geo-transform 2)))))
             ;; (from-source-geo-ref-to-lon-lat (georefx georefy)
	     ;;   (transform-point* ftransform georefy georefx))
	     ;; (from-source-pixel-to-lon-lat (pixelx pixely)
	     ;;   (apply #'from-source-geo-ref-to-lon-lat (from-source-pixel-to-source-geo-ref pixelx pixely)))
             (clip/round-to-pixel (arr min max)
               (declare (type (simple-array double-float (*)) arr)
                        (type fixnum min max))
               (labels ((clip-to-bounds (value min max)
                          (declare (type (signed-byte 16) value)
                                   (type (unsigned-byte 16) min max))
                          (if (or (< value min) (>= value max)) +out-of-source-range+ value)))
                 (declare (inline clip-to-bounds))
                 (map '(simple-array (unsigned-byte 16) (*)) (lambda (x)
                                                               (declare (type (double-float -1d60 1d60) x))
                                                               (clip-to-bounds (round x) min max)) arr)))
             (from-source-geo-ref-to-source-pixel! (xarr yarr)
               (declare (type (simple-array double-float (*)) xarr yarr))
               (format t "Converting / rounding to pixel indices: clipping x to 0 and ~A, y to 0 and ~A~%" x-size y-size)
               (labels ((scale! (array scale shift)
                          (declare (type (simple-array double-float (*)) array)
                                   (type double-float shift scale))
                          (loop for idx fixnum below (length array)
                                do (setf (aref array idx) (* scale (- (aref array idx) shift))))
                          array))
                 (values (scale! xarr (/ x-size-df source-width) ulx)
                         (scale! yarr (/ y-size-df source-height) uly))))
	     (from-google-geo-ref-to-source-geo-ref (lons lats)
               (declare (type (simple-array double-float (*)) lons lats))
               (format t "Calling transform point array~%")
               (multiple-value-bind (source-pixel-x source-pixel-y)
                   (multiple-value-call #'from-source-geo-ref-to-source-pixel! (transform-point-array itransform lons lats))
                 (multiple-value-bind (source-pixel-x-inc source-pixel-y-inc)
                     (multiple-value-call #'from-source-geo-ref-to-source-pixel! (transform-point-array! itransform (map '(simple-array double-float (*)) (lambda (x) (+ x 0.1d0)) lons) lats))
                   (let ((angles (map '(simple-array single-float (*)) (lambda (xpixel-unshifted ypixel-unshifted xpixel-shifted ypixel-shifted)
                                                                         (declare (type double-float xpixel-unshifted ypixel-unshifted xpixel-shifted ypixel-unshifted))
                                                                         (float (angle (- xpixel-shifted xpixel-unshifted) (- ypixel-shifted ypixel-unshifted) 0.1d0 0.0d0) 0f0))
                                      source-pixel-x source-pixel-y source-pixel-x-inc source-pixel-y-inc)))
                     (values (clip/round-to-pixel source-pixel-x 0 x-size) (clip/round-to-pixel source-pixel-y 0 y-size) angles))))))
      (destructuring-bind (ullon ullat)
          (transform-point* ftransform ulx uly)
        (destructuring-bind (lrlon lrlat)
            (transform-point* ftransform lrx lry)
          (format t "Corner Coordinates of source file:~%")
          (format t "Upper Left (~,4f, ~,4f) (LON ~,6f, LAT ~,6f) ~%" ulx uly ullon ullat)
          (format t "Lower Right (~,4f, ~,4f) (LON ~,6f, LAT ~,6f)~%" lrx lry lrlon lrlat)
      #'from-google-geo-ref-to-source-geo-ref)))))



;;The original one
(defun generate-lat-lon-to-pixel-transform (handle)
  (declare (optimize (speed 3)))
  "Remember pixel 0 0 is the upper left of the image... so the bottom right of a 100x100 pixel image
   is actually at pixel 100x100 (the bottom right of the last pixel at (99,99))"
  (let* ((x-size (gdal-get-raster-band-x-size (gdal-get-raster-band handle 1)))
	 (y-size (gdal-get-raster-band-y-size (gdal-get-raster-band handle 1)))
	 (my-projection (osr-new-spatial-reference (gdalGetProjectionRef handle)))
	 (google-projection (osr-new-spatial-reference +google-map-wkt+))
	 (itransform (make-coordinate-transformation google-projection my-projection))
	 (ftransform (make-coordinate-transformation my-projection google-projection))
	 (source-geo-transform (get-geo-transform handle))
	 (ulx (aref source-geo-transform 0))
	 (lry (+ (aref source-geo-transform 3) (* x-size (aref source-geo-transform 4)) (* y-size (aref source-geo-transform 5))))
	 (lrx (+ (aref source-geo-transform 0) (* x-size (aref source-geo-transform 1)) (* y-size (aref source-geo-transform 2))))
	 (uly (aref source-geo-transform 3)))
    (declare (type fixnum x-size y-size)
	     (type double-float ulx lry lrx uly)
	     (type (simple-array double-float (6)) source-geo-transform))
    (labels ((from-source-pixel-to-source-geo-ref (pixelx pixely)
	       (list (+ (aref source-geo-transform 3) (* pixelx (aref source-geo-transform 4)) (* pixely (aref source-geo-transform 5)))
		     (+ (aref source-geo-transform 0) (* pixelx (aref source-geo-transform 1)) (* pixely (aref source-geo-transform 2)))))
	     (from-source-geo-ref-to-source-pixel (ref)
	       (let ((xpixel (* x-size (/ (- (the double-float (car ref)) ulx) (- lrx ulx))))
		     (ypixel (* y-size (/ (- (the double-float (cdr ref)) uly) (- lry uly)))))
		 (list (round xpixel) (round ypixel))))
	     (from-source-geo-ref-to-lon-lat (georefx georefy)
	       (transform-point* ftransform georefy georefx))
	     (from-source-pixel-to-lon-lat (pixelx pixely)
	       (apply #'from-source-geo-ref-to-lon-lat (from-source-pixel-to-source-geo-ref pixelx pixely)))
	     (from-google-geo-ref-to-source-geo-ref (a b)
	       (from-source-geo-ref-to-source-pixel
		(transform-point-unthreadsafe itransform (coerce a 'double-float) (coerce b 'double-float)))))
      (destructuring-bind (ullon ullat)
	  (transform-point* ftransform ulx uly)
	(destructuring-bind (lrlon lrlat)
	    (transform-point* ftransform lrx lry)
	  (format t "Corner Coordinates of source file:~%")
	  (format t "Upper Left (~,4f, ~,4f) (LON ~,6f, LAT ~,6f) ~%" ulx uly ullon ullat)
	  (format t "Lower Right (~,4f, ~,4f) (LON ~,6f, LAT ~,6f)~%" lrx lry lrlon lrlat)
	  (format t "Upper Left is ~A~%" (from-source-geo-ref-to-source-pixel (cons ulx uly)))
	  (format t "Lower Right is ~A~%" (from-source-geo-ref-to-source-pixel (cons lrx lry)))
      (values #'from-google-geo-ref-to-source-geo-ref #'from-source-pixel-to-lon-lat))))))
