(in-package :cl-gdal)

;; Mapping from longitude / latitude to pixels in a geo referenced file and the inverse

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

(defun set-geo-transform (hdriver array)
  "GDALSetGeoTransform copies the coefficients."
  (declare (type geotransform array))
  (GDALSetGeoTransform hdriver array))

(defun get-geo-transform (dshandle)
  (let ((a (make-array 6 :element-type 'double-float)))
    (handling-cpl-err (gdalgetgeotransform dshandle a))
    a))

(deftype pixelidx () 'fixnum)

(defun make-geo-transformer (hband)
  "For a raster band HBAND, generate a function that converts from pixel indices
   (where xpixel,ypixel = 0,0 is the upper left pixel of the raster
   band), to the center of the pixel in source geo reference units (typically
   meters).  Returns (cons geo-x geo-y)."
  (let ((gt (get-geo-transform hband)))
    (declare (type geotransform gt))
    (lambda (x-idx y-idx) ;; pixel indices, from 0
      (let ((center-x (+ x-idx 0.5))
            (center-y (+ y-idx 0.5)))
        (cons (+ (aref gt 0) (* (aref gt 1) center-x) (* (aref gt 2) center-y))
              (+ (aref gt 3) (* (aref gt 4) center-x) (* (aref gt 5) center-y)))))))

(defun make-inverse-geo-transformer (hband)
  (let ((gt (get-geo-transform hband)))
    (declare (type geotransform gt))
    (lambda (geo-x geo-y) ;; meters typically
      ;; do math
      )))

(defun transform-point* (coordinate-transformation x y &optional (z 0d0 z-p))
  "coordinate-transformation should be from something like:
           (let* ((source-spatial-reference (cl-gdal::osr-new-spatial-reference (cl-gdal::gdalgetprojectionref handle)))
                  (google-spatial-reference (cl-gdal::osr-new-spatial-reference cl-gdal::+google-map-wkt+)))
               (cl-gdal::make-coordinate-transformation google-spatial-reference source-spatial-reference))"
  (cffi:with-foreign-object (data :double 3)
    (setf (cffi:mem-aref data :double 0) x)
    (setf (cffi:mem-aref data :double 1) y)
    (setf (cffi:mem-aref data :double 2) z)
    (octt-transform-array coordinate-transformation
                          1
                          (cffi:mem-aptr data :double 0)
                          (cffi:mem-aptr data :double 1)
                          (cffi:mem-aptr data :double 2))
    (if z-p
        (list (cffi:mem-aref data :double 0) (cffi:mem-aref data :double 1) (cffi:mem-aref data :double 2))
        (list (cffi:mem-aref data :double 0) (cffi:mem-aref data :double 1)))))

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

(defun print-bounding-area (gdal-dataset-h)
  )

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
		     (ypixel (* y-size (/ (- (the double-float (cadr ref)) uly) (- lry uly)))))
		 (list xpixel ypixel)))
	     (from-source-geo-ref-to-lon-lat (georefx georefy)
	       (transform-point* ftransform georefy georefx))
	     (from-source-pixel-to-lon-lat (pixelx pixely)
	       (apply #'from-source-geo-ref-to-lon-lat (from-source-pixel-to-source-geo-ref pixelx pixely)))
	     (from-google-geo-ref-to-source-geo-ref (a b)
               (destructuring-bind (xpixel-unshifted ypixel-unshifted)
                   (from-source-geo-ref-to-source-pixel
                    (transform-point* itransform (coerce a 'double-float) (coerce b 'double-float)))
                 (destructuring-bind (xpixel-shifted ypixel-shifted)
                   (from-source-geo-ref-to-source-pixel
                    (transform-point* itransform (+ (coerce a 'double-float) 0.1d0) (coerce b 'double-float)))
                   (values (round xpixel-unshifted) (round ypixel-shifted)
                           (float (angle (- xpixel-shifted xpixel-unshifted) (- ypixel-shifted ypixel-unshifted) 0.1d0 0.0d0) 0f0))))))
      (destructuring-bind (ullon ullat)
          (transform-point* ftransform ulx uly)
        (destructuring-bind (lrlon lrlat)
            (transform-point* ftransform lrx lry)
          (format t "Corner Coordinates of source file:~%")
          (format t "Upper Left (~,4f, ~,4f) (LON ~,6f, LAT ~,6f) ~%" ulx uly ullon ullat)
          (format t "Lower Right (~,4f, ~,4f) (LON ~,6f, LAT ~,6f)~%" lrx lry lrlon lrlat)
          (format t "Upper Left is ~A~%" (from-source-geo-ref-to-source-pixel (list ulx uly)))
          (format t "Lower Right is ~A~%" (from-source-geo-ref-to-source-pixel (list lrx lry)))))
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

;; (defun clip-geo-transform (gt ulpixelx ulpixely)
;;   "From an extant raster band with geo-transform GT, return a new geo-transform which has as the upper left, upper right corner
;;    the upper left of pixel (ULPIXELX, ULPIXELY)"
;;   ;; GT(0) x-coordinate of the upper-left corner of the upper-left pixel.
;;   ;; GT(1) w-e pixel resolution / pixel width.
;;   ;; GT(2) row rotation (typically zero).
;;   ;; GT(3) y-coordinate of the upper-left corner of the upper-left pixel.
;;   ;; GT(4) column rotation (typically zero).
;;   ;; GT(5) n-s pixel resolution / pixel height (negative value for a north-up image).
;;   (declare (type (simple-array double-float (6)) gt))
;;   (assert (and (zerop (aref gt 2)) (zerop (aref gt 4))))
;;   ;; If you want to handle rotated grid, here are what the lower right geo coordinates are with a rotated grid... too lazy to think about this right now
;;   ;; 	 (lry (+ (aref source-geo-transform 3) (* x-size-df (aref source-geo-transform 4)) (* y-size-df (aref source-geo-transform 5))))
;;   ;;     (lrx (+ (aref source-geo-transform 0) (* x-size-df (aref source-geo-transform 1)) (* y-size-df (aref source-geo-transform 2))))
;;   (let ((new-array (make-array 6 :element-type 'double-float :initial-contents gt)))
;;     (setf (aref new-array 0) (* (- ulpixelx 0.5) (aref new-array 1))) ;; we want the upper left of the requested pixel
;;     (setf (aref new-array 3) (* (- ulpixely 0.5) (aref new-array 2)))
;;     new-array))

;; (defun rough-clip-gdal-raster (raster lon-lat-to-pixel-transform geo-transform ul-lon ul-lat lr-lon lr-lat)
;;   "This does not change the projection, but chooses instead the best sub-set of the square that includes the target ul-lon ul-lat lr-long lr-lat values.
;;    Hard coded to double floats right now..."
;;   (declare (type function lon-lat-to-pixel-transform)
;;            (type geotransform geo-transform))
;;   (destructuring-bind (ul-x ul-y)
;;         (funcall lon-lat-to-pixel-transform ul-lon ul-lat)
;;       (destructuring-bind (lr-x lr-y)
;;           (funcall lon-lat-to-pixel-transform lr-lon lr-lat)
;;         (let ((real-ul-x (min ul-x lr-x))
;;               (real-lr-x (max ul-x lr-x))
;;               (real-ul-y (min ul-y lr-y))
;;               (real-lr-y (max ul-y lr-y)))
;;           (let* ((new-geo-transform (clip-geo-transform geo-transform real-ul-x real-ul-y))
;;                  (new-width (1+ (- real-lr-x real-ul-x))) ;; we want to include the pixels requested
;;                  (new-height (1+ (- real-lr-y real-ul-y)))
;;                  (new-raster (cffi:foreign-alloc :double :count (* new-height new-width))))
;;             (format t "New-raster is ~A~%" new-raster)
;;             (loop
;;               for y-target of-type fixnum from 0 below new-height
;;               for y-source of-type fixnum from real-ul-y
;;               do
;;                  (loop
;;                    for x-target of-type fixnum from 0 below new-width
;;                    for x-source of-type fixnum from real-ul-x
;;                    do
;;                       (setf (cffi:mem-aref new-raster :double (+ (* y-target new-width) x-target)) (aref raster y-source x-source))))
;;             (values new-raster new-geo-transform new-width new-height))))))

;; Since we can only CreateCopy not Create with grib2, we want to generate a template file for the outputs.  Maybe if we use all zeros we can
;; generate some very minimal set over the tiles with the right size band, and then just write in the new band?  For now, though, the input-file
;; will be the large file.

;; (defun make-cpl-string-list (key-value-alist)
;;   "key-value-pairs is an alist of key value string pairs"
;;   (let ((string-array (cffi:foreign-alloc :pointer :count (1+ (length key-value-alist)))))
;;     (loop
;;       for alist-entry in key-value-alist
;;       for index fixnum from 0
;;       do
;;       (let ((string (format nil "~A=~A" (car alist-entry) (cdr alist-entry))))
;;         (setf (cffi:mem-aref string-array :pointer index)
;;               (cffi:foreign-string-alloc string :null-terminated-p t)))
;;       finally
;;            (setf (cffi:mem-aref string-array :pointer index) (cffi:null-pointer))
;;            (return string-array))))

;; (defun free-cpl-string-list (cpl-string-list)
;;   (loop
;;     for index fixnum from 0
;;     for ptr = (cffi:mem-aref cpl-string-list :pointer index)
;;     until (cffi:null-pointer-p ptr)
;;     do
;;        (cffi:foreign-free ptr))
;;   (cffi:foreign-free cpl-string-list))
       

;; (defun test-rough-clip-gdal-raster (input-file tile output-file)
;;   (cl-gdal::maybe-initialize-gdal-ogr)
;;   (cl-gdal::with-gdal-file (handle input-file)
;;     (let* ((driver (cl-gdal::gdal-get-driver-by-name "GRIB"))
;;            (data (cl-gdal::gdal-read-all-data (cl-gdal::gdal-get-raster-band handle 1)))
;;            (geo-transform (get-geo-transform handle)))
;;       (multiple-value-bind (lon-lat-to-pixel-transform pixel-to-lon-lat-transform)
;;           (generate-lon-lat-to-pixel-transform handle)
;;         (declare (ignorable pixel-to-lon-lat-transform))
;;         (format t "Source image is ~A by ~A pixels~%" (cadr (array-dimensions data)) (car (array-dimensions data)))
;;         (multiple-value-bind (new-raster new-geo-transform x-size y-size)
;;             (apply #'rough-clip-gdal-raster data lon-lat-to-pixel-transform geo-transform tile)
;;           ;; UGH, GRIB2 does not support create.  But can use gdal_translate call????
;;           (let* ((papszoptions (make-cpl-string-list '(("DATA_ENCODING" . "IEEE_FLOATING_POINT") ("NBITS" . "64"))))
;;                  (new-dataset (cl-gdal::gdal-create-copy driver output-file handle 0 papszoptions (cffi::null-pointer) (cffi::null-pointer))))
;;             (when (cffi:null-pointer-p new-dataset)
;;               (format t "Error: ~A~%" (cl-gdal::cpl-get-last-error-msg)))
;;             (free-cpl-string-list papszoptions)
;;             (print new-dataset)
;;             (print (set-geo-transform new-dataset new-geo-transform))
;;             (print (cl-gdal::GDALSetProjection new-dataset (cl-gdal::GDALGetProjectionRef handle)))
;;             (format t "new-raster is ~A and is ~A x ~A~%" new-raster x-size y-size)
;;             (print (cl-gdal::gdal-raster-io new-dataset :GF_Write 0 0 x-size y-size new-raster x-size y-size :GDT_Float64 0 0))
;;             (print (cl-gdal::gdal-close new-dataset))
;;             (cffi:foreign-free new-raster)))))))
           
