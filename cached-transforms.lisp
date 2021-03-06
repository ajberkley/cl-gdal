(defpackage :cached-transforms
  (:use :common-lisp :cl-gdal :mmap-shared-cache)
  (:export
   )
  (:documentation "")
  (:export
   #:with-cached-transform))

(in-package :cached-transforms)

(defun cache-filename (ul-lon ul-lat lr-lon lr-lat x-size y-size input-x-size input-y-size source-spatial-reference source-geo-transform)
  (format nil "~A.cache"
          (logxor 
           (sxhash (list ul-lon ul-lat lr-lon lr-lat))
           (sxhash (list x-size y-size input-x-size input-y-size))
           (sxhash source-spatial-reference)
           (sxhash (coerce source-geo-transform 'list)))))

(defconstant +out-of-source-range+ (1- (ash 1 16)))

(deftype target-size () `(integer 0 ,+out-of-source-range+))

(defun generate-nearest-neighbors (ul-lon ul-lat dlon dlat x-size y-size input-x-size input-y-size source-spatial-reference source-geo-transform)
  "Given a desired output lon/lat grid with x-size by y-size elements
   that has an upper left corner defined by longitude ul-lon and
   latitude ul-lat and lower right corner having longitude lr-lon and
   latitude lr-lat, and some source grid defined with a
   source-spatial-reference and source-geo-transform, returns the step
   size in the output grid dlon and dlat, and two 2D arrays containing
   the nearest neighbor indices into the source grid for each point in
   the requested lon/lat grid."
  (declare (optimize (speed 3))
           (type target-size x-size y-size input-x-size input-y-size))
  (let* ((ulx (coerce ul-lon 'double-float))
         (uly (coerce ul-lat 'double-float))
         (xarray (make-array (* y-size x-size) :element-type 'double-float))
         (yarray (make-array (* y-size x-size) :element-type 'double-float))
         (lon-lat-translator (cl-gdal::generate-lon-lat-to-pixel-transform-array input-x-size input-y-size source-spatial-reference source-geo-transform)))
    (declare (type (simple-array (double-float -1d9 1d9) (*)) xarray yarray)
             (type double-float ulx uly dlon dlat) (function lon-lat-translator))
    (format t "Initializing lon lat data pairs for ~A by ~A array~%" x-size y-size)
    (loop for y fixnum below y-size
          do 
             (loop for x fixnum below x-size
                   for idx fixnum from (* x-size y)
                   do
                      (progn
                        (setf (aref xarray idx) (+ ulx (* x dlon)))
                        (setf (aref yarray idx) (- uly (* y dlat))))))
    (multiple-value-bind (xpixels ypixels angles)
        (funcall lon-lat-translator xarray yarray)
    (values xpixels ypixels angles))))

(defun write-nearest-neighbor-cache-file (addr ul-lon ul-lat dlon dlat x-size y-size input-x-size input-y-size source-spatial-reference source-geo-transform)
  (multiple-value-bind (xpixels ypixels angles)
      (generate-nearest-neighbors ul-lon ul-lat dlon dlat x-size y-size input-x-size input-y-size source-spatial-reference source-geo-transform)
    (format t "Writing ~A xpixels (~A) ~A ypixels (~A) and ~A angles (~A)~%" (length xpixels) (type-of xpixels) (length ypixels) (type-of ypixels) (length angles) (type-of angles))
    (mmap-shared-cache::write-vector-to-sap addr xpixels :uint16)
    (mmap-shared-cache::write-vector-to-sap (cffi:mem-aptr addr :uint16 (length xpixels)) ypixels :uint16)
    (mmap-shared-cache::write-vector-to-sap (cffi:mem-aptr addr :uint16 (+ (length xpixels) (length ypixels))) angles :float)
    ))


(defun test-cache ()
  (let* ((xpixels 3348)
         (ypixels 2162)
         (args (list -152.0d0 71.0d0 0.02d0 0.03d0 xpixels ypixels 2576 1456 "PROJCS[\"unnamed\",GEOGCS[\"Coordinate System imported from GRIB file\",DATUM[\"unknown\",SPHEROID[\"Sphere\",6371229,0]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433]],PROJECTION[\"Polar_Stereographic\"],PARAMETER[\"latitude_of_origin\",60],PARAMETER[\"central_meridian\",252],PARAMETER[\"scale_factor\",1],PARAMETER[\"false_easting\",0],PARAMETER[\"false_northing\",0]]" (make-array 6 :element-type 'double-float :initial-contents #(-2099127.4944969374d0 2500.0d0 0.0d0 -2099388.5214996273d0 0.0d0 -2500.0d0))))
         (filename (apply #'cache-filename args)))
    (labels ((create-file (addr size)
               (declare (ignorable size))
               (apply #'write-nearest-neighbor-cache-file addr args)))
      (mmap-shared-cache::with-mmap-shared-cache (#'create-file filename addr (* ypixels xpixels 8)) ;; two arrays of 2 byte uint16 and one of 4 byte float
        (let ((xvector (cffi:mem-aptr addr :uint16 0))
              (yvector (cffi:mem-aptr addr :uint16 (* xpixels ypixels)))
              (angles  (cffi:mem-aptr addr :uint16 (* 2 xpixels ypixels))))
          (loop for idx in (list 0 (1- xpixels) (1- (* xpixels (1- ypixels))) (1- (* (1- xpixels) (1- ypixels))))
                for place in '("Upper left" "Upper right" "Lower left" "Lower right")
                do
          (format t "~A is at idx ~A x ~A, y ~A, and angle ~A~%" place idx (cffi:mem-aref xvector :uint16 idx) (cffi:mem-aref yvector :uint16 idx) (round (* 360 (/ (cffi:mem-aref angles :float idx) (* 2 pi)))))))))))

(defmacro with-cached-transform ((xpixel ypixel anglepixel ul-lon ul-lat dlon dlat x-size y-size input-x-size input-y-size source-spatial-reference source-geo-transform) &body body)
  (let ((filename (gensym "CACHEFILE-"))
        (ypixeladdr (gensym "YPIXELADDR-"))
        (xpixeladdr (gensym "XPIXELADDR-"))
        (anglepixeladdr (when anglepixel (gensym "ANGLEADDR-")))
        (addr (gensym "ADDR-")))
    (alexandria:once-only (ul-lon ul-lat dlon dlat x-size y-size input-x-size input-y-size source-spatial-reference source-geo-transform)
      `(let ((,filename (cache-filename ,ul-lon ,ul-lat ,dlon ,dlat ,x-size ,y-size ,input-x-size ,input-y-size ,source-spatial-reference ,source-geo-transform)))
         (mmap-shared-cache::with-mmap-shared-cache ((lambda (addr size)
                                                       (declare (ignorable size))
                                                       (write-nearest-neighbor-cache-file addr ,ul-lon ,ul-lat ,dlon ,dlat ,x-size ,y-size ,input-x-size ,input-y-size ,source-spatial-reference ,source-geo-transform))
                                                     ,filename ,addr (* ,x-size ,y-size ,(if anglepixel 8 4)))
           (let ((,xpixeladdr ,addr)
                 (,ypixeladdr (cffi:mem-aptr ,addr :uint16 (* ,y-size ,x-size)))
                 ,@(when anglepixeladdr `((,anglepixeladdr (cffi:mem-aptr ,addr :uint16 (* 2 ,y-size ,x-size))))))
           (labels ((,xpixel (index)
                      (cffi:mem-aref ,xpixeladdr :uint16 index))
                    (,ypixel (index)
                      (cffi:mem-aref ,ypixeladdr :uint16 index))
                    ,@(when anglepixel
                        `((,anglepixel (index)
                                       (cffi:mem-aref ,anglepixeladdr :float index)))))
             (declare (inline ,xpixel ,ypixel))
             ,@body)))))))


