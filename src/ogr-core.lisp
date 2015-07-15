;;; -*- package: OGR; Syntax: Common-lisp; Base: 10 -*-

;; based on the auto-generated documentation at:
;; http://www.gdal.org/ogr/ogr__api_8h.html

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defctype ogr-layer-h :pointer "OGRLayerH")

(cffi:defctype ogr-data-source-h :pointer "OGRDataSourceH")

(cffi:defctype ogr-geometry-h :pointer "OGRGeometryH")

(cffi:defctype ogr-feature-h :pointer "OGRFeatureH")

(cffi:defctype ogr-feature-defn-h :pointer "OGRFeatureDefnH")

(cffi:defctype ogr-spatial-reference-h :pointer "OGRSpatialReferenceH")

(cffi:defctype ogr-field-defn-h :pointer "OGRFieldDefnH")

(cffi:defctype ogr-geom-field-defn-h :pointer "OGRGeomFieldDefnH")

(cffi:defctype ogr-style-mgr-h :pointer "OGRStyleMgrH")

(cffi:defctype ogr-style-table-h :pointer "OGRStyleTableH")

(cffi:defctype ogr-style-tool-h :pointer "OGRStyleToolH")

(cffi:defctype ogr-sf-driver-h :pointer "OGRSFDriverH")

(cffi:defcstruct ogr-envelope
  "OGREnvelope is declared either as a class or a plain struct depending on the language."
  (MinX :double)
  (MaxX :double)
  (MinY :double)
  (MaxY :double))

(export 'ogr-envelope)
(export 'MinX)
(export 'MaxX)
(export 'MinY)
(export 'MaxY)

;; --------------------------------------------------------

(defclass <ogr-class> ()
  ((pointer
    :type (or null cffi:foreign-pointer)
    :initarg :pointer
    :accessor pointer
    :initform nil)))
(export 'pointer)

(defclass <data-source> (<ogr-class>)
  ;; original documentation at: http://www.gdal.org/ogr/classOGRDataSource.html
  ()
  (:documentation "This class represents a data source.

  A data source potentially consists of many layers (OGRLayer). A data
  source normally consists of one, or a related set of files, though
  the name doesn't have to be a real item in the file system.

  When an OGRDataSource is destroyed, all it's associated OGRLayers
  objects are also destroyed."))

(defclass <layer> (<ogr-class>)
  ((data-source
    :type (or null <data-source>)
    :initarg :data-source
    :accessor data-source
    :initform nil)))

(defclass <feature> (<ogr-class>)
  ())

(defclass <geometry> (<ogr-class>)
  ((data-source
    :type (or null <data-source>)
    :initarg :data-source
    :accessor data-source
    :initform nil)))

(defclass <spatial-ref> (<ogr-class>)
  ())

;; --------------------------------------------------------

(cffi:defcenum ogr-field-type
  "List of feature field types. This list is likely to be extended in
the future ... avoid coding applications based on the assumption that
all field types can be known. "
  (:oft-integer 0)		     ; Simple 32bit integer
  (:oft-integer-list 1)		     ; List of 32bit integers
  (:oft-real 2)			     ; Double Precision floating point
  (:oft-real-list 3)		     ; List of doubles
  (:oft-string 4)		     ; String of ASCII chars
  (:oft-string-list 5)		     ; Array of strings
  (:oft-wide-string 6)		     ; deprecated
  (:oft-wide-string-list 7)	     ; deprecated
  (:oft-binary 8)		     ; Raw Binary data
  (:oft-date 9)			     ; Date
  (:oft-time 10)		     ; Time
  (:oft-date-time 11))		     ; Date and Time

;; --------------------------------------------------------

(cffi:defcenum ogr-err
  "Errors are defined as macro constants, but we define is an
enumeration with set constant values."
  (:none                0)
  (:not-enough-data     1)
  (:not-enough-memory   2)
  (:unsupported-geometry-type 3)
  (:unsupported-operation 4)
  (:corrupt-data        5)
  (:failure             6)
  (:unsupported-srs     7)
  (:invalid-handle      8))

;; --------------------------------------------------------

(cffi:defcenum ogr-wkb-geometry-type
  "List of well known binary geometry types. These are used within the
BLOBs but are also returned from OGRGeometry::getGeometryType() to
identify the type of a geometry object."
  (:wkb-unknown 0)	; unknown type, non-standard
  (:wkb-point 1)	; 0-dimensional geometric object, standard WKB
  (:wkb-line-string 2)	; 1-dimensional geometric object with linear
					; interpolation between Points, standard WKB
  (:wkb-polygon 3) ; planar 2-dimensional geometric object defined by 1
					; exterior boundary and 0 or more interior
					; boundaries, standard WKB
  (:wkb-multi-point 4)	  ; GeometryCollection of Points, standard WKB
  (:wkb-multi-line-string 5) ; GeometryCollection of LineStrings, standard WKB
  (:wkb-multi-polygon 6) ; GeometryCollection of Polygons, standard WKB
  (:wkb-geometry-collection 7) ; geometric object that is a collection
					; of 1 or more geometric objects,
					; standard WKB
  (:wkb-none 100)	    ; non-standard, for pure attribute records
  (:wkb-linear-ring 101)    ; non-standard, just for createGeometry()
  (:wkb-point-25d #x80000001)		; 2.5D extension as per 99-402
  (:wkb-line-string-25d #x80000002)	; 2.5D extension as per 99-402
  (:wkb-polygon-25d #x80000003)		; 2.5D extension as per 99-402
  (:wkb-multi-point-25d #x80000004)	; 2.5D extension as per 99-402
  (:wkb-multi-line-string-25d #x80000005) ; 2.5D extension as per 99-402
  (:wkb-multi-polygon-25d #x80000006)	; 2.5D extension as per 99-402
  (:wkb-geometry-collection25d #x80000007)) ; 2.5D extension as per 99-402

;; --------------------------------------------------------

(cffi:defcenum ogr-justification)

;; --------------------------------------------------------

(cffi:defcenum ogr-st-class-id
    "ogr_style_tool_class_id: OGRSTClassId;"
  (:OGRSTCNone    0)
  (:OGRSTCPen     1)
  (:OGRSTCBrush   2)
  (:OGRSTCSymbol  3)
  (:OGRSTCLabel   4)
  (:OGRSTCVector  5))

;; --------------------------------------------------------

(cffi:defcenum OGR-ST-Unit-Id
    "ogr_style_tool_units_id: OGRSTUnitId"
  (:OGRSTUGround  0)
  (:OGRSTUPixel   1)
  (:OGRSTUPoints  2)
  (:OGRSTUMM      3)
  (:OGRSTUCM      4)
  (:OGRSTUInches  5))

;; --------------------------------------------------------

(cffi:defcenum ogr-st-pen-param
    "ogr_style_tool_param_pen_id: OGRSTPenParam"
  (:OGRSTPenColor        0)
  (:OGRSTPenWidth        1)
  (:OGRSTPenPattern      2)
  (:OGRSTPenId           3)
  (:OGRSTPenPerOffset    4)
  (:OGRSTPenCap          5)
  (:OGRSTPenJoin         6)
  (:OGRSTPenPriority     7)
  (:OGRSTPenLast         8))

;; --------------------------------------------------------

(cffi:defcenum ogr-st-brush-param
    "ogr_style_tool_param_brush_id: OGRSTBrushParam"
  (:OGRSTBrushFColor     0)
  (:OGRSTBrushBColor     1)
  (:OGRSTBrushId         2)
  (:OGRSTBrushAngle      3)
  (:OGRSTBrushSize       4)
  (:OGRSTBrushDx         5)
  (:OGRSTBrushDy         6)
  (:OGRSTBrushPriority   7)
  (:OGRSTBrushLast       8))

;; --------------------------------------------------------

(cffi:defcenum ogr-st-symbol-param
    "ogr_style_tool_param_symbol_id: OGRSTSymbolParam"
  (:OGRSTSymbolId        0)
  (:OGRSTSymbolAngle     1)
  (:OGRSTSymbolColor     2)
  (:OGRSTSymbolSize      3)
  (:OGRSTSymbolDx        4)
  (:OGRSTSymbolDy        5)
  (:OGRSTSymbolStep      6)
  (:OGRSTSymbolPerp      7)
  (:OGRSTSymbolOffset    8)
  (:OGRSTSymbolPriority  9)
  (:OGRSTSymbolFontName  10)
  (:OGRSTSymbolOColor    11)
  (:OGRSTSymbolLast      12))

;; --------------------------------------------------------

(cffi:defcenum ogr-st-label-param
    "ogr_style_tool_param_label_id: OGRSTLabelParam"
  (:OGRSTLabelFontName   0)
  (:OGRSTLabelSize       1)
  (:OGRSTLabelTextString 2)
  (:OGRSTLabelAngle      3)
  (:OGRSTLabelFColor     4)
  (:OGRSTLabelBColor     5)
  (:OGRSTLabelPlacement  6)
  (:OGRSTLabelAnchor     7)
  (:OGRSTLabelDx         8)
  (:OGRSTLabelDy         9)
  (:OGRSTLabelPerp       10)
  (:OGRSTLabelBold       11)
  (:OGRSTLabelItalic     12)
  (:OGRSTLabelUnderline  13)
  (:OGRSTLabelPriority   14)
  (:OGRSTLabelStrikeout  15)
  (:OGRSTLabelStretch    16)
  (:OGRSTLabelAdjHor     17)
  (:OGRSTLabelAdjVert    18)
  (:OGRSTLabelHColor     19)
  (:OGRSTLabelOColor     20)
  (:OGRSTLabelLast       21))

;; --------------------------------------------------------

(defconstant +wkb25DBit+ #x80000000)

(defun wkb-flatten (x)
  "The wkb-flatten function is used above to convert the type for a
:wkbPoint25D (a point with a z coordinate) into the base 2D geometry
type code (:wkbPoint). For each 2D geometry type there is a
corresponding 2.5D type code. The 2D and 2.5D geometry cases are
handled by the same C++ class, so our code will handle 2D or 3D cases
properly."

  (let ((%x (if (keywordp x)
		(cffi:foreign-enum-value 'OGR-wkb-Geometry-Type x)
		x)))
    (cffi:foreign-enum-keyword 'OGR-wkb-Geometry-Type
			       (logand %x (lognot +wkb25DBit+)))))
(export 'wkb-flatten)

;; --------------------------------------------------------

(cffi:defcfun ("CPLFree" cpl-free) :void
  (ref :pointer))
(export 'cpl-free)

;; --------------------------------------------------------

(cffi:defcfun ("OGRCleanupAll" ogr-cleanup-all) :void
  "Cleanup all OGR related resources.

 This function will destroy the OGRSFDriverRegistrar along with all
 registered drivers, and then cleanup long lived
 OSR (OGRSpatialReference) and CPL resources. This may be called in an
 application when OGR services are no longer needed. It is not
 normally required, but by freeing all dynamically allocated memory it
 can make memory leak testing easier.

 In addition to destroying the OGRDriverRegistrar, this function also
 calls:

 OSRCleanup()
 CPLFinderClean()
 VSICleanupFileManager()
 CPLFreeConfig()
 CPLCleanupTLS()")

;; --------------------------------------------------------

(defgeneric get-spatial-ref (g))
(export 'get-spatial-ref)

;; EOF
