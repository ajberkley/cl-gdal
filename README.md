This package provides minimal set of
[CFFI](http://common-lisp.net/project/cffi/)-based Common Lisp
bindings to the [GDAL/OGR library](http://www.gdal.org/).

This was forked from vityok/cl-gdal.  It's a snapshot of what I use
for the canadarasp.com backend.  I added some more functionality and
some wrappers for somewhat ease of use.

Install:

```bash
git clone https://github.com/ajberkley/cl-gdal.git
# add repo to your local asdf source registry
echo "(:tree \"~/cl-gdal\")" > ~/.config/common-lisp/source-registry.conf.d/10-cl-gdal.conf
```
```lisp
;; Start up slime
(quicklisp:quickload :cffi)
(quicklisp:quickload :trivial-garbage)

(require 'cl-gdal)
(require 'cl-ogr)
(load "src/gdal-core.lisp")
(load "src/ogr-core.lisp")
(load "cl-gdal-local.lisp")
(use-package :cl-gdal)
(use-package :cl-ogr)
```