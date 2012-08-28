(in-package :glu-tessellate)

;; -----------------------------------------------------------------------------
;; The following constands and bindings were taken directly from the CL-GLFW GLU
;; bindings since cl-opengl see's fit to not support tessellation at all.
;; -----------------------------------------------------------------------------
;;  TessCallback
(defconstant +tess-begin+ 100100)
(defconstant +begin+ 100100)
(defconstant +tess-vertex+ 100101)
(defconstant +vertex+ 100101)
(defconstant +tess-end+ 100102)
(defconstant +end+ 100102)
(defconstant +tess-error+ 100103)
(defconstant +tess-edge-flag+ 100104)
(defconstant +edge-flag+ 100104)
(defconstant +tess-combine+ 100105)
(defconstant +tess-begin-data+ 100106)
(defconstant +tess-vertex-data+ 100107)
(defconstant +tess-end-data+ 100108)
(defconstant +tess-error-data+ 100109)
(defconstant +tess-edge-flag-data+ 100110)
(defconstant +tess-combine-data+ 100111)
;;  TessContour
(defconstant +cw+ 100120)
(defconstant +ccw+ 100121)
(defconstant +interior+ 100122)
(defconstant +exterior+ 100123)
(defconstant +unknown+ 100124)
;;  TessProperty
(defconstant +tess-winding-rule+ 100140)
(defconstant +tess-boundary-only+ 100141)
(defconstant +tess-tolerance+ 100142)
;;  TessError
(defconstant +tess-error-1+ 100151)
(defconstant +tess-error-2+ 100152)
(defconstant +tess-error-3+ 100153)
(defconstant +tess-error-4+ 100154)
(defconstant +tess-error-5+ 100155)
(defconstant +tess-error-6+ 100156)
(defconstant +tess-error-7+ 100157)
(defconstant +tess-error-8+ 100158)
(defconstant +tess-missing-begin-polygon+ 100151)
(defconstant +tess-missing-begin-contour+ 100152)
(defconstant +tess-missing-end-polygon+ 100153)
(defconstant +tess-missing-end-contour+ 100154)
(defconstant +tess-coord-too-large+ 100155)
(defconstant +tess-need-combine-callback+ 100156)
;;  TessWinding
(defconstant +tess-winding-odd+ 100130)
(defconstant +tess-winding-nonzero+ 100131)
(defconstant +tess-winding-positive+ 100132)
(defconstant +tess-winding-negative+ 100133)
(defconstant +tess-winding-abs-geq-two+ 100134)
;; ***********************************************************
(defconstant +tess-max-coord+ 1.0d150)

(defconstant +points+ #x0)
(defconstant +lines+ #x1)
(defconstant +line-loop+ #x2)
(defconstant +line-strip+ #x3)
(defconstant +triangles+ #x4)
(defconstant +triangle-strip+ #x5)
(defconstant +triangle-fan+ #x6)
(defconstant +quads+ #x7)
(defconstant +quad-strip+ #x8)
(defconstant +polygon+ #x9)

(define-foreign-library glu
  (:windows "glu32.dll") ; XXX?
  ((:and :unix (:not :darwin)) (:or "libGLU.so.1" "libGLU.so"))
  ((:not :darwin) (:default "libGLU")))

(use-foreign-library glu)

(defcfun ("gluNextContour" next-contour) :void (tess :pointer) (type :int))
(defcfun ("gluEndPolygon" end-polygon) :void (tess :pointer))
(defcfun ("gluDeleteTess" delete-tess) :void (tess :pointer))
(defcfun ("gluNewTess" new-tess) :pointer)
(defcfun ("gluTessBeginContour" tess-begin-contour) :void (tess :pointer))
(defcfun ("gluTessBeginPolygon" tess-begin-polygon) :void (tess :pointer) (data :pointer))
(defcfun ("gluTessCallback" tess-callback) :void (tess :pointer) (which :int) (CallBackFunc :pointer))
(defcfun ("gluTessEndContour" tess-end-contour) :void (tess :pointer))
(defcfun ("gluTessEndPolygon" tess-end-polygon) :void (tess :pointer))
(defcfun ("gluTessNormal" tess-normal) :void (tess :pointer) (valueX :double) (valueY :double) (valueZ :double))
(defcfun ("gluTessProperty" tess-property) :void (tess :pointer) (which :int) (value :double))
(defcfun ("gluTessVertex" tess-vertex) :void (tess :pointer) (location :pointer) (data :pointer))

(defcfun ("gluGetTessProperty" get-tess-property) :void (tess :pointer) (which :int) (data :pointer))
(defcfun ("gluBeginPolygon" begin-polygon) :void (tess :pointer))
(defcfun ("gluErrorString" error-string) :pointer (code :int))
;; -----------------------------------------------------------------------------
;; End code taken from CL-GLFW. Thank you!!
;; -----------------------------------------------------------------------------

(defparameter *polygons* nil)
(defparameter *triangles* nil)
(defparameter *cur-triangle* nil)
(defparameter *cur-triangle-wind* nil)
(defparameter *cur-type* nil)
(defparameter *created-points* nil)

(defun do-tess-begin (type)
  "Called when a tessellation shape begins. Stores the shape type for later
  reference."
  (setf *cur-type* type))

(defun do-tess-end ()
  "Called with tessellation finishes with a shape. Saves all relevant data and
  sets up for another shape to come its way."
  (setf *polygons* (append *polygons* *triangles*)
        *triangles* nil
        *cur-triangle* nil))

(defun do-tess-vertex (vertex)
  "Called when tessellation sends us a vertex. Uses the current shape type to
  figure out how the vertex will be processed/stored."
  (let ((x (mem-aref vertex :double 0))
        (y (mem-aref vertex :double 1)))
    ;(format t "vert(~a): ~a ~a~%" *cur-type* x y)
    (cond
      ((= *cur-type* +triangles+)
        (push (list x y) *cur-triangle*)
        (when (= 3 (length *cur-triangle*))
          (push (reverse *cur-triangle*) *triangles*)
          (setf *cur-triangle* nil)))
      ((= *cur-type* +triangle-strip+)
        (if (<= 2 (length *cur-triangle*))
            (setf *cur-triangle* (list (list x y)
                                       (nth 0 *cur-triangle*)
                                       (nth 1 *cur-triangle*)))
            (push (list x y) *cur-triangle*))
        (when (= 3 (length *cur-triangle*))
          (push (if *cur-triangle-wind*
                    *cur-triangle*
                    (reverse *cur-triangle*)) *triangles*)
          (setf *cur-triangle-wind* (not *cur-triangle-wind*))))
      ((= *cur-type* +triangle-fan+)
        (if (= 3 (length *cur-triangle*))
            (setf *cur-triangle* (list (list x y)
                                       (nth 0 *cur-triangle*)
                                       (nth 2 *cur-triangle*)))
            (push (list x y) *cur-triangle*))
        (when (= 3 (length *cur-triangle*))
          (push (reverse *cur-triangle*) *triangles*))))))

(defun do-tess-error (err)
  "Error!!!!"
  (format t "Tessellation error(~a): ~a~%" err (foreign-string-to-lisp (error-string err))))

(defun do-tess-combine (coords vertex-data weights data-out)
  "Called when tessellation must create a new point. Ideally this is where
  user data would be merged into a new point, but since we don't support user
  data, there's nothing to do but create the point and return =]."
  (declare (ignore weights vertex-data))
  (let ((vertex (foreign-alloc :double :count 6))
        (x (mem-aref coords :double 0))
        (y (mem-aref coords :double 1))
        (z (mem-aref coords :double 2)))
    (push vertex *created-points*)
    ;(format t "x,y,z: ~a ~a ~a~%" x y z)
    (setf (mem-aref vertex :double 0) x
          (mem-aref vertex :double 1) y
          (mem-aref vertex :double 2) z
          (mem-aref vertex :double 3) x
          (mem-aref vertex :double 4) y
          (mem-aref vertex :double 5) z)
    ;; This is mainly for merging colors an junk, which can be important, but
    ;; right now it's screwing up more than helping.
    ;(loop for i from 3 to 6 do
    ;  (let ((weight0 (mem-aref weights :float 0))
    ;        (weight1 (mem-aref weights :float 1))
    ;        (weight2 (mem-aref weights :float 2))
    ;        (weight3 (mem-aref weights :float 3))
    ;        (vert-data0 (mem-aref (mem-aref vertex-data :double 0) :double i))
    ;        (vert-data1 (mem-aref (mem-aref vertex-data :double 1) :double i))
    ;        (vert-data2 (mem-aref (mem-aref vertex-data :double 2) :double i))
    ;        (vert-data3 (mem-aref (mem-aref vertex-data :double 3) :double i)))
    ;    (setf (mem-aref vertex :double i) (+ (* weight0 vert-data0)
    ;                                         (* weight1 vert-data1)
    ;                                         (* weight2 vert-data2)
    ;                                         (* weight3 vert-data3)))))
    (setf (mem-aref data-out :pointer) vertex)))

;; Define some wrapper callbacks
(defcallback tess-begin-cb :void ((type :int))
  (do-tess-begin type))
(defcallback tess-end-cb :void ()
  (do-tess-end))
(defcallback tess-vertex-cb :void ((vertex :pointer))
  (do-tess-vertex vertex))
(defcallback tess-error-cb :void ((err :int))
  (do-tess-error err))
(defcallback tess-combine-cb :void ((coords :pointer) (vertex :pointer) (weight :pointer) (data-out :pointer))
  (do-tess-combine coords vertex weight data-out))

(defun polygon-clockwise-p (polygon-points)
  "Determine if the points of a polygon are in clockwise order."
  (declare (type vector polygon-points))
  (let ((sum 0))
    (loop for i from 0 to (1- (length polygon-points))
          for cur-point = (aref polygon-points i)
          for next-point = (aref polygon-points (mod (1+ i) (length polygon-points))) do
      (incf sum (- (* (car cur-point) (cadr next-point))
                   (* (cadr cur-point) (car next-point)))))
    (< sum 0)))

(defun get-winding-rule (wind-keyword)
  "Get the global winding rule for the keyword passed in."
  (assert (find wind-keyword '(:odd :nonzero :positive :negative :abs-geq-two)))
  (case wind-keyword
    (:odd +tess-winding-odd+)
    (:nonzero +tess-winding-nonzero+)
    (:positive +tess-winding-positive+)
    (:negative +tess-winding-negative+)
    (:abs-geq-two +tess-winding-abs-geq-two+)))

(defun tessellate (points &key (holes nil) (winding-rule :odd) cw)
  "Tessellate a polygon into triangles."
  ;; if we rebind these with let, we can make the entire thing thread-safe
  (let ((*polygons* nil)
        (*triangles* nil)
        (*cur-triangle* nil)
        (*cur-triangle-wind* nil)
        (*cur-type* nil)
        (*created-points* nil)
        (tess (new-tess)))
    (let ((poly-data (foreign-alloc :pointer :count (length points)))
          (hole-data (make-array (length holes))))
      (loop for i from 0
            for vert across points do
        (let ((x (coerce (nth 0 vert) 'double-float))
              (y (coerce (nth 1 vert) 'double-float))
              (z 0d0))
          (setf (mem-aref poly-data :pointer i) (foreign-alloc :double :initial-contents (list x y z)))))
      (when holes
        (loop for i from 0
              for hole in holes do
          (let ((hole-ptr (foreign-alloc :pointer :count (length hole))))
            (loop for i from 0
                  for vert across hole do
              (let ((x (coerce (nth 0 vert) 'double-float))
                    (y (coerce (nth 1 vert) 'double-float))
                    (z 0d0))
                (setf (mem-aref hole-ptr :pointer i) (foreign-alloc :double :initial-contents (list x y z)))))
            (setf (aref hole-data i) hole-ptr))))
      (unwind-protect
        (progn
          (tess-callback tess +tess-begin+ (callback tess-begin-cb))
          (tess-callback tess +tess-end+ (callback tess-end-cb))
          (tess-callback tess +tess-vertex+ (callback tess-vertex-cb))
          (tess-callback tess +tess-error+ (callback tess-error-cb))
          (tess-callback tess +tess-combine+ (callback tess-combine-cb))

          ;; set the winding rule
          (let ((wind (coerce (get-winding-rule winding-rule) 'double-float)))
            (tess-property tess +tess-winding-rule+ wind))

          ;; triangulate!
          (tess-begin-polygon tess (null-pointer))
          (tess-begin-contour tess)
          (dotimes (i (length points))
            (let ((data (mem-aref poly-data :pointer i)))
              (tess-vertex tess data data)))
          (tess-end-contour tess)

          (when holes
            (loop for hole in holes
                  for hole-ptr across hole-data do
              (tess-begin-contour tess)
              (dotimes (i (length hole))
                (let ((data (mem-aref hole-ptr :pointer i)))
                  (tess-vertex tess data data)))
              (tess-end-contour tess)))
          (tess-end-polygon tess))

        ;; clean up allocated memory
        (dotimes (i (length points))
          (foreign-free (mem-aref poly-data :pointer i)))
        (loop for hole in holes
              for hole-ptr across hole-data do
          (dotimes (i (length hole))
            (foreign-free (mem-aref hole-ptr :pointer i))))
        (dolist (vert *created-points*)
          (foreign-free vert))
        (delete-tess tess)))
    (mapcar (lambda (tri)
              (let ((clockwise (polygon-clockwise-p (coerce tri 'vector))))
                (if (or (and cw (not clockwise))
                        (and (not cw) clockwise))
                    (reverse tri)
                    tri)))
            (reverse *polygons*))))

