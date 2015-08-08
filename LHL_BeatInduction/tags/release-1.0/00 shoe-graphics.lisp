
;;----------------------------------------------------------------------------
;; This example demonstrates the uses of images in Graphics Ports.
;;   GP:READ-EXTERNAL-IMAGE to read an external-image from a file.
;;   GP:LOAD-IMAGE to convert an external-image to an image for drawing.
;;   GP:DRAW-IMAGE for drawing an image to an output-pane.
;;   GP:FREE-IMAGE for freeing an image.

#|
(setf *timer* (mp:make-timer 'print 4 *standard-output*))
;(mp:schedule-timer-relative-milliseconds *timer* 2000 )
;(mp:unschedule-timer *timer*)

(print (capi:prompt-for-file "Choose a bitmap"
                                      :pathname (pathname-location #.(current-pathname))
                                      :filter (second *image-file-filters*)
                                      :filters *image-file-filters*
                                      :ok-check #'(lambda (file)
                                                    (member (pathname-type file) *image-file-types* :test 'equalp))))
|#

(in-package "CL-USER")

;;----------------------------------------------------------------------------
;; Define an interface
;;----------------------------------------------------------------------------

(capi:define-interface virtual-shoe ()
  ((image :initform nil))
  (:panes
   (viewer capi:output-pane
           :display-callback 'display-test-image-functions
           :horizontal-scroll nil
           :vertical-scroll nil
           :visible-min-width 400 ;250 ;320
           :visible-min-height 233 ; 150 ;240
           )
   ;(controller capi:push-button-panel
   ;            :items '( :close)
   ;            :callbacks '(test-image-functions-close-image)
   ;            :callback-type :interface
   ;            :print-function 'string-capitalize)
   )
  (:layouts
   (main-layout capi:column-layout
                '(viewer ))) ;controller)))
  (:default-initargs 
   :title "Virtual Tapping Shoe"
   :layout 'main-layout
   :best-width 200
   :best-height 200))

;(defmethod initialize-instance :after ((self test-image-functions) &key
;                                       &allow-other-keys)
;  (update-test-image-functions-enabled self))

(defun update-test-image-functions (interface)
  (with-slots (viewer image) interface
    (gp:invalidate-rectangle viewer)
    (capi:set-horizontal-scroll-parameters viewer :min-range 0 :max-range (if image (gp:image-width image) 0))
    (capi:set-vertical-scroll-parameters viewer :min-range 0 :max-range (if image (gp:image-height image) 0))
   ; (update-test-image-functions-enabled interface)
    ))

#|
(defun update-test-image-functions-enabled (interface)
  (with-slots (controller image) interface
    (if image
        (capi:set-button-panel-enabled-items
         controller
         :set t)
      (capi:set-button-panel-enabled-items
       controller
       :set t
       :disable '(:close)))))

|#

(defun display-test-image-functions (pane x y width height)
  (with-slots (image) (capi:top-level-interface pane)
    (when image
      (when (gp:rectangle-overlap x y (+ x width) (+ y height)
                                  0 0 (gp:image-width image) (gp:image-height image))
        (gp:draw-image pane image 0 0)))))


(defun test-image-functions-change-image (interface file)
  (with-slots (viewer image) interface
      (when (and file (probe-file file))
        (let ((external-image (gp:read-external-image file)))
          (when image
            (gp:free-image viewer image))
          (setf image (gp:load-image viewer external-image))
          (update-test-image-functions interface)))))

(defun test-image-functions-close-image (interface)
  (with-slots (viewer image) interface
    (gp:free-image viewer (shiftf image nil))
    (update-test-image-functions interface)))


