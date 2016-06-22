;; this file is only required to regenerate the README.md. Load this
;; after native-usb.lisp and call update-markdown-readme whenever new
;; functions were added or documentation was changed.

;; alternatively, if you native-usb.asd is linked into a subfolder of
;; quicklisp/local-projects, this file should load in slime with C-c
;; C-k

(eval-when (:compile-toplevel)
  (ql:quickload :mgl-pax)
  (ql:quickload :ulisp))

(mgl-pax:define-package :ulisp-doc
    (:documentation "C-mera s-expressions to generate Ulisp C code")
  (:use #:cl #:mgl-pax))

(in-package :ulisp-doc)


(mgl-pax:defsection @usage-sec (:title "Usage")
  "```common-lisp
 (eval-when (:load-toplevel :execute :compile-toplevel)
   (ql:quickload :native-usb))
 (in-package :native-usb)
 (with-open-usb (s #x10c4 :product-id #x87a0)
   (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
     (usb-control-msg s #xc0 #x22 0 0 buf)))
 ```"
  (calc-builtin-name-max-len function))

(mgl-pax:defsection @compilation-sec (:title "Compilation")
  "This code uses C2FFI to parse the header file linux/usbdevice_fs.h
  and generate the foreign function interface for usbdevfs. It
  generates a few files in /tmp. If your architecture is AMD64,
  running C2FFI again is not required. It should suffice to load
  native-usb-ffi.lisp with the definitions that C2FFI generated on my
  system. In this case the c2ffi binary is not required.")

(mgl-pax:defsection @doc-sec (:title "Updating Documentation")
  "I use mgl-pax to generate the documentation. Whenever docstrings
  have been changed or new functions were added, update and run the
  code in gendoc.lisp to update the README.md file.")


(mgl-pax:defsection @native-usb-manual (:title "Native USB manual")
  "This is an pure Common Lisp interface for Linux USB. It requires SBCL because I rely on its internals `sb-sys:with-pinned-objects` and `sb-sys:vector-sap`. I use c2ffi and cl-autowrap to obtain the required IOCTL type and constant definitions. However, if you are using AMD64 you might not have to install c2ffi.

For debugging and functional verification I use `sudo modprobe usbmon` and `wireshark`.
"
  (@install-sec section)
  (@usage-sec section)
  (@compilation-sec section)
  (@doc-sec section))




(defun update-markdown-readme ()
 (with-open-file (s (merge-pathnames "quicklisp/local-projects/sb-look-ma-no-libusb/README.md" (user-homedir-pathname))
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (mgl-pax:document (list @native-usb-manual) :stream s :format :markdown
    )))

#+nil
(update-markdown-readme)
