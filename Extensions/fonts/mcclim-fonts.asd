
#| dummy system to make Quicklisp happy |#
(defsystem #:mcclim-fonts)

(defsystem #:mcclim-fonts/truetype
  :depends-on (#:mcclim-clx #:zpb-ttf #:cl-vectors #:cl-paths-ttf #:cl-aa #:alexandria)
  :components
  ((:static-file "README.md")
   (:file "truetype-package")
   (:file "xrender-fonts" :depends-on ("mcclim-native-ttf" "truetype-package" "fontconfig"))
   (:file "fontconfig" :depends-on ("truetype-package"))
   (:file "mcclim-native-ttf" :depends-on ("truetype-package"))))

(defmethod perform :after ((o load-op)
                           (s (eql (find-system :mcclim-fonts/truetype))))
  (uiop:symbol-call :mcclim-truetype :autoconfigure-fonts))