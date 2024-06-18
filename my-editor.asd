(asdf:defsystem #:my-editor
  :version "0.0.1"
  :depends-on (#:my-editor/api
               #:trivial-garbage
               #:cl-sdl3
               #:kslgui
               #:state-dependent-effect-tree
               #:global-vars
               #:clingon
               #:cl-ppcre)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "my-editor"
  :entry-point "my-editor:main"
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "project")
                             (:module "gfx"
                              :components ((:file "util")
                                           (:file "sdl-surface")
                                           (:file "sdl-renderer")))
                             (:module "gui"
                              :components ((:file "menubar")
                                           (:file "menu")
                                           (:file "tabs")
                                           (:file "activities")))
                             (:module "app"
                              :components ((:file "activities")
                                           (:file "windows")
                                           (:file "context")
                                           (:file "loop")
                                           (:file "main")))
                             (:module "activities"
                              :components ((:file "messages")
                                           (:file "repl")
                                           (:file "inspector")
                                           (:file "file-browser")))))
               (:module "data"
                :components ((:module "fonts"
                              :components ((:static-file "Montserrat.ttf")
                                           (:static-file "Montserrat.LICENSE.txt")
                                           (:static-file "IBMPlexMono.ttf")
                                           (:static-file "IBMPlexMono.LICENSE.txt")))))))

(asdf:defsystem #:my-editor/api
  :version "0.0.1"
  :depends-on (#:kslgui)
  :serial t
  :components ((:module "src"
                :components ((:module "api"
                              :components ((:file "package")
                                           (:file "util")
                                           (:file "gfx")
                                           (:file "editor")
                                           (:file "gui")
                                           (:file "command")
                                           (:file "activity")))))))

(asdf:defsystem #:my-editor/opengl
  :version "0.0.1"
  :depends-on (#:kslgui
               #:my-editor/api
               #:my-editor
               #:cl-opengl
               #:bordeaux-threads
               #:global-vars)
  :serial t
  :components ((:module "src"
                :components ((:module "gfx"
                              :components ((:file "opengl")))
                             (:module "util"
                              :components ((:file "barrier")))))))
