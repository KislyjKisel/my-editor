(in-package #:my-editor)

(defun cli-top-level/options ()
  (list
   (clingon:make-option
     :string
     :description "project's metadata file path"
     :short-name #\p
     :long-name "project"
     :key :project)
  ;  (clingon:make-option
  ;    :boolean/true
  ;    :description "refresh dependencies"
  ;    :short-name #\r
  ;    :long-name "refresh"
  ;    :key :refresh)
    ; (clingon:make-option
    ;   :string
    ;   :description "project's ASD file path"
    ;   :short-name #\a
    ;   :long-name "asd"
    ;   :key :asd)
   ;  (clingon:make-option
   ;    :string
   ;    :description "project's ASD system name"
   ;    :short-name #\p
   ;    :long-name "system"
   ;    :key :system)
   ))

(defun cli-top-level/handler (cmd)
  (let ((args (clingon:command-arguments cmd))
        (project-meta-path (clingon:getopt cmd :project)))
    (when (< 0 (length args))
          (format t "Invalid arguments: ~a~%" args)
          (return-from cli-top-level/handler))
    (cond
     ((not (null project-meta-path)) (main-loop (load-project project-meta-path)))
     (t (format t "Unimplemented~%")))))

(defun cli-top-level/command ()
  (clingon:make-command
    :name "my-editor"
    :version "0.0.1"
    :license "BSD 3-Clause"
    :usage "[-p <PROJECT>]"
    :options (cli-top-level/options)
    :handler #'cli-top-level/handler))

(defun main* (s)
  (cli-top-level/handler
   (clingon:parse-command-line (cli-top-level/command)
                               (ppcre:all-matches-as-strings "(?<=\")(?:\\\\\"|[^\"])*(?=\")|[^\\s\"]+" s))))

(export 'main)
(defun main ()
  (ignore-errors (clingon:run (cli-top-level/command))))
