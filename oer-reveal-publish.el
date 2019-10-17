;;; oer-reveal-publish.el --- Publish reveal.js presentations from Org sources  -*- lexical-binding: t; -*-
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2017-2019 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; License: GPLv3

;;; Commentary:
;; This file contains setup code to publish reveal.js presentations
;; from Org source files with oer-reveal.
;;
;; Function `oer-reveal-publish-all' is meant to be used in batch mode
;; (more details follow below) and invokes the standard Org export
;; function `org-publish-all' to publish all projects of
;; `org-publish-project-alist'.  That latter list of projects is
;; mostly set up by code in `oer-reveal-publish-all', but your
;; pre-existing projects are exported as well.
;; Org source files (except explicitly excluded ones) are published
;; according to `oer-reveal-publish-org-publishing-functions'.  Other
;; resources (e.g., reveal.js and plugins, CSS, figures) are copied with
;; `org-publish-attachment'.
;; The target directory is "public".
;;
;; If file "index.org" is present in the current directory (e.g., to
;; collect links to generated reveal.js presentations), it is added to
;; `org-publish-project-alist' for export to HTML (with the standard
;; HTML export back-end).
;; If existing, file "index.css" and directories among "audio",
;; "figures", "quizzes" are added to `org-publish-project-alist'
;; for export as attachment (copy).
;;
;; You may want to load/require this file from your own publish.el
;; with additional entries added to `org-publish-project-alist'.
;; Then, invoke publication based on your own publish.el:
;; emacs --batch --load publish.el --funcall oer-reveal-publish-all
;;
;; Warning!  By default, `oer-reveal-publish-all' let-binds
;; `org-confirm-babel-evaluate' to `oer-reveal-publish-confirm-evaluate',
;; which defaults to nil.
;; This enables automatic execution of code embedded in Org source
;; files.  This may be dangerous, but is useful for execution in batch
;; mode.  Set `oer-reveal-publish-confirm-evaluate' to t to be asked
;; for confirmation.
;; Also, `oer-reveal-publish-all' invokes the function specified in
;; `oer-reveal-publish-faces-function' to change faces for syntax
;; highlighting in batch mode; set that variable to nil to avoid such
;; changes.
;;
;; Function `oer-reveal-publish-setq-defaults' uses `setq' to change
;; various variables of other packages related to export to HTML and
;; LaTeX.  Please check what it does before invoking it.
;;
;; Originally inspired by publish.el by Rasmus:
;; https://gitlab.com/pages/org-mode/blob/master/publish.el

;;; Code:
(require 'org)
(require 'ox-publish)
(require 'oer-reveal)

(defcustom oer-reveal-publish-confirm-evaluate nil
  "Value to assign to `org-confirm-babel-evaluate' before export.
The default is nil, which may be dangerous and is not recommended for
general Emacs sessions."
  :group 'oer-reveal
  :type 'boolean)

(defcustom oer-reveal-publish-org-publishing-functions
  '(oer-reveal-publish-to-reveal org-latex-publish-to-pdf)
  "Functions to publish Org source files.
By default, Org files are published as reveal.js presentations and as
PDF.  For the latter, `org-latex-pdf-process' is set to
`oer-reveal-publish-pdf-process' in `oer-reveal-publish-setq-defaults'."
  :group 'oer-reveal
  :type '(repeat function))

(defcustom oer-reveal-publish-descriptive-links nil
  "Value to assign to `org-descriptive-links'.
Assignment happens in `oer-reveal-publish-setq-defaults'."
  :group 'oer-reveal
  :type 'boolean
  :package-version '(oer-reveal . "1.6.0"))

(defcustom oer-reveal-publish-makeindex nil
  "Value to use for makeindex option when publishing Org files."
  :group 'oer-reveal
  :type 'boolean)

(defcustom oer-reveal-publish-pdf-process
  '("latexmk -outdir=%o -interaction=nonstopmode -shell-escape -bibtex -pdf %f")
  "Value to assign to `org-latex-pdf-process'.
Assignment happens in `oer-reveal-publish-setq-defaults'."
  :group 'oer-reveal
  :type '(repeat string))

(defcustom oer-reveal-publish-figure-float "H"
  "Value to assign to `oer-reveal-latex-figure-float'.
Assignment happens in `oer-reveal-publish-setq-defaults'.
The default uses the LaTeX float package to position figures \"here\",
which results in a layout that is more similar to HTML slides.
See URL `https://ctan.org/pkg/float' for float documentation."
  :group 'oer-reveal
  :type 'string)

(defcustom oer-reveal-publish-html-doctype "html5"
  "Value to assign to variable `org-html-doctype'.
Assignment happens in `oer-reveal-publish-setq-defaults'."
  :group 'oer-reveal
  :type 'string)

(defcustom oer-reveal-publish-html-postamble
  "<p class=\"author\">License: This text, “<span property=\"dc:title\">%t</span>,” by <span property=\"dc:creator cc:attributionName\">%a</span> is published under the Creative Commons license <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a>.</p>
<p class=\"date\">Created: <span property=\"dc:created\">%C</span></p>
<div class=\"legalese\"><p><a href=\"/imprint.html\">Imprint</a> | <a href=\"/privacy.html\">Privacy Policy</a></p></div>"
  "Value to assign to `org-html-postamble' before export.
Assignment happens in `oer-reveal-publish-setq-defaults'.
The default generates CC BY-SA 4.0 license information and links to
imprint and privacy policy."
  :group 'oer-reveal
  :type 'string)

(defun oer-reveal-publish-faces ()
  "Call `custom-set-faces' for syntax highlighting in batch mode.
Invoked from function `oer-reveal-publish-all'."
  ;; The following colors are based on the tango custom theme.
  (custom-set-faces
   '(default                      ((t (:foreground "#2e3436"))))
   '(font-lock-builtin-face       ((t (:foreground "#75507b"))))
   '(font-lock-comment-face       ((t (:foreground "#5f615c"))))
   '(font-lock-constant-face      ((t (:foreground "#204a87"))))
   '(font-lock-function-name-face ((t (:bold t :foreground "#a40000"))))
   '(font-lock-keyword-face       ((t (:foreground "#346604"))))
   '(font-lock-string-face        ((t (:foreground "#5c3566"))))
   '(font-lock-type-face          ((t (:foreground "#204a87"))))
   '(font-lock-variable-name-face ((t (:foreground "#b35000"))))
   ))

(defcustom oer-reveal-publish-faces-function #'oer-reveal-publish-faces
  "Function to change faces for syntax highlighting.
This function is called in `oer-reveal-publish-all'.  Set to nil for default
syntax highlighting."
  :group 'oer-reveal
  :type '(choice (const nil) function))

(defcustom oer-reveal-publish-latex-packages
  '(
    ;; Setup url package with hyphens option.  This is done here to avoid
    ;; option clashes when implicitly loading the package from hyperref.
    ("hyphens" "url" nil)
    ;; Load float package.  This must come before hyperref to avoid
    ;; warnings for figures:
    ;; warning (ext4): destination with the same identifier
    ("" "float" nil))
  "Packages to add to beginning of `org-latex-default-packages-alist'.
Assignment happens in `oer-reveal-publish-setq-defaults'."
  :group 'oer-reveal
  :type '(repeat
	  (choice
	   (list :tag "options/package pair"
		 (string :tag "options")
		 (string :tag "package")
		 (boolean :tag "Snippet")
		 (choice
		  (const :tag "For all compilers" nil)
		  (repeat :tag "Allowed compiler" string)))
	   (string :tag "A line of LaTeX"))))

(defcustom oer-reveal-publish-babel-languages '((emacs-lisp . t))
  "Babel languages to activate in `oer-reveal-publish-setq-defaults'."
  :group 'oer-reveal
  :type '(repeat
          (cons (symbol :tag "language")
                (choice
                 (const :tag "Enabled" t)
                 (const :tag "Disabled" nil)))))

(require 'table)
;;;###autoload
(defun oer-reveal-publish-setq-defaults ()
  "Change various variables with `setq'."
  (setq table-html-th-rows 1
	table-html-table-attribute "class=\"emacs-table\""
        org-entities-user '(("textbackslash" "\\textbackslash{}" nil "\\" "\\" "\\" "\\"))
	org-html-table-default-attributes nil
	org-html-doctype oer-reveal-publish-html-doctype
	org-html-postamble oer-reveal-publish-html-postamble
        org-descriptive-links oer-reveal-publish-descriptive-links
	oer-reveal-latex-figure-float oer-reveal-publish-figure-float
	org-re-reveal-script-files oer-reveal-script-files
	org-re-reveal--href-fragment-prefix org-re-reveal--slide-id-prefix
	org-re-reveal-body-attrs
	"prefix=\"dc: http://purl.org/dc/terms/ cc: http://creativecommons.org/ns#\""
	org-latex-pdf-process oer-reveal-publish-pdf-process
	;; Add packages that need to be at the beginning of
	;; org-latex-default-packages-alist.
	org-latex-default-packages-alist
	(append oer-reveal-publish-latex-packages
		org-latex-default-packages-alist)
        )
  (org-babel-do-load-languages
   'org-babel-load-languages oer-reveal-publish-babel-languages)
  )

(defun oer-reveal-publish-klipse-projects ()
  "Compute project list for local klipse libraries."
  (let (result)
      (push (list "klipse-libs"
		  :base-directory (expand-file-name
				   "klipse-libs/klipse-dist"
				   oer-reveal-submodules-dir)
		  :base-extension 'any
		  :publishing-directory "./public/reveal.js/plugin/klipse"
		  :publishing-function 'org-publish-attachment)
            result)
      (push (list "codemirror"
		  :base-directory (expand-file-name
				   "klipse-libs/python"
				   oer-reveal-submodules-dir)
		  :base-extension 'any
		  :publishing-directory "./public/reveal.js/plugin/klipse/python"
		  :publishing-function 'org-publish-attachment
                  :recursive t)
            result)
      (push (list "skulpt"
		  :base-directory (expand-file-name
				   "klipse-libs/skulpt-dist"
				   oer-reveal-submodules-dir)
		  :base-extension "js"
                  :exclude "debug.*"
		  :publishing-directory "./public/reveal.js/plugin/klipse"
		  :publishing-function
                  'org-publish-attachment)
            result)))

(defun oer-reveal-publish-plugin-projects ()
  "Compute list of plugin projects for `org-publish-project-alist'.
For each plugin in `oer-reveal-plugins', add what to publish."
  (let (result)
    (when (member "reveal.js-coursemod" oer-reveal-plugins)
      (push (list "reveal.js-coursemod"
		  :base-directory (expand-file-name
				   "reveal.js-coursemod/coursemod"
				   oer-reveal-submodules-dir)
		  :base-extension 'any
		  :publishing-directory "./public/reveal.js/plugin/coursemod"
		  :publishing-function 'org-publish-attachment
		  :recursive t)
            result))
    (when (member "reveal.js-jump-plugin" oer-reveal-plugins)
      (push (list "reveal.js-jump-plugin"
		  :base-directory (expand-file-name
				   "reveal.js-jump-plugin/jump"
				   oer-reveal-submodules-dir)
		  :base-extension 'any
		  :publishing-directory "./public/reveal.js/plugin/jump"
		  :publishing-function 'org-publish-attachment
		  :recursive t)
            result))
    (when (member "reveal.js-plugins" oer-reveal-plugins)
      (push (list "reveal.js-plugins-anything"
		  :base-directory (expand-file-name
				   "reveal.js-plugins/anything"
				   oer-reveal-submodules-dir)
		  :base-extension 'any
		  :publishing-directory "./public/reveal.js/plugin/anything"
		  :publishing-function 'org-publish-attachment
		  :recursive t)
            result)
      (push (list "reveal.js-plugins-audio-slideshow"
		  :base-directory (expand-file-name
				   "reveal.js-plugins/audio-slideshow"
				   oer-reveal-submodules-dir)
		  :base-extension 'any
		  :publishing-directory "./public/reveal.js/plugin/audio-slideshow"
		  :publishing-function 'org-publish-attachment
		  :recursive t)
            result))
    (when (member "reveal.js-quiz" oer-reveal-plugins)
      (push (list "reveal.js-quiz-plugin"
		  :base-directory (expand-file-name
				   "reveal.js-quiz/quiz"
				   oer-reveal-submodules-dir)
		  :base-extension 'any
		  :publishing-directory "./public/reveal.js/plugin/quiz"
		  :publishing-function 'org-publish-attachment
		  :recursive t)
            result))
    (when (member "Reveal.js-TOC-Progress" oer-reveal-plugins)
      (push (list "reveal-toc-plugin"
		  :base-directory (expand-file-name
				   "Reveal.js-TOC-Progress/plugin"
				   oer-reveal-submodules-dir)
		  :base-extension 'any
		  :publishing-directory "./public/reveal.js/plugin"
		  :publishing-function 'org-publish-attachment
		  :recursive t)
            result))
    (when (member "klipse-libs" oer-reveal-plugins)
      (nconc result (oer-reveal-publish-klipse-projects)))
    result))

(defun oer-reveal-publish-optional-projects ()
  "Compute list of optional projects for `org-publish-project-alist'.
These are \"index.org\" to be published with `org-html-publish-to-html'
as well as \"index.css\" and the directories \"audio\", \"figures\",
\"quizzes\" to be published with `org-publish-attachment'."
  (let (result)
    (when (file-exists-p "index.org")
      (push (list "index"
		  :base-directory "."
		  :include '("index.org")
		  :exclude ".*"
		  :publishing-function '(org-html-publish-to-html)
		  :publishing-directory "./public")
            result))
    (when (file-exists-p "index.css")
      (push (list "index-css"
		  :base-directory "."
		  :include '("index.css")
		  :exclude ".*"
		  :publishing-function '(org-publish-attachment)
		  :publishing-directory "./public")
            result))
    (when (file-accessible-directory-p "audio")
      (push (list "audio"
		  :base-directory "audio"
		  :base-extension (regexp-opt '("ogg" "mp3"))
		  :publishing-directory "./public/audio"
		  :publishing-function 'org-publish-attachment)
            result))
    (when (file-accessible-directory-p "figures")
      (push (list "figures"
		  :base-directory "figures"
		  :base-extension (regexp-opt '("png" "jpg" "ico" "svg" "gif"))
		  :publishing-directory "./public/figures"
		  :publishing-function 'org-publish-attachment
		  :recursive t)
            result))
    (when (file-accessible-directory-p "quizzes")
      (push (list "quizzes"
		  :base-directory "quizzes"
		  :base-extension (regexp-opt '("js"))
		  :publishing-directory "./public/quizzes"
		  :publishing-function 'org-publish-attachment)
            result))
    result))

(defun oer-reveal-publish-all (&optional project-alist)
  "Configure settings and invoke `org-publish-all'.
Invoke function `oer-reveal-publish-faces-function' if non-nil,
let-bind `org-confirm-babel-evaluate' to
`oer-reveal-publish-confirm-evaluate', set up
`org-publish-project-alist', and invoke `org-publish-all'.
Before publication, `org-publish-project-alist' contains the following:
* Org files for publication as reveal.js presentations.
* Resources of directory title-slide, to be published as attachments.
* Reveal.js and plugins, to be published as attachments.
* Optional resources as defined by `oer-reveal-publish-optional-projects'.
* Original contents of `org-publish-project-alist'.
* Optional PROJECT-ALIST."
  (interactive)
  (when oer-reveal-publish-faces-function
    (funcall oer-reveal-publish-faces-function))
  (let ((org-confirm-babel-evaluate oer-reveal-publish-confirm-evaluate)

	;; Export different parts of Org presentations to sub-directory
	;; "public".  Org presentations are exported according to
	;; `oer-reveal-publish-org-publishing-functions'.
	;; Other parts are just copied with `org-publish-attachment'.
	(org-publish-project-alist
	 (append
	  (list
	   (list "org-presentations"
		 :base-directory "."
		 :base-extension "org"
		 :makeindex oer-reveal-publish-makeindex
		 :exclude "index\\|backmatter\\|config\\|course-list\\|license-template\\|imprint\\|privacy\\|README\\|CONTRIBUTING\\|CHANGELOG"
		 :publishing-function oer-reveal-publish-org-publishing-functions
		 :publishing-directory "./public")
	   (list "title-slide"
		 :base-directory (expand-file-name "title-slide" oer-reveal-dir)
		 :base-extension (regexp-opt '("png" "jpg" "svg"))
		 :publishing-directory "./public/title-slide/"
		 :publishing-function 'org-publish-attachment)
	   (list "reveal-theme"
		 :base-directory (expand-file-name "css" oer-reveal-dir)
		 :base-extension 'any
		 :publishing-directory "./public/reveal.js/css/theme"
		 :publishing-function 'org-publish-attachment)
	   (list "reveal-static"
		 :base-directory (expand-file-name
				  "reveal.js" oer-reveal-submodules-dir)
		 :exclude "\\.git"
		 :base-extension 'any
		 :publishing-directory "./public/reveal.js"
		 :publishing-function 'org-publish-attachment
		 :recursive t))
          (oer-reveal-publish-plugin-projects)
          (oer-reveal-publish-optional-projects)
          org-publish-project-alist
	  project-alist)))
    (org-publish-all)))

(provide 'oer-reveal-publish)
;;; oer-reveal-publish.el ends here
