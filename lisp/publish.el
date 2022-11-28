;; Initialize the package manager
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(normal-top-level-add-subdirs-to-load-path)

;; Packages to install from external archives
(package-install 'ox-reveal)

;; Load packages
(require 'ox-reveal)
(require 'ox-publish)

;;; Configurations:

;; Org caching:

(setq org-export-time-stamp-file nil
      org-publish-timestamp-directory ".cache/")

;; Org content metadata:

(setq org-html-metadata-timestamp-format "%B %d, %Y")

;; Org source blocks:

(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-html-htmlize-output-type nil)

;; Emacs:

(setq make-backup-files nil)

;; Metadata:

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "ba.tahaaziz@gmail.com")

(defun read-template (path)
  "Read a template from the  directory."
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat "src/templates" path))
    (buffer-string)))

(defun publish-html-head ()
  "<link rel=\"stylesheet\" href=\"https://grtcdr.tn/css/main.css\" />")

;;; Project specification:

(setq org-publish-project-alist
      (let ((content-preamble (read-template "preamble/content.html"))
	    (html-head (publish-html-head)))
	(list
	 (list "content"
	       :base-extension "org"
	       :base-directory "src"
	       :publishing-directory "public"
	       :publishing-function 'org-html-publish-to-html
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble content-preamble
	       :html-postamble nil
	       :html-head-extra html-head
	       :html-head-include-default-style nil)
	 (list "docs"
	       :base-extension "org"
	       :base-directory "src/docs"
	       :publishing-directory "public/docs"
	       :publishing-function 'org-html-publish-to-html
	       :makeindex t
	       :section-numbers t
	       :with-toc t
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble content-preamble
	       :html-postamble nil
	       :html-head-extra html-head
	       :html-head-include-default-style nil)
	 (list "presentations"
	       :base-extension "org"
	       :base-directory "src/presentations"
	       :publishing-directory "public/p"
	       :publishing-function 'org-reveal-publish-to-reveal
	       :recursive t
	       :with-toc t
	       :with-title t
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble nil
	       :html-postamble nil
	       :html-head-include-default-style nil)
	 (list "assets"
	       :base-extension "webp\\|png\\|jpe?g"
	       :base-directory "assets"
	       :publishing-directory "public/assets"
	       :publishing-function 'org-publish-attachment
	       :recursive t)
	 (list "all"
	       :components (list "content" "docs" "presentations" "assets")))))
