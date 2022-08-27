;;; publish.el --- prepares websites for publishing -*- lexical-binding: t; -*-

;; Author: Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; URL: https://github.com/grtcdr/grtcdr.github.io

;; publish.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; publish.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with publish.el.  If not, see <https://www.gnu.org/licenses/>.

;; This file contains code taken from GNU Emacs, which is
;; Copyright (C) 1976-2022 Free Software Foundation, Inc.

;;; Commentary:

;; publish.el specifies the structure of a website built atop
;; org-publish-project-alist.  It should also specify any other
;; options that affect the building of the website.

(require 'ox-publish)

;;; Configurations:

;; Org caching:

(setq org-export-time-stamp-file nil
      org-publish-timestamp-directory ".timestamps/")

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

(defun read-snippet (directory file)
  "Read a snippet from the snippets directory."
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat "snippets" directory file))
    (buffer-string)))

;;; Project specification:

(setq org-publish-project-alist
      (let ((content-preamble (read-snippet "preamble" "content.html")))
	(list
	 (list "content"
	       :base-extension "org"
	       :base-directory "."
	       :publishing-directory "public"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("README.org" "setup.org"))
	       :section-numbers nil
	       :with-toc nil
	       :with-title t
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble content-preamble
	       :html-postamble nil
	       :html-head-include-default-style nil)
	 (list "stylesheets"
	       :base-extension "css"
	       :base-directory "stylesheets" 
	       :publishing-directory "public/stylesheets"
	       :publishing-function 'org-publish-attachment)
	 (list "all"
	       :components (list "content"
				 "stylesheets")))))
