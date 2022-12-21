(require 'ox-publish)
(require 'shr)

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn"
      make-backup-files nil)

(setq org-publish-timestamp-directory ".cache/"
      org-export-time-stamp-file nil
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-html-postamble nil
      org-html-head-include-default-style nil
      org-html-htmlize-output-type nil
      org-html-metadata-timestamp-format "%B %d, %Y")

(defun site/sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date."
  (format "[[file:%s][%s]]"
	  entry
	  (org-publish-find-title entry project)))

(defun site/link (href)
  "Format the stylesheet located at HREF as a ’link’ tag."
  (shr-dom-to-xml
   `(link ((rel . "stylesheet")
	   (href . ,href)))))

(defvar site/html-head
  (concat
   (site/link "https://grtcdr.tn/css/common.css")
   (site/link "https://grtcdr.tn/css/heading.css")
   (site/link "https://grtcdr.tn/css/source.css")
   (site/link "https://grtcdr.tn/css/table.css")
   (site/link "https://grtcdr.tn/css/nav.css")
   (site/link "https://grtcdr.tn/css/org.css")
   (shr-dom-to-xml '(link ((rel . "icon")
			   (type . "image/x-icon")
			   (href . "/assets/favicon.ico")))))
  "HTML headers shared across projects.")

(defvar site/preamble
  (shr-dom-to-xml
   '(nav nil
	 (ul nil
	     (li nil
		 (a ((href . "/index.html"))
		    "home"))
	     (li nil
		 (a ((href . "/practicum/theindex.html"))
		    "practicum"))
	     (li nil
		 (a ((href . "/zettelkasten/sitemap.html"))
		    "zettelkasten")))))
  "Define an HTML snippet/template used as a preamble across all projects.")

(setq org-publish-project-alist
      (list
	 (list "content"
	       :base-extension "org"
	       :base-directory "src"
	       :publishing-directory "public"
	       :publishing-function 'org-html-publish-to-html
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble site/preamble
	       :html-head-extra site/html-head)
	 (list "practicum"
	       :base-extension "org"
	       :base-directory "src/practicum"
	       :publishing-directory "public/practicum"
	       :publishing-function 'org-html-publish-to-html
	       :makeindex t
	       :section-numbers t
	       :with-toc t
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble site/preamble
	       :html-head-extra site/html-head)
	 (list "zettelkasten"
	       :base-extension "org"
	       :base-directory "src/zettelkasten"
	       :publishing-directory "public/zettelkasten"
	       :publishing-function 'org-html-publish-to-html
	       :auto-sitemap t
	       :with-toc nil
	       :sitemap-title "Zettelkasten"
	       :sitemap-format-entry 'site/sitemap-format-entry
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble site/preamble
	       :html-head-extra site/html-head)
	 (list "assets"
	       :base-extension "webp\\|png\\|jpe?g\\|svg\\|ico"
	       :base-directory "assets"
	       :publishing-directory "public/assets"
	       :publishing-function 'org-publish-attachment
	       :recursive t)
	 (list "all"
	       :components (list "content" "practicum" "zettelkasten" "assets"))))
