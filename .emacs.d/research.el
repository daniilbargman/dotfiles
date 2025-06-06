;;; research.el --- academic research module -*- lexical-binding: t -*-

;; Author: Daniil Bargman
;; Maintainer: Daniil Bargman
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: convenience


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

;;; CUSTOMISATIONS

(defgroup dbargman/research nil
  "Group for configuring research projects."
  :group 'External
  :prefix 'dbargman/research
  :version '0.1.0)

(defcustom dbargman/research-base-dir (file-truename "~/Research")
  "Base directory for all research projects."
  :group 'dbargman/research
  :type 'string
  :safe (lambda (_) t))

(defcustom dbargman/research-bibliography-dir
  (expand-file-name "bibliography" dbargman/research-base-dir)
  "Direcory for storing bibliography files."
  :group 'dbargman/research
  :type 'string
  :safe (lambda (_) t))

(defcustom dbargman/research-bibliography-pdf-dir
  (expand-file-name "PDFs" dbargman/research-bibliography-dir)
  "Direcory for storing bibliography files."
  :group 'dbargman/research
  :type 'string
  :safe (lambda (_) t))

(defcustom dbargman/research-bibliography-files
  (directory-files dbargman/research-bibliography-dir t ".*\.bib$")
  "list of .bib files in the research directory."
  :group 'dbargman/research
  :type 'list
  :safe (lambda (_) t))

(defcustom dbargman/research-export-dir
  (expand-file-name "exports" dbargman/research-base-dir)
  "Direcory for storing bibliography files."
  :group 'dbargman/research
  :type 'string
  :safe (lambda (_) t))

(defcustom dbargman/research-markups-dir
  (expand-file-name "markups" org-roam-directory)
  "Direcory for storing bibliography files.

Created as a subfolder inside 'org-roam-directory', unless an absolute
  path is specified."
  :group 'dbargman/research
  :type 'string
  :set (lambda
	 (symbol value)
	 (set-default-toplevel-value
	  symbol
	  (expand-file-name value org-roam-directory)
	  )
	 )
  :safe (lambda (_) t))


;;; PACKAGES

;; auctex
(use-package tex
  :straight auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  )

;; pdf-tools for latex export
(use-package pdf-tools
  :init (pdf-tools-install)
  :config
  (add-hook 'pdf-view-mode-hook
	    #'(lambda () (display-line-numbers-mode -1)))
  )

;; bibliography management with ebib
(use-package ebib

  ;; include org-ebib
  :straight (ebib :files (:defaults "*") :includes (org-ebib))

  ;; set default directory and use biblatex dialect by default
  :custom
  (ebib-bibtex-dialect 'biblatex)

  (ebib-preload-bib-files dbargman/research-bibliography-files)
  (ebib-bib-search-dirs (list (file-truename "~/Downloads/")))
  (ebib-file-search-dirs
   (list
    (expand-file-name "PDFs" dbargman/research-bibliography-dir)
    )
   )
  (ebib-truncate-file-names nil)

  ;; this setting is only relevant when importing files into the
  ;; database. Bib files tend to be downloaded into "~/Downloads", which
  ;; is also the first directory in 'ebib-bib-search-dirs'.
  (ebib-default-directory 'first-bib-dir)

  )

;; add biblio for reference lookup and import
(use-package biblio
  :config
  (with-eval-after-load "personalizations"
    (setq biblio-crossref-user-email-address user-mail-address)
    )
  )


;;; CITATION SUPPORT IN ORG AND LaTeX
;;; SEE: https://kristofferbalintona.me/posts/202206141852/

;; use citar
(use-package citar
  ;; The `:straight' keyword is not necessary. However, I do this to set
  ;; a value for the `:includes' keyword. This keyword tells use-package
  ;; that those package(s) are provided by this package, and not to
  ;; search for them on Melpa for download. Alternatively, you can set
  ;; the `:straight' keyword to nil in those package(s) use-package
  ;; declaration.
  :straight (citar :type git :host github :repo "emacs-citar/citar"
		   :includes (citar-org))
  :custom
  ;; A list of bib files. A good idea would be having its value be
  ;; identical to that of `org-cite-global-bibliography'. For me, I have
  ;; all my bib file(s) as a list of strings in `kb/bib-files'.
  (citar-bibliography dbargman/research-bibliography-files)
  (citar-library-paths (list dbargman/research-bibliography-pdf-dir))

  ;; setup completions for citar using capf
  :hook
  (org-mode . citar-capf-setup)
  (LaTeX-mode . citar-capf-setup)

  )

;; integrate with embark
(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode)
  )


;;; ADDITIONAL CONFIGURATIONS FOR ORG COMPATIBILITY

(with-eval-after-load "org"

;;; ORG INTEGRATION

  ;; prefer user-defined labels
  (setq org-latex-prefer-user-labels t)

  ;; use biblatex as the export processor for latex
  (setq org-cite-export-processors '((latex biblatex) (t basic)))

  ;; org-cite config
  (setq org-cite-global-bibliography
	dbargman/research-bibliography-files)

  ;; Use `citar' with `org-cite'
  (use-package citar-org
    :after oc

    :custom

    ;; use citar as org-cite processor
    (org-cite-insert-processor 'citar)
    (org-cite-follow-processor 'citar)
    (org-cite-activate-processor 'citar)

    )

  ;; org-noter for annotating PDFs directly
  (use-package org-noter

    :custom
    (org-noter-highlight-selected-text t)
    (org-noter-auto-save-last-location t)
    (org-noter-window-location 'vertical-split)
    )


  ;; ORG ROAM INTEGRATION

  ;; add org-roam integration for org-roam-bibtex for cite integration
  (use-package org-roam-bibtex
    ;; :hook (org-mode . org-roam-bibtex-mode)

    :custom

    ;; set bibliography
    (bibtex-completion-bibliography
     dbargman/research-bibliography-files)

    ;; go with org-cite
    (orb-roam-ref-format 'org-cite)

    ;; ;; NOTE: these values are required for org-noter integration in
    ;; ;; org-roam notes as per the orb manual but are the defaults anyway
    ;; (orb-process-file-keyword t)
    ;; (orb-attached-file-extensions '("pdf"))

    ;; :config

    ;; ;; NOTE: this has been commented out because the same can be
    ;; ;; achieved with 'citar-org-roam-template-keys'
    ;; ;; need to add keywords if using them as filetags in roam template
    ;; (add-to-list 'orb-preformat-keywords "keywords")

    )

  ;; org-roam and citar integration
  (use-package citar-org-roam

    :after (citar org-roam org-roam-bibtex)

    ;; :hook (org-roam-bibtex-mode . citar-org-roam-mode)

    :custom

    ;; make sure citar creates notes as roam nodes
    (citar-open-note-function 'orb-citar-edit-note)


    :config

    (citar-org-roam-setup)

    )

  ;; add keywords to citar template keys
  (setq citar-org-roam-template-fields
	'(
	  (:citar-title "title") (:citar-author "author" "editor")
	  (:citar-date "date" "year" "issued") (:citar-pages "pages")
	  (:citar-type "type")
	  (:citar-document-title "title")
	  (:citar-citekey "citekey")
	  (:citar-keywords "keywords")
	  (:citar-file "file")
	  (:citar-url "url")
	  )
	)

  ;; note title template (can be overridden in personalizations too)
  (setq citar-org-roam-note-title-template
	"${author editor} (${year})")


  ;; change roam directory for notes per individual session config
  (advice-add
   'ecm-session-init
   :after
   #'(lambda (&rest _)
       (custom-set-variables
	`(citar-notes-paths (list org-roam-directory))
	)
       )
   )


  ;; LATEX CONFIGURATION FOR RESEARCH EXPORT

  ;; redefine what a report should look like when transpiled to LaTeX
  (custom-set-variables
   '(org-latex-classes
     '(
       ("article" "\\documentclass[11pt]{article}"
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	("\\paragraph{%s}" . "\\paragraph*{%s}")
	("\\subparagraph{%s}" . "\\subparagraph*{%s}")
	)
       ("report" "\\documentclass[11pt,bibliography=numbered]{report}"
	("\\chapter{%s}" . "\\chapter*{%s}")
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	)
       ("book" "\\documentclass[11pt]{book}"
	("\\part{%s}" . "\\part*{%s}")
	("\\chapter{%s}" . "\\chapter*{%s}")
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	)
       ("elsarticle" "\\documentclass[preprint,12pt]{elsarticle}"
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	("\\paragraph{%s}" . "\\paragraph*{%s}")
	("\\subparagraph{%s}" . "\\subparagraph*{%s}")
	)
       ("econometrica" "\\documentclass[ecta,nameyear,draft]{econsocart}"
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	("\\paragraph{%s}" . "\\paragraph*{%s}")
	("\\subparagraph{%s}" . "\\subparagraph*{%s}")
	)
       )
     )
   )



  ;; nicer latex formula sizes
  (plist-put org-format-latex-options :scale 1.5)
  (plist-put org-format-latex-options :html-scale 0.9)

  ;; store latex images in a dedicated directory
  (setq
   org-preview-latex-image-directory
   (concat dbargman/research-export-dir "/.tmp")
   )

  ;; adjust pdf processing command to auto-update
  (setq
   org-latex-pdf-process
   (list
    (concat
     "rm -f %o/%b.bbl && "
     "latexmk "
     "-f "
     "-pdf "
     "-%latex "
     "-new-viewer- "
     "-interaction=nonstopmode "
     "-output-directory=%o "
     "%f")
    )
   )

  ;; export org-LaTeX to PDF without the extra visual clutter
  (defun dbargman/org-latex-export-to-pdf (p)
    "Export Org buffer to a latex PDF, bypassing interactive menus.

This function is a shorthand for 'org-latex-export-to-pdf' which
bypasses two distractions:

1. No interactive export menu is presented.
2. No additional confirmation prompts pop up when a PDF is re-generated.

For the second point, this function relies on 'auto-revert-mode'.

If the PDF buffer is already visible, it is simply updated on-screen.
Otherwise,envoking with a prefix places the PDF buffer in a dedicated
vertical split on the right, whereas running without a prefix places it
in the 'other window'."
    (interactive "P")
    (let* (
	   ;; this command exports to PDF asynchronously and returns the
	   ;; file name
	   (pdf-preview-file
	    (if (and (boundp 'org-beamer-mode) org-beamer-mode)
		(org-beamer-export-to-pdf)
	      (org-latex-export-to-pdf)
	      )
	    )
	   (pdf-preview-buffer (find-buffer-visiting pdf-preview-file))
	   (pdf-preview-window
	    (when pdf-preview-buffer
	      (get-buffer-window pdf-preview-buffer)
	      )
	    )
	   )

      ;; ;; if the buffer is visible, do nothing
      (unless pdf-preview-window

	;; ;; DEPRECATED: if a buffer exists, show it in a vertical split
	;; (if pdf-preview-buffer
	;;     (switch-to-buffer-other-window pdf-preview-buffer)
	;;   (evil-window-vsplit nil pdf-preview-file)
	;; )

	;; show buffer in "other" window if no prefix, vsplit otherwise
	(when (or p (= (length (window-list)) 1))
	  (evil-window-vsplit) (evil-window-move-far-right))
	(evil-window-prev 0)
	(if pdf-preview-buffer
	    (switch-to-buffer pdf-preview-buffer nil t)
	  (find-file pdf-preview-file)
	  )

	)

      )
    )


  )


(provide 'research)

;;; research.el ends here
