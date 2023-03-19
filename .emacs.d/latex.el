;;; latex.el --- LaTeX mode support -*- lexical-binding: t -*-

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
  (concat dbargman/research-base-dir "/bibliography")
  "Direcory for storing bibliography files."
  :group 'dbargman/research
  :type 'string
  :safe (lambda (_) t))

(defcustom dbargman/research-export-dir
  (concat dbargman/research-base-dir "/exports")
  "Direcory for storing bibliography files."
  :group 'dbargman/research
  :type 'string
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
  :config
  (pdf-tools-install))


;; bibliography management with ebib
(use-package ebib

  ;; include org-ebib
  :straight (ebib :files (:defaults "*") :includes (org-ebib))

  ;; set default directory and use biblatex dialect by default
  :custom
  (ebib-default-directory dbargman/research-bibliography-dir)
  (ebib-bibtex-dialect 'biblatex)

  )

;;; ADDITIONAL CONFIGURATIONS

(with-eval-after-load "org"

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
     "latexmk "
     "-f "
     "-pdf "
     "-%latex "
     "-new-viewer- "
     "-interaction=nonstopmode "
     "-output-directory=" dbargman/research-export-dir "/.tmp "
     "%f")
    )
   )

  )


(provide 'latex)

;;; latex.el ends here
