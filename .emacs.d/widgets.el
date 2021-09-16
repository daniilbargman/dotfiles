;;; widgets.el --- app widgets for Emacs -*- lexical-binding: t -*-

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

;; additional widgets for Emacs, e.g. better browser, terminal, etc.

;;; Code:

;;; DISABLED IN FAVOUR OF EAF
;; ;; improved xwidgets-based webkit
;; (use-package xwwp
;;   :config
;;   (use-package xwwp-follow-link-ivy)
;;   :custom
;;   (xwwp-follow-link-completion-system 'ivy)
;;   ;; :bind (:map xwidget-webkit-mode-map
;;   ;;             ("v" . xwwp-follow-link))
;;   )

;;; browser and other widgets via EAF
(use-package eaf
  :straight
  (eaf :type git
       :host github
       :repo "manateelazycat/emacs-application-framework"
       :files ("*"))
  :init
  (use-package epc :defer t)
  (use-package ctable :defer t)
  (use-package deferred :defer t)
  (use-package s :defer t)
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true")
  ;; (eaf-bind-key scroll_up "C-u" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-d" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding) ;; unbind, see more in the Wiki

  )

;; ;; using straight.el - THIS DOESN'T WORK WITHER
;; (straight-use-package
;;  '(eaf :type git
;;        :host github
;;        :repo "manateelazycat/emacs-application-framework"
;;        :files ("*.el" "*.py" "core" "app"))
;;  )
;; (use-package epc :defer t)
;; (use-package ctable :defer t)
;; (use-package deferred :defer t)
;; (use-package s :defer t)
;; (setq eaf-browser-continue-where-left-off t)
;; (eaf-setq eaf-browser-enable-adblocker "true")
;; ;; (eaf-bind-key scroll_up "C-u" eaf-pdf-viewer-keybinding)
;; ;; (eaf-bind-key scroll_down "C-d" eaf-pdf-viewer-keybinding)
;; ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;; (eaf-bind-key nil "M-q" eaf-browser-keybinding) ;; unbind, see more in the Wiki


(provide 'widgets)

;;; widgets.el ends here
