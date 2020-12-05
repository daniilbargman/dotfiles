;;; k8s.el --- Kubernetes integration -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  <daniilbargman@daniilbargman-xps>
;; Keywords: configuration

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Kubernetes support and integration

;;; Code:

;; install kubernetes plugin
(use-package kubernetes)

;; evil-mode integration
(use-package kubernetes-evil
  :after kubernetes)

;; use M-k as prefix for Kubernetes commands
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k M-k")
		'(lambda()
		   (interactive)
		   (tab-new)
		   (kubernetes-overview)
		   (tab-close)
		   (other-window 1)
		   (evil-split-buffer "*kubernetes overview*")
		   ))
(global-set-key (kbd "M-k n") 'kubernetes-set-namespace)
(global-set-key (kbd "M-k e") 'kubernetes-exec-into)

;; done
(provide 'k8s)
;;; k8s.el ends here
