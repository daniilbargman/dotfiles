;;; yaml.el --- YAML support -*- lexical-binding: t -*-

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

;; YAML support

;;; Code:

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  )

;; function for executing a python code block using terminal.el
(defun my-yaml-save-and-apply-manifest (p)
  "Apply manifest(s) inside buffer file to active Kubernetes cluster.

Saves the buffer before applying manifests.

Prefix P works like in get-or-create-terminal from terminal.el."
  (interactive "P")

  ;; save buffer
  (save-buffer)

  ;; run "apply" on terminal file
  (get-or-create-terminal
   p nil nil nil
   (lambda (term-buffer)
     (comint-send-string
      term-buffer
      (concat "kubectl apply -f " buffer-file-name "\n")
      )
     )
   )
  )

;; function for executing a python code block using terminal.el
(defun my-yaml-delete-manifest (p)
  "Delete manifest specified in buffer from active Kubernetes cluster.

Prefix P works like in get-or-create-terminal from terminal.el."
  (interactive "P")
  (get-or-create-terminal
   p nil nil nil
   (lambda (term-buffer)
     (comint-send-string
      term-buffer
      (concat "kubectl delete -f " buffer-file-name "\n")
      )
     )
   )
  )

(provide 'yaml)

;;; yaml.el ends here
