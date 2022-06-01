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

;; kubernetes/helm syntax support
(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode)
  )

;; ;; enable LSP support for helm
;; (use-package helm-lsp
;;   :config
;;   (define-key lsp-mode-map [remap xref-find-apropos]
;; 	      #'helm-lsp-workspace-symbol)
;;   )

;; function for inferring which pod/container should be the target of an
;; `exec' function based on namespace and deployment
(defun k8s-parse-exec-command (label &optional namespace exec-command)
  "Parse a `kubectl exec...' command, inferring the target pod name.

LABEL: label(s) that identify the target pods.

NAMESPACE: optional namespace override (default: current namespace set
in the kubeconfig file).

EXEC-COMMAND: optional override for the command (default: /bin/sh).

If multiple pods have been matched, the first one will be used."
  (let* (
	 (pod-disc-cmd
	  (concat "echo $(kubectl get pod -l '" label "' "
		  (when namespace (concat "-n " namespace " "))
		  "-o jsonpath='{.items[0].metadata.name}')"))
	 (target-pod
	  (car (split-string (shell-command-to-string pod-disc-cmd))))
	 )
    (concat
     "kubectl exec -it "
     (when namespace (concat "-n " namespace " "))
     target-pod
     " -- "
     (or exec-command "/bin/sh"))
    )
  )


;; done
(provide 'k8s)
;;; k8s.el ends here
