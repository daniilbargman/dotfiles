;;; ai.el --- AI functionality -*- lexical-binding: t -*-

;; Author: Daniil Bargman
;; Maintainer: Daniil Bargman
;; Version: 0.1.0
;; Package-Requires: ()
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

;; Customizations for markdown mode.

;;; Code:

;; gptel
(use-package gptel

  :hook (
	  (gptel-post-stream-hook . gptel-auto-scroll)
	  (gptel-post-response-functions . gptel-end-of-response)
	  )

  :config

  ;; use OpenRouter as default backend
  (setq gptel-backend
	(gptel-make-openai
	    "OpenRouter"
	  :host "openrouter.ai"
	  :endpoint "/api/v1/chat/completions"
	  :stream t
	  :key (replace-regexp-in-string
		"\n$" ""
		(shell-command-to-string
		 "pass show private/openrouter-api-key")
		)
	  :models '(anthropic/claude-sonnet-5.5
		    anthropic/claude-opus-5.5
		    anthropic/claude-fable-5.2
		    )
	  )
	)

  ;; default to Claude Sonnet
  (setq gptel-model 'anthropic/claude-sonnet-5.5)

  ;; use org-mode in conversation buffers
  (setq gptel-default-mode 'org-mode)

  ;; read saved sending options (set in gptel-menu) automatically
  (defun gptel-send-with-options (&optional arg)
    "Send query.  With prefix ARG open gptel's menu instead."
    (interactive "P")
    (if arg
	(call-interactively 'gptel-menu)
      (gptel--suffix-send (transient-args 'gptel-menu))))

  )

(provide 'ai)

;;; ai.el ends here
