;;; openvpn.el --- config for openvpn client -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  <daniilbargman@daniilbargman-xps>
;; Keywords: convenience

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

;; Configuration for the Emacs openvpn client

;;; Code:

;; ovpn-mode for VPN connections
(use-package ovpn-mode

  :hook (ovpn-mode . (lambda () (evil-local-set-key
		      'normal "s" 'ovpn-mode-start-vpn)))

  :init

  ; add /sbin/ to exec-path so it finds openvpn executable
  (setq exec-path (append exec-path '("/sbin")))  ; path to openvpn

  :config

  ; set vpn directory to ~/.vpn/
  (ovpn-mode-dir-set "~/.vpn/")

  ; use "C-s l" to open ovpn dashboard in new buffer
  (global-set-key (kbd "C-s l")
		  '(lambda() (interactive) (other-window 1)
		     (evil-split-buffer "*scratch*") (ovpn)))

  )

;; add evil hook for starting vpn client in normal mode
;; NOTE: This hook does not work for some reason
(add-hook 'ovpn-mode-hook (lambda () (evil-local-set-key 'normal
			    (kbd "s") 'ovpn-mode-start-vpn)))

(provide 'openvpn)
;;; openvpn.el ends here
