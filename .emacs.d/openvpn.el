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

  :custom

  ;; disable prompt when switching off ipv6
  (ovpn-mode-ipv6-auto-toggle t)

  :config

  ;; disable ipv6
  (ovpn-mode-ipv6-linux-sysctl-disable 1)

  ;; set vpn directory to ~/.vpn/
  (ovpn-mode-dir-set "~/.vpn/")

  )

(provide 'openvpn)
;;; openvpn.el ends here
