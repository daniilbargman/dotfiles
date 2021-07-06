;;; snippets.el --- code and text snippets -*- lexical-binding: t; -*-


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

;; Code and text snippets via YASnippet

;;; Code:

;; pull YASnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; pull predefined collection of snippets
(use-package yasnippet-snippets)

;; helper function to split python function input arguments for
;; yasnippet templates
(defun python-split-args-dbargman (input-string)
  "Split python arguments INPUT-STRING into ((name, type) value)."
  (let* (
	 (arglist (split-string input-string " *, *\n* *" t))
	 (argmap
	  (mapcar (lambda (x) (split-string x " *= *" nil)) arglist))
	 )
    (mapcar
     (lambda (x) (list (split-string (car x) " *: *" nil) (nth 1 x)))
     argmap)
    )
  )


(provide 'snippets)
;;; snippets.el ends here
