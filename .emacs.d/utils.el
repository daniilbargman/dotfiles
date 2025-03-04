;;; utils.el --- utility scripts -*- lexical-binding: t; -*-


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

;; utility scripts and functions for use across different elisp modules

;;; Code:


;; get string from file
(defun dbargman/contents-of-file (filePath)
  "Return contents of file under FILEPATH."
  (with-temp-buffer
    (when (file-exists-p filePath)
      (insert-file-contents filePath))
    (buffer-string)))


;; functions for formatting expressions inside braces
(defun dbargman/format-parens ()
  "Format collections inside parens."
  (interactive)
  ;; (message (number-to-string (nth 0 (syntax-ppss))))
  ;;  )

  ;; only run the command if point is at an opening paren
  (when (looking-at-p
	 (string-join
	  `("["
	    ,(string-join ide-format-parens-opening-paren-alist)
	    "]")))

    ;; do not move cursor after exiting this function
    (save-excursion

      ;; originally move cursor to closing paren to extract scope
      (evil-jump-item)

      (let* (

	     ;; check current smartparens mode status
	     (current-smartparens-mode smartparens-mode)

	     ;; get matching paren position
	     (match-paren-pos (point))

	     ;; save expression depth as failsafe against nested parens
	     (sexp-depth (ppss-depth (syntax-ppss)))

	     ;; alist of delimiters to use
	     (found-delims nil)

	     ;; pre-allocate list of breakpoints
	     (breakpoint-list nil)

	     ;; value of column indent, to infer how points should move
	     (column-offset nil)

	     ;; cumulative offset since start of iteration
	     (cumulative-offset 0)

	     ;; do not auto-fill text as filling is handled manually
	     (auto-fill-function nil)

	     )

	;; disable smartparens mode temporarily
	(smartparens-mode -1)

	;; save closing paren data
	(setq breakpoint-list
	      `((,(1- (point)) .
		 (1 ,(looking-back "^ *" (- match-paren-pos 200))))))

	;; return to opening paren and save opening paren data
	(evil-jump-item)
	(setq breakpoint-list
	      (append `((,(1+ (point)) . (1 ,(looking-at-p ".$"))))
		      breakpoint-list))

	;; try searching for elements of alists containing delimiters
	;; if found, assemble list of breakpoints
	(setq found-delims
	      (cl-loop
	       for delim-alist in ide-paren-wrap-delimiters

	       ;; loop over each element in an alist and look for match
	       if (cl-loop
		   for delim in delim-alist

		   ;; check if match exists
		   if (cl-loop
		       while t
		       if (search-forward (cadr delim) match-paren-pos t)
		       if (progn
			    (goto-char (- it 1))
			    (let
				((match-found
				  (and
				   ;; check same ppss depth
				   (= (ppss-depth (syntax-ppss)) sexp-depth)
				   ;; check that it's not inside a string
				   (not (in-string-p)))))

			      ;; append match to breakpoint list
			 
			      (forward-char)
			      (when match-found it)
			      )
			    )
		       collect `(,(- it (length (cadr delim)))
				 ,(looking-at-p ".$"))
		       into breakpoint-list
		       end
		       else return breakpoint-list
		       )
		   return it
		   )
	       return it
	       ))

	;; only continue if any breakpoints have been found
	(when breakpoint-list

	  ;; prepend closing paren data to the list of breakpoints
	  (goto-char match-paren-pos)
	  (setq breakpoint-list
		(append
		 `(
		   (,(match-paren-pos)
		    ,(looking-back "^ *" (- match-paren-pos 200)))
		   )
		 breakpoint-list
		 ))

	  ;; prepend opening paren data to the list of breakpoints
	  (evil-jump-item)
	  (setq breakpoint-list
		(append
		 `(
		   ,(1+ (point))
		   ,(looking-back "^ *" (- match-paren-pos 200))
		   )
		 breakpoint-list
		 ))

	  ;; print the breakpoint list
	  (prin1 breakpoint-list)
	  )

	;; return to previous smartparens mode
	(smartparens-mode current-smartparens-mode)
	)
      )
    )
  )


(provide 'utils)
;;; utils.el ends here
