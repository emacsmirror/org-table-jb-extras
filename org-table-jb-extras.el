;;; org-table-jb-extras.el --- Extra commands & functions for working with org-tables

;; Filename: org-table-jb-extras.el
;; Description: Extra commands & functions for working with org-tables
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2024, Joe Bloggs, all rites reversed.
;; Created: 2024-05-23 22:44:11
;; Version: 20240523.2317
;; Last-Updated: Thu May 23 23:17:48 2024
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/org-table-jb-extras
;; Keywords: tools 
;; Compatibility: GNU Emacs 25.2.2
;; Package-Requires: ((org "9.4.6") (cl-lib "1") (ido-choose-function "0.1") (dash "2.19.1") (s "1.13.1"))
;;
;; Features that might be required by this library:
;;
;; org cl-lib ido-choose-function ampl-mode dash s 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 16xSxS5v7rpknr4WL8unwCRab5nZEfc7eK
;;
;; This library contains many extra functions & commands for dealing with org-tables which are documented below.
;; For more information see the Readme.org file: https://github.com/vapniks/org-table-jb-extras/tree/main
;; 
;;;;;;;;

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `org-table-insert-or-delete-vline'
;;    Insert a vertical line in the current column, or delete some if NDELETE is non-nil.
;;  `org-table-grab-columns'
;;    Copy/kill columns or region of table and return as list(s).
;;  `org-table-flatten-columns'
;;    Apply FN to next NROWS cells in selected columns and replace cells in current row with results.
;;  `org-table-dispatch'
;;    Do something with column(s) of org-table at point.
;;  `insert-file-as-org-table'
;;   Insert a file into the current buffer at point, and convert it to an org table.
;;  `org-table-kill-field'
;;    Kill the org-table field under point.
;;  `org-table-copy-field'
;;    Copy the org-table field under point to the kill ring.
;;  `org-table-narrow-column'
;;    Split the current column of an org-mode table to be WIDTH characters wide.
;;  `org-table-narrow'
;;   Narrow the entire org-mode table, apart from FIXEDCOLS, to be within WIDTH characters by adding new rows.
;;  `org-table-fill-empty-cells'
;;   Fill empty cells in current column of org-table at point by splitting non-empty cells above them.
;;  `org-table-query-dimension'
;;   Print and return the number of columns, data lines, cells, hlines, height & width (in chars) of org-table at point.
;;  `org-table-move-cell'
;;   Prompt for a direction and move the current cell in that direction.
;;  `org-table-show-jump-condition'
;;   Display a message in the minibuffer showing the current jump condition.
;;  `org-table-set-jump-condition'
;;   Set the CONDITION for `org-table-jump-condition'.
;;  `org-table-set-jump-direction'
;;   Set the DIRECTION for `org-table-jump-condition'; 'up, 'down, 'left or 'right.
;;  `org-table-jump-next'
;;   Jump to the STEPS next field in the org-table at point matching `org-table-jump-condition'.
;;  `org-table-jump-prev'
;;   Like `org-table-jump-next' but jump STEPS in opposite direction.
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `org-table-flatten-functions'
;;    Alist of (NAME . FUNCTION) pairs for use with `org-table-flatten-column'.
;;  `org-table-graph-types'
;;    List of graph types for `org-plot/gnuplot'.
;;  `org-table-dispatch-actions'
;;    Actions that can be applied when `org-table-dispatch' is called.
;;  `org-table-filter-function-bindings'
;;    Function bindings (with descriptions) used by `org-table-jump-condition' & `org-dblock-write:tablefilter'.
;;  `org-table-jump-condition-presets'
;;    Named presets for `org-table-jump-condition'.
;;  `org-table-timestamp-patterns'
;;    List of java style date-time matching patterns as accepted by `datetime-matching-regexp' and related functions.
;;  `org-table-timestamp-format'
;;    Default format for timestamps output by `org-table-convert-timestamp'.
;;
;; All of the above can be customized by:
;;      M-x customize-group RET org-table RET
;;

;;; Installation:
;;
;; Put org-table-jb-extras.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'org-table-jb-extras)

;;; History:

;;; Require
(require 'org)
(require 'cl-lib)
(require 'ido-choose-function)
(require 'dash)
(require 's)
(require 'ampl-mode nil t) ;; my version which contains `run-ampl-async'
;;; Code:

;; simple-call-tree-info: CHECK  
(defvar org-table-vline-delim-regexp "^[[:blank:]]*\\($\\|#\\+\\|\\*[[:blank:]]+\\)"
  "A regular expression used by `org-table-insert-or-delete-vline' for matching 
  lines immediately before or after a table.")

;;;###autoload
;; simple-call-tree-info: CHECK  
(defun org-table-insert-or-delete-vline (&optional ndelete)
  "Insert a vertical line in the current column, or delete some if NDELETE is non-nil.
If NDELETE is a positive integer, or if called interactively with a positive numeric prefix arg, 
then NDELETE of the following vertical lines will be deleted. If NDELETE is negative then 
previous vertical lines will be deleted."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-value current-prefix-arg))))
  (if ndelete (dotimes (x (abs ndelete))
		(if (> 0 ndelete)
		    (re-search-backward "|\\|+" (line-beginning-position))
		  (re-search-forward "|\\|+" (line-end-position))
		  (forward-char -1))
		(org-table-delete-vline)
		(org-table-align))
    (let ((startn (save-excursion
		    (re-search-backward org-table-vline-delim-regexp)
		    (line-number-at-pos)))
	  (endn (save-excursion
		  (re-search-forward org-table-vline-delim-regexp)
		  (line-number-at-pos)))
	  (thisn (line-number-at-pos)))
      (org-table-insert-vline (- startn thisn))
      (delete-char 1)
      (org-table-insert-vline (- endn thisn))))
  (org-table-align))

;; simple-call-tree-info: CHECK  
(defun org-table-insert-vline (len)
  "Insert a vertical line of length LEN starting at point.
  If LEN is negative the line goes upwards, otherwise it goes downwards."
  (let ((col (current-column))
	(start (point)))
    (save-excursion
      (end-of-line (if (> len 0) len (+ 2 len)))
      (let* ((col2 (current-column))
	     (diff (- col2 col))
	     (end
	      (if (>= diff 0) (- (point) diff)
		(insert (make-string (- diff) 32))
		(point))))
	(if (< start end)
	    (string-rectangle start end "|")
	  (string-rectangle end (if (< diff 0) (- start diff) start) "|"))))))

;; simple-call-tree-info: CHECK  
(defun org-table-delete-vline nil
  "Delete the vertical line in the current column (if any)."
  (let ((col (current-column))
	(start (point)))
    (cl-flet ((vlineend (start col fwd)
			(save-excursion
			  (while (and (eq (current-column) col)
				      (or (eq (char-after) 124)
					  (eq (char-after) 43)))
			    (forward-line (if fwd 1 -1))
			    (forward-char-same-line (- col (current-column))))
			  (unless (eq (point) start)
			    (forward-line (if fwd -1 1))
			    (forward-char-same-line (- col (current-column)))
			    (if fwd (1+ (point)) (point))))))
      (delete-extract-rectangle (vlineend start col nil) (vlineend start col t)))))

;;;###autoload
;; simple-call-tree-info: CHECK  
(defcustom org-table-flatten-functions
  '(("append" . (lambda (sep lst)
		  (interactive (list (read-string "Separator (default \" \"): " nil nil " ") '<>))
		  (mapconcat 'identity lst sep)))
    ("prepend" . (lambda (sep lst)
		   (interactive (list (read-string "Separator (default \" \"): " nil nil " ") '<>))
		   (mapconcat 'identity (reverse lst) sep)))
    ("sum" . (lambda (lst) (apply '+ (mapcar 'string-to-number lst))))
    ("mean" . (lambda (lst) (/ (apply '+ (mapcar 'string-to-number lst)) (length lst))))
    ("max" . (lambda (lst) (apply 'max (mapcar 'string-to-number lst))))
    ("min" . (lambda (lst) (apply 'min (mapcar 'string-to-number lst))))
    ("delete all but nth" . (lambda (n lst)
			      (interactive (list (1- (read-number "Row to keep (1 = first, etc.): " 1)) '<>))
			      (nth n lst))))
  "Alist of (NAME . FUNCTION) pairs for use with `org-table-flatten-column'."
  :group 'org-table
  :type 'alist)

;; simple-call-tree-info: CHECK  
(defun org-table-flatten-column (nrows fn)
  "Replace current cell with results of applying FN to NROWS cells under, and including, current one.
If NROWS is a positive integer then the NROWS cells below and including the current one will be used.
If NROWS is a negative integer then the NROWS cells above and including the current one will be used.
If NROWS is not a number then all cells in the current column between horizontal separator lines will
be used. Function FN should take a single argument; a list of the contents of the cells. 
Return value is a cons cell containing the number of rows used & the length of the newly combined field."
  (unless (org-at-table-p) (error "Not in org-table"))
  (unless (numberp nrows)
    (let ((col (org-table-current-column)))
      (re-search-forward org-table-hline-regexp (org-table-end) 1)
      (forward-line -1)
      (let ((endline (line-number-at-pos (point))))
	(re-search-backward org-table-hline-regexp (org-table-begin) 1)
	(forward-line 1)
	(org-table-goto-column col)
	(setq nrows (1+ (- endline (line-number-at-pos (point))))))))
  (let* ((count nrows)
	 (l (if (> count 0) 1 -1))
	 (startpos (point))
	 (col (org-table-current-column))
	 (fields (cl-loop for n from 1 to (abs count) while (org-at-table-p)
			  collect (org-trim (org-table-blank-field))
			  do (forward-line l)
			  (while (org-at-table-hline-p) (forward-line l))
			  (org-table-goto-column col)))
	 (newtext (if fn (format "%s" (funcall fn fields)))))
    (goto-char startpos)
    (if newtext (insert newtext))
    (org-table-align)
    (cons nrows (length newtext))))

;;;###autoload
;; simple-call-tree-info: CHECK
(defun org-table-flatten-columns (nrows ncols fn &optional repeat)
  "Apply FN to next NROWS cells in selected columns and replace cells in current row with results.
If NROWS is a positive integer then the NROWS cells below and including the current one will be used.
If NROWS is a negative integer then the NROWS cells above and including the current one will be used.
If NROWS is not a number (e.g. when called interactively with a C-u prefix), then cells between
the separator lines above and below the current line will be used.
If NCOLS is non-nil then flatten the next NCOLS columns (including the current one), otherwise
flatten all columns. 
Alternatively, when called interactively, if region is active then that will be used to determine 
which cells are used.
This function calls `org-table-flatten-column' (which see) on columns in the current row.
If REPEAT is supplied then repeat this process REPEAT times.
Return value is the sum of lengths of the text in the newly combined fields."
  (interactive (let* ((regionp (region-active-p))
		      (regionstart (if regionp (region-beginning)))
		      (regionend (if regionp (region-end))))
		 (list 
		  (or (if regionp
			  (1+ (- (line-number-at-pos regionend)
				 (line-number-at-pos regionstart))))
		      current-prefix-arg
		      (read-number "Number of rows (-ve numbers count backwards): "))
		  (if regionp
		      (1+ (- (save-excursion (goto-char (region-end))
					     (org-table-current-column))
			     (progn (goto-char (region-beginning))
				    (org-table-current-column))))
		    (if (not (y-or-n-p "Flatten entire line? "))
			(read-number "Number of columns (-ve numbers count backwards): " 1)))
		  (ido-choose-function
		   org-table-flatten-functions nil "User function with one arg (list of fields): " t)
		  (if regionp 1 (read-number "Number or repetitions: " 1)))))
  (if (not (org-at-table-p)) (error "No org-table at point"))
  (let* ((startline (org-table-current-line))
	 (line startline)
	 (col (org-table-current-column))
	 (maxcol (save-excursion (end-of-line)
				 (search-backward "|")
				 (org-table-current-column)))
	 (startcol (if ncols col 1))
	 (endcol (if ncols (if (> ncols 0)
			       (min (1- (+ col ncols)) maxcol)
			     (max (1+ (+ col ncols)) 1))
		   maxcol))
	 (numreps (or repeat 1))
	 (textlen 0)
	 l pair)
    (org-table-goto-line startline)
    (org-table-goto-column col)
    (while (and (org-at-table-p) (> numreps 0))
      (cl-loop for c from (min startcol endcol) to (max startcol endcol)
	       do (org-table-goto-line line)
	       (org-table-goto-column c)
	       (setq pair (org-table-flatten-column nrows fn)
		     nrows (car pair)
		     textlen (+ textlen (cdr pair))))
      (setq l (if (> nrows 0) 1 -1))
      (forward-line l)
      (dotimes (n (1- (abs nrows)))
	(while (looking-at "^[-+|]*$") (forward-line l))
	(unless (not (org-at-table-p))
	  (if (looking-at "^[ |]*$")
	      (progn (kill-region (point-at-bol)
				  (min (1+ (point-at-eol)) (point-max)))
		     (if (< nrows 0) (forward-line l)))
	    (forward-line l))))
      (while (looking-at "^[-+|]*$") (forward-line l))
      (org-table-goto-column col)
      (setq line (org-table-current-line))
      (setq numreps (1- numreps)))
    (org-table-goto-line startline)
    (org-table-goto-column col)
    textlen))

;;;###autoload
;; simple-call-tree-info: CHECK  is this interactive?
(defun org-table-grab-columns (top bottom arg &optional kill)
  "Copy/kill columns or region of table and return as list(s).
  The return value is a list of lists - one for each row of the copied/killed data.
  If KILL is non-nill clear the copied fields, otherwise leave them.
  With no prefix ARG copy entire table, with a single C-u prefix copy the entire current column,
  with a numeric prefix copy that many columns from the current one rightwards, with a double C-u 
  prefix copy the data in the current column between horizontal separator lines.
  If region is active, or TOP & BOTTOM args are non-nil copy all cells in rectangle defined by region 
   (or TOP & BOTTOM args), ignoring horizontal separator lines."
  (interactive "r\nP")
  (if (org-at-table-p)
      (save-excursion
	(org-table-check-inside-data-field)
	(let (col beg end (org-timecnt 0) diff h m s org-table-clip)
	  ;; copy column(s) to `org-table-clip' 
	  (cond
	   ((org-region-active-p)
	    (org-table-copy-region top bottom kill))
	   ;; separator delimited column(s) 
	   ((and (listp arg) (> (prefix-numeric-value current-prefix-arg) 10))
	    (setq col (org-table-current-column))
	    (if (re-search-backward org-table-hline-regexp beg t)
		(forward-line 1)
	      (goto-char (org-table-begin)))
	    (org-table-goto-column col)
	    (setq beg (point))
	    (unless (re-search-forward org-table-hline-regexp end t)
	      (goto-char (org-table-end)))
	    (forward-line -1)
	    (org-table-goto-column col)
	    (org-table-copy-region beg (point) kill))
	   ;; whole column(s)
	   (arg
	    (setq col (org-table-current-column))
	    (goto-char (org-table-begin))
	    (org-table-goto-column col)
	    (setq beg (point))
	    (goto-char (org-table-end))
	    (forward-line -1)
	    (org-table-goto-column
	     (if (listp arg) col
	       (+ col (- arg (/ arg (abs arg))))))
	    (org-table-copy-region beg (point) kill))
	   ;; whole table
	   (t 
	    (goto-char (org-table-begin))
	    (org-table-goto-column 1)
	    (setq beg (point))
	    (goto-char (org-table-end))
	    (forward-line -1)
	    (org-end-of-line)
	    (org-table-previous-field)
	    (org-table-copy-region beg (point) kill)))
	  org-table-clip))))

;;;###autoload
;; simple-call-tree-info: CHECK  
(defun org-table-to-calc (lsts &optional unpack)
  "Add data in LSTS to calc as a matrix.
  The lists in LSTS will form the rows of the calc matrix created.
  If optional argument UNPACK is non-nil then unpack the vector/matrix."
  (let ((data
	 (cl-loop for col in lsts
		  collect
		  (nconc
		   (list 'vec)
		   (cl-remove-if-not
		    (lambda (x) (memq (car-safe x) '(float nil)))
		    (mapcar 'math-read-expr col))))))
    (calc)
    (calc-slow-wrapper
     (calc-enter-result
      0 "grab" (nconc (list 'vec)
		      (if (= (length data) 1) (cdar data) data))))
    (if unpack (calc-unpack nil))))

;;;###autoload
;; simple-call-tree-info: CHECK  
(defun org-table-plot-list (lst)
  (require 'org-plot)
  (let* ((name (ido-completing-read
		"Graph type: "
		(append '("Default" "User defined")
			(mapcar 'car org-table-graph-types))))
	 (params (cdr-safe (assoc name org-table-graph-types)))
	 (plotrx "[[:space:]]*#\\+PLOT:[[:space:]]*")
	 params2 tofile tlst)
    (if (null params)
	(save-excursion
	  (goto-char (org-table-begin))
	  (if (equal name "User defined")
	      (setq params2 (org-plot/add-options-to-plist
			     params2 (read-string "Plotting parameters (set, title, ind, deps, type, with, labels, line, map, script, timefmt):\n"
						  (if (re-search-backward
						       (concat plotrx "\\(.*\\)")
						       (save-excursion (forward-line -3) (point)) t)
						      (match-string 1)
						    "ind:1 with:lines plot-type:2d"))))
	    ;; if there are no params use the default ones
	    (while (and (equal 0 (forward-line -1))
			(looking-at plotrx))
	      (setf params2 (org-plot/collect-options params2)))))
      (cl-loop for prop in params by 'cddr
	       do (let ((val (plist-get params prop)))
		    (setq params2
			  (plist-put params2 prop
				     (if (eq val 'prompt)
					 (case prop
					   (:plot-type (make-symbol (ido-completing-read "Plot type: "
											 '("2d" "3d" "grid"))))
					   (:script (ido-read-file-name "Gnuplot script to include: "))
					   (:line (read-string "Line to be added to gnuplot script: "))
					   (:set (read-string "Set an option (e.g. ylabel 'Name'): "))
					   (:title (read-string "Title of plot: "))
					   (:ind (read-number "Column containing independent var, relative to selected columns: " 1))
					   (:deps (read--expression
						   "Columns containing dependent vars as a list, e.g. (2 3 4), and relative to selected columns : "))
					   (:with (make-symbol
						   (ido-completing-read
						    "Style: "
						    '("lines" "dots" "steps" "errorbars" "xerrorbar" "xyerrorlines"
						      "points" "impulses" "fsteps" "errorlines" "xerrorlines" "yerrorbars"
						      "linespoints" "labels" "histeps" "financebars" "xyerrorbars" "yerrorlines"
						      "vectors" "boxes" "candlesticks" "image" "circles" "boxerrorbars"
						      "filledcurves" "rgbimage" "ellipses" "boxxyerrorbars" "histograms"
						      "rgbalpha" "pm3d" "boxplot"))))
					   (:labels (let (label labels)
						      (while (> (length
								 (setq label
								       (read-string
									"Labels to be used for the dependent vars (enter to stop): ")))
								0)
							(push label labels))
						      labels))
					   (:map  (y-or-n-p "Use flat mapping for grid plot"))
					   (:timefmt  (read-string "Timefmt (e.g. %Y-%m-%d): ")))
				       val))))))
    (setq tofile (y-or-n-p "Save to file?")
	  filename (if tofile
		       (substring-no-properties
			(ido-read-file-name "Filename: "))))
    (if (= (length (setq tlst (org-table-transpose lst))) 1)
	(setq lst
	      (org-table-transpose
	       (cons (cl-loop for i from 1 to (length (car tlst))
			      collect (number-to-string i))
		     tlst))
	      params2 (plist-put params2 :ind 1))
      (setq lst (org-table-transpose tlst)))
    (if tofile
	(setq params2 (plist-put params2 :file filename)))
    (with-temp-buffer
      (insert (org-table-lisp-to-string lst))
      (forward-line -1)
      (org-plot/gnuplot params2))
    (if (and tofile (y-or-n-p "View saved graph"))
	(find-file filename))
    (message "Plotting parameters: %s" params2)))

;;;###autoload
;; simple-call-tree-info: STARTED  
(defcustom org-table-graph-types '(("lines in 2D" :plot-type 2d :with lines :ind 1 :set
				    ("terminal wxt 0"))
				   ("histogram" :set
				    ("terminal wxt 0")
				    :plot-type 2d :with histograms :ind 1 :set
				    ("terminal wxt 0"))
				   ("flat grid plot" :plot-type grid :map t)
				   ("3D plot" :plot-type 3d :with pm3d))
  "List of graph types for `org-plot/gnuplot'.

  An assoc-list of (NAME . PLIST) pairs where NAME is the name of the graph type,
  and PLIST is a property list to be used as an argument for `org-plot/gnuplot'.
  For a list of possible properties and values see the org-plot.org file.
  If any of the property values is 'prompt then the user will be prompted for a
  value when the graph is chosen."
  :group 'org-table
  :type '(repeat (cons string
		       (plist :key-type sexp :value-type sexp))))

;;;###autoload
;; simple-call-tree-info: STARTED  
(defcustom org-table-dispatch-actions '(("copy cells in region to org-table-clip" . org-table-copy-region)
					("cut cells in region to org-table-clip" . org-table-cut-region)
					("paste copied cells from org-table-clip" . org-table-paste-rectangle)
					("copy table to kill ring" . (lambda (lst) (kill-new (org-table-lisp-to-string lst))))
					("copy cells in region to rectangle" . (lambda (lst)
										 (setq killed-rectangle
										       (split-string (org-table-lisp-to-string lst)
												     "\n"))))
					("export to file" . (lambda (lst)
							      (with-temp-buffer
								(insert (org-table-lisp-to-string lst))
								(forward-line -1)
								(org-table-export))))
					("copy cols to calc" .
					 (lambda (lst) (org-table-to-calc (org-table-transpose lst))))
					("plot graph" . (lambda (lst) (org-table-plot-list lst)))
					("fit curve to cols" .
					 (lambda (lst) (org-table-to-calc (org-table-transpose lst) nil)
					   (calc-curve-fit nil)))
					("transpose table" . org-table-transpose-table-at-point)
					("split/join columns (insert/delete vline)" . org-table-insert-or-delete-vline)
					("join rows/flatten columns" . org-table-flatten-columns)
					("Toggle display of row/column refs" . org-table-toggle-coordinate-overlays)
					("Hide/show column" . org-table-toggle-column-width)
					("Narrow column" . org-table-narrow-column)
					("Narrow table" . org-table-narrow)
					("Fill empty cells" . org-table-fill-empty-cells)
					("Move current cell" . org-table-move-cell)
					("Set jump condition" . org-table-set-jump-condition)
					("Show table dimensions/info" . org-table-query-dimension))
  "Actions that can be applied when `org-table-dispatch' is called.
Each element should be of the form (NAME . FUNC) where NAME is a name for the action,
  and FUNC is a function with no non-optional args, or a lambda function of one argument. 
  If the latter case columns of data returned by `org-table-grab-columns' will be passed in as the argument,
  and if the NAME contains the string \"kill\" then the selected columns will be deleted from the table."
  :group 'org-table
  :type '(alist :key-type string :value-type (function :tag "Function acting on list of lists")))

;; TODO - do something similar for .csv files?
;;;###autoload
;; simple-call-tree-info: DONE
(defun org-table-dispatch nil
  "Do something with column(s) of org-table at point.
Prompt the user for an action in `org-table-dispatch-actions' and apply the corresponding function.
If the function takes a single argument then pass in a subtable list obtained from `org-table-grab-columns'.
If in addition, the name of the action contains the word \"kill\" then the cells in the selected columns/region 
will be cleared."
  (interactive)
  (let* ((pair (assoc (ido-completing-read "Action: "
					   (mapcar
					    (lambda (x)
					      (concat (car x)
						      (let ((keyseq (where-is-internal (cdr x))))
							(if keyseq (concat " (" (key-description (car keyseq)) ")")))))
					    org-table-dispatch-actions))
		      org-table-dispatch-actions))
	 (func (cdr pair)))
    (if (and (listp func)
	     (= (length (cadr func)) 1))
	(funcall func (org-table-grab-columns (if (region-active-p) (region-beginning))
					      (if (region-active-p) (region-end))
					      current-prefix-arg
					      (string-match "\\<kill\\>" (car pair))))
      (if (commandp func)
	  (call-interactively func)
	(funcall func)))))
;; Add to org-table menu
(easy-menu-add-item org-tbl-menu nil ["Ido select table action..." org-table-dispatch (org-at-table-p)])
(easy-menu-add-item orgtbl-mode-menu nil ["Ido select table action..." org-table-dispatch (org-at-table-p)])
;;(easy-menu-remove-item org-tbl-menu nil "Ido select table action...")
;;(easy-menu-remove-item orgtbl-mode-menu nil "Ido select table action...")

;;;###autoload
;; simple-call-tree-info: DONE  
(defun insert-file-as-org-table (filename)
  "Insert a file into the current buffer at point, and convert it to an org table."
  (interactive (list (ido-read-file-name "csv file: ")))
  (let* ((start (point))
	 (end (+ start (nth 1 (insert-file-contents filename)))))
    (org-table-convert-region start end)))

;;;###autoload
;; simple-call-tree-info: DONE
(defun org-table-copy-field nil
  "Copy the org-table field under point to the kill ring."
  (interactive)
  (kill-new (replace-regexp-in-string " +$" "" (replace-regexp-in-string "^ +" "" (org-table-get-field)))))

;;;###autoload
;; simple-call-tree-info: DONE  
(defun org-table-kill-field nil
  "Kill the org-table field under point."
  (interactive)
  (kill-new (replace-regexp-in-string " +$" "" (replace-regexp-in-string "^ +" "" (org-table-get-field))))
  (org-table-blank-field))

;; Useful
;; simple-call-tree-info: DONE  
(defsubst split-string-by-width (width str)
  "Split a string STR into substrings of length at most WIDTH without breaking words."
  (split-string (s-word-wrap width str) "\n"))

;;;###autoload
;; simple-call-tree-info: DONE  
(defun org-table-narrow-column (width &optional arg)
  "Split the current column of an org-mode table to be WIDTH characters wide.
If a cell's content exceeds WIDTH, split it into multiple rows, leaving new cells in other columns empty.
When called interactively or if WIDTH is nil, the user will be prompted for a width.
If ARG is non-nil, or a prefix arg is used interactively, put a horizontal line between each group of rows 
corresponding to the same original row."
  (interactive "nNew column width: \nP")
  (unless (org-at-table-p) (error "Not in an org-mode table"))
  (let* ((curcol (org-table-current-column))
         (table (org-table-to-lisp))
	 (curline (org-table-current-line))
	 (newrows (--map (if (or (eq it 'hline)
				 (<= (length (nth (1- curcol) it)) width))
			     (list it)
			   (apply '-zip-lists-fill
				  ""
				  (append
				   (mapcar 'list (-select-by-indices (number-sequence 0 (- curcol 2)) it))
				   (list (split-string-by-width width (nth (1- curcol) it)))
				   (mapcar 'list (nthcdr curcol it)))))
			 table)))
    (org-table-align)
    (let ((start (org-table-begin))
          (end (org-table-end)))
      (delete-region start end)
      (goto-char start)
      (dolist (row (apply 'append (if arg (-interpose '(hline) newrows) newrows)))
        (if (eq row 'hline)
            (insert "|-\n")
          (insert "| " (mapconcat 'identity row " | ") " |\n")))
      (org-table-align)
      (org-table-goto-line curline)
      (org-table-goto-column curcol))))

;; simple-call-tree-info: DONE  
(defun org-table-get-column-widths (&optional tbl)
  "Return the widths of columns in an org table.
Optional arg TBL is a list containing the table as returned by `org-table-to-lisp',
if this is nil then it will be calculated using `org-table-to-lisp'."
  (let ((table (or tbl (org-table-to-lisp))))
    (-reduce-from (lambda (acc row)
		    (if (eq row 'hline)
			acc
		      (-zip-with 'max acc (-map 'length row))))
		  (make-list (org-table-ncols table) 0)
		  table)))

;;;###autoload
(when (fboundp 'run-ampl-async)
  ;; simple-call-tree-info: CHECK 
  (defun org-table-narrow (width &optional arg fixedcols)
    "Narrow the entire org-mode table, apart from FIXEDCOLS, to be within WIDTH characters by adding new rows.
FIXEDCOLS should be a list of indices of the columns that shouldn't be narrowed (starting at 0).
New cells added beneath those that don't need to be split will be left empty.
If ARG is non-nil, or if a prefix arg is used when called interactively, then put horizontal lines between
sets of rows in the new table corresponding with rows in the original table."
    (interactive (let* ((tblstart (org-table-begin))
			(lineend (save-excursion (goto-char tblstart) (line-end-position)))
			(numcols 0)
			(hist (progn (save-excursion (goto-char tblstart)
						     (while (search-forward "|" lineend t)
						       (setq numcols (1+ numcols))))
				     (cons (mapconcat 'number-to-string (number-sequence 0 (- numcols 2)) " ")
					   minibuffer-history)))
			(str (read-string "Indices of fixed columns (press <up> to see full list, default = None): "
					  nil 'hist)))
		   (list
		    (read-number (format "New table width (current width = %s): "
					 (- lineend tblstart)))
		    current-prefix-arg
		    (progn (while (not (string-match "^[0-9 ]*$" str))
			     (setq str (read-string "Indices of fixed columns (space separated): ")))
			   (mapcar 'string-to-number
				   (cl-remove "" (split-string str "\\s-+") :test 'equal))))))
    (unless (org-at-table-p) (error "Not in an org-mode table"))
    (let* ((table (org-table-to-lisp))
	   (nrows (length table))
	   (ncols (org-table-ncols table))
	   (colwidths (org-table-get-column-widths table))
	   (freecols (-difference (number-sequence 0 (1- ncols)) fixedcols))
	   (inputstr (mapconcat 'number-to-string ;; input data for AMPL
				(append (list nrows (length freecols)
					      (- width
						 (apply '+ (-select-by-indices fixedcols colwidths))))
					(mapcan (lambda (row)
						  (if (symbolp row)
						      (make-list (length freecols) 0)
						    (mapcar 'length (-select-by-indices freecols row))))
						table))
				" "))
	   (outbuf "*org-table AMPL output*")
	   (errbuf "*org-table AMPL error*")
	   (curline (org-table-current-line))
	   (curcol (org-table-current-column))
	   (startpos (org-table-begin))
	   (endpos (org-table-end))
	   amplproc rowcounts newwidths newrows)
      (setq amplproc (run-ampl-async inputstr outbuf errbuf
				     (list
				      (concat (file-name-directory (locate-library "org-table-jb-extras"))
					      "table_widths.mod"))))
      (while (not (memq (process-status amplproc) '(exit signal)))
	(sit-for 0.1))
      (if (with-current-buffer errbuf
	    (search-backward "AMPL command executed successfully" nil t))
	  (with-current-buffer outbuf
	    (goto-char (point-min))
	    (search-forward "Widths: ")
	    (setq newwidths (mapcar 'string-to-number
				    (split-string (string-trim (buffer-substring (point) (point-at-eol)))
						  "\\s-+")))
	    (dotimes (i (length freecols))
	      (setf (nth (nth i freecols) colwidths) (nth i newwidths)))
	    (search-forward "Rows: ")
	    (setq rowcounts (mapcar 'string-to-number
				    (split-string (string-trim (buffer-substring (point) (point-at-eol)))
						  "\\s-+"))))
	(error "AMPL error. See %s buffer for details" errbuf))
      (if (-any? 'zerop newwidths)
	  (message "Unable to narrow table to desired width")
	(setq newrows (--map (let* ((row (nth it table)))
			       (if (> (nth it rowcounts) 1)
				   (apply '-zip-lists-fill ""
					  (--zip-with (split-string-by-width it other)
						      colwidths row))
				 (list row)))
			     (number-sequence 0 (1- nrows))))
	(delete-region startpos endpos)
	(goto-char startpos)
	(dolist (row (apply 'append (if arg (-interpose '(hline) newrows) newrows)))
	  (if (eq row 'hline)
	      (insert "|-\n")
	    (insert "| " (mapconcat 'identity row " | ") " |\n")))
	(org-table-align)
	(org-table-goto-line curline)
	(org-table-goto-column curcol)))))

;; This could be done more accurately using an AMPL program, but I want it to be usable even if AMPL is not available.
;;;###autoload
;; simple-call-tree-info: CHECK  
(defun org-table-fill-empty-cells (&optional col beg end min1 min2 rx)
  "Fill empty cells in current column of org-table at point by splitting non-empty cells above them.
Specify a different column using the COL argument.
BEG and END are optional positions defining the range of lines of the table to consider, by default they
will be set to the beginning & end of region if active, or the beginning and end of the table otherwise
 (so that the entire column is processed).
MIN1 specifies that only cells of length >= MIN1 should be split, and MIN2 specifies that new cells should
have average length at least MIN2. Finally you can limit cell splitting to only those match regexp RX.
If called interactively with a prefix arg these last 3 arguments will be prompted for. By default they are
not used."
  (interactive)
  (unless (org-at-table-p)
    (error "Point is not in an org-table"))
  (let* ((col (or col (org-table-current-column)))
         (beg (or beg (if (use-region-p) (region-beginning) (org-table-begin))))
         (end (or end (if (use-region-p) (region-end) (- (org-table-end) 2))))
	 (min1 (if (and current-prefix-arg (called-interactively-p 'any))
		   (read-number "Minimum length of cells to split: " 0)
		 (or min1 0)))
	 (min2 (if (and current-prefix-arg (called-interactively-p 'any))
		   (read-number "Minimum average length of new cells: " 0)
		 (or min2 0)))
	 (rx (if (and current-prefix-arg (called-interactively-p 'any))
		 (read-regexp "Only split cells matching regexp (default matches all): ")
	       rx))
	 (numcelllines 0)
         celllines newcells)
    (save-excursion
      (goto-char end)
      (while (>= (point) beg)
	(let* ((line (org-table-current-line))
	       (cell (org-table-get line col))
	       (celllen (length cell))
	       newcellwidth)
	  (push line celllines)
	  (cl-incf numcelllines)
	  (when (and cell (not (string-empty-p cell)))
	    (when (and (> numcelllines 1)
		       (> celllen min1)
		       (or (not rx) (string-match rx cell)))
	      (setq newcellwidth (max (1+ (/ celllen numcelllines))
				      min2)
		    newcells (split-string-by-width newcellwidth cell))
	      (while (> (length newcells) (length celllines))
		(setq newcellwidth (+ newcellwidth 5)
		      newcells (split-string-by-width newcellwidth cell)))
	      (save-excursion
		(dolist (newcell newcells)
		  (org-table-goto-line (pop celllines))
		  (org-table-goto-column col)
		  (org-table-blank-field)
		  (insert (pop newcells)))))
	    (setq celllines nil numcelllines 0)))
	(forward-line -1)
	(org-table-goto-column col))
      (org-table-align))))

;; TODO: org-table-reformat: user chooses from a collection of preset options which
;; determines latex/html/org-attribs code to put before & after the table (e.g. for adjusting font size & margins)
;; and the width of the table, etc.

;;;###autoload
;; simple-call-tree-info: TODO
(defcustom org-table-filter-function-bindings
  '(((num (x) (string-to-number x)) . "Convert a string to a number.")
    ((days-to-now (x) (condition-case nil (org-time-stamp-to-now x) (error nil))) .
     "Number of days from org timestamp string arg to now.")
    ((seconds-to-now (x) (condition-case nil (org-time-stamp-to-now x t) (error nil))) .
     "Number of seconds from org timestamp string arg to now.")
    ((stime (x) (condition-case nil (org-time-string-to-seconds x) (error nil))) .
     "Convert an org timestamp string to number of seconds since the epoch (1970-01-01 01:00)")
    ((dtime (x) (condition-case nil (org-time-string-to-absolute x) (error nil))) .
     "Convert an org timestamp to number of days since 0000-12-30")
    ((year (x) (condition-case nil (string-to-number
                                    (format-time-string "%Y" (org-time-string-to-time x)))
                 (error nil))) . "The year of org timestamp arg (as a number)")
    ((month (x) (condition-case nil (string-to-number
				     (format-time-string "%m" (org-time-string-to-time x)))
                  (error nil))) . "The month of org timestamp arg (as a number)")
    ((yearday (x) (condition-case nil (string-to-number
				       (format-time-string "%j" (org-time-string-to-time x)))
                    (error nil))) . "The yearday of org timestamp arg (as a number)")
    ((monthday (x) (condition-case nil (string-to-number
					(format-time-string "%d" (org-time-string-to-time x)))
                     (error nil))) . "The monthday of org timestamp arg (as a number)")
    ((weekday (x) (condition-case nil (string-to-number
				       (format-time-string "%w" (org-time-string-to-time x)))
                    (error nil))) . "The weekday of org timestamp arg (as a number, Sunday is 0)")
    ((hrs (x) (condition-case nil (string-to-number
				   (format-time-string "%H" (org-time-string-to-time x)))
                (error nil))) . "The hour of org timestamp arg (as a number)")
    ((mins (x) (condition-case nil (string-to-number
				    (format-time-string "%M" (org-time-string-to-time x)))
                 (error nil))) . "The minute of org timestamp arg (as a number)")
    ((secs (x) (condition-case nil (string-to-number
				    (format-time-string "%S" (org-time-string-to-time x)))
                 (error nil))) . "The second of org timestamp arg (as a number)")
    ((gt (x y) (and x y (> x y))) . "Non-nil if x > y (and both x & y are non-nil)")
    ((lt (x y) (and x y (< x y))) . "Non-nil if x < y (and both x & y are non-nil)")
    ((gteq (x y) (and x y (>= x y))) . "Non-nil if x >= y (and both x & y are non-nil)")
    ((lteq (x y) (and x y (<= x y))) . "Non-nil if x <= y (and both x & y are non-nil)")
    ((between (x y z &optional excly exclz) (org-table-between x y z excly exclz)) .
     "Non-nil if X is a value/date/string between Y & Z (see `org-table-between')")
    ((rowmatch (regex) (some (lambda (x) (string-match regex x)) row)) .
     "Non-nil if REGEX matches any column in a row")
    ((rowsum nil (-sum (mapcar 'string-to-number row))) .
     "Sum the numbers in all the columns of a row.")
    ((field (&optional roffset coffset)
	    (nth (+ (or coffset 0) (1- currentcol))
		 (nth (nth (+ (or roffset 0) (1- currentline)) table-dlines) table)))
     . "Return contents of cell in row (current row + ROFFSET) & column (current column + COFFSET).")
    ((matchfield (regex &optional roffset coffset) (string-match regex (field roffset coffset)))
     . "Perform `string-match' with REGEX on contents of a field/cell indexed relative to current one.")
    ((setfield (value &optional roffset coffset noprompt)
	       (org-table-set-relative-field value noprompt (+ (or roffset 0) currentline)
					     (+ (or coffset 0) currentcol))
	       (setf (nth (1- (+ currentcol (or coffset 0)))
			  (nth (1- (+ currentline (or roffset 0))) table))
		     value))
     . "Set field in row (current row + ROFFSET) & column (current column + COFFSET) to VALUE.")
    ((replace-in-field (regexp rep &optional roffset coffset noprompt)
		       (when (string-match regexp (field roffset coffset))
			 (setfield (replace-regexp-in-string regexp rep (field roffset coffset))
				   roffset coffset noprompt)))
     . "Replace matches to REGEXP with REP in field in row (current row + ROFFSET) & column (current column + COFFSET).")
    ((field2num (&optional roffset coffset) (string-to-number (field roffset coffset)))
     . "Read field in row (current row + ROFFSET) & column (current column + COFFSET) as a number and return it.")
    ((changenumber (func &optional roffset coffset noprompt)
		   (setfield (number-to-string (funcall func (field2num roffset coffset)))
			     roffset coffset noprompt))
     . "Apply FUNC to number in field, and replace the field with the result.")
    ((convertdate (&optional outfmt roffset coffset noprompt &rest patterns)
		  (let ((newfield (org-table-convert-timestamp
				   (field roffset coffset) outfmt patterns)))
		    (if (not newfield) nil
		      (setfield newfield roffset coffset noprompt)
		      (org-table-align))))
     . "Convert date in relative field to different format, if it contains one, otherwise return nil.")
    ((flatten (&optional nrows ncols func reps) (org-table-jump-flatten-cells nrows ncols func reps)) .
     "See `org-table-flatten-columns'.") ;TODO; check this works at currentline & currentcol
    ((hline-p (roffset)
	      (seq-contains table-hlines (+ (1- currentline) roffset))) .
	      "Return non-nil if row at (current row + ROFFSET) is a horizontal line.")
    ;; TODO: wrapper around org-table-insert-hlines
    ((countcells (dir roffset coffset &rest regexs)
		 (apply 'org-table-count-matching-fields
			table table-dlines dir (+ currentline roffset) (+ currentcol coffset) numdlines numcols regexs))
     . "Return list of counts of sequential cells, in direction DIR from cell offset, matching each regexp in REGEXS.")
    ((checkcounts (counts bounds) (org-table-check-bounds counts bounds)) .
     "Check BOUNDS of each number in COUNTS.")
    ((sumcounts (dir roffset coffset &rest regexs)
		(apply '+ (apply 'org-table-count-matching-fields
				 table table-dlines dir (+ currentline roffset) (+ currentcol coffset) regexs)))
     . "Return total No. of sequential matches to REGEXS in direction DIR from cell offset.")
    ((getvar (key) (cdr (assoc key org-table-jump-state))) .
     "Get the value associated with KEY in `org-table-jump-state'.")
    ((setvar (key val)
	     (setf (alist-get key org-table-jump-state) val) val) .
	     "Set the value associated with KEY in `org-table-jump-state' to VAL, and return VAL.")
    ((checkvar (key &rest vals) (member (getvar key) vals)) .
     "Return t if value associated with KEY in `org-table-jump-state' is among VALS, and nil otherwise.")
    ((gotocell (line &optional col) (org-table-goto-line (or line startline))
	       (org-table-goto-column (or col startcol))
	       (setq currentline (or line startline) currentcol (or col startcol)))
     . "Jump immediately to cell in specified LINE & COL. If either arg is nil use the current line/column."))
  "Function bindings (with descriptions) used by `org-table-jump-condition' & `org-dblock-write:tablefilter'.
These function bindings can be used in the cdr of `org-table-jump-condition', or the :filter parameter of
a tablefilter dynamic block. For :filter parameters the functions are created after the default row & column
variables have been created, and so can make use of those variables. However functions that use those variables
are not usable in `org-table-jump-condition', and be careful not to shadow any existing functions used by
 `org-table-filter-list'.
These function will only work in a :filter parameter: rowmatch & rowsum
and the functions in the list from field onwards will only work in `org-table-jump-condition' (mostly wrapper 
functions, see the documentation of the functions they wrap for more info)."
  :group 'org-table
  :type '(repeat (cons (sexp :tag "Function") (string :tag "Description"))))

;; simple-call-tree-info: DONE  
(defun org-table-timestamp-p (x)
  "Return non-nil if X is a string containing an org timestamp."
  (and (stringp x)
       (string-match "[0-9]\\{4\\}-[0-9]\\{1,2\\}-[0-9]\\{1,2\\}" x)))

;; simple-call-tree-info: CHECK
(defun org-table-equal (x y)
  "Return non-nil if X & Y represent the same number/symbol/string."
  (cond
   ((and (numberp x) (numberp y)) (equalp x y))
   ((and (numberp x) (stringp y)) (equalp x (string-to-number y)))
   ((and (stringp x) (numberp y)) (equalp (string-to-number x) y))
   ((and (stringp x) (stringp y)) (equalp x y))
   ((and (symbolp x) (stringp y)) (equalp (symbol-name x) y))
   ((and (stringp x) (symbolp y)) (equalp x (symbol-name y)))
   ((and (symbolp x) (symbolp y)) (eql x y))
   (t (equalp x y))))

;; simple-call-tree-info: DONE  
(defun org-table-between (x y z &optional excly exclz)
  "Return non-nil if X is >= to Y and <= Z, where X is a string, and Y & Z may be numbers, timestamps or strings.
If Y is nil then just test for X <= Z and if Z is nil just test X >= Y.
If EXCLY/EXCLZ are non nil then don't allow values of X equal to Y/Z respectively.

The type of test performed depends on the types of Y & Z:
If Y and Z are numbers , then convert X to a number and compare numbers.
If Y and Z are org-timestamps, then treat X as a timestamp and compare times.
In all other cases compare strings lexicographically (using `string<', `string>', and `string=')."
  (let (cmpy cmpz)
    (cond
     ((and (not y) (not z)) (error "Need at least one of Y or Z args to compare"))
     ((or (numberp y) (numberp z))
      (setq cmpy (if excly '> '>=)
            cmpz (if exclz '< '<=)
            x (string-to-number x)))
     ((or (not (org-table-timestamp-p y))
          (not (org-table-timestamp-p z)))
      (setq cmpy (if excly (lambda (a b) (and (not (string= a b)) (not (string< a b))))
                   (lambda (a b) (not (string< a b))))
            cmpz (if exclz 'string< (lambda (a b) (or (string< a b) (string= a b))))))
     (t (setq cmpy (if excly '> '>=)
              cmpz (if exclz '< '<=)
              x (org-time-string-to-seconds x)
              y (if y (org-time-string-to-seconds y))
              z (if z (org-time-string-to-seconds z)))))
    (and (if y (funcall cmpy x y) t)
         (if z (funcall cmpz x z) t))))

;; simple-call-tree-info: DONE  
(defun org-table-fetch (name-or-id &optional aslisp)
  "Return table named NAME as text, or as a lisp list if optional arg ASLISP is non-nil.
The name of a table is determined by a #+NAME or #+TBLNAME line before the table."
  (let (buffer start end id-loc)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (if (re-search-forward
             (concat "^[ \t]*#\\+\\(tbl\\)?name:[ \t]*"
                     (regexp-quote name-or-id) "[ \t]*$")
             nil t)
            (setq buffer (current-buffer) start (match-beginning 0))
          (setq id-loc (org-id-find name-or-id 'marker))
          (unless (and id-loc (markerp id-loc))
            (user-error "Can't find remote table \"%s\"" name-or-id))
          (setq buffer (marker-buffer id-loc)
                start (marker-position id-loc))
          (move-marker id-loc nil))))
    (with-current-buffer buffer
      (save-excursion
	(save-restriction
	  (widen)
          (goto-char start)
          (re-search-forward "^|")
          (beginning-of-line)
          (if aslisp (org-table-to-lisp)
            (buffer-substring (point) (org-table-end))))))))

;; simple-call-tree-info: DONE  
(defun org-table-ncols (tbl)
  "Returns the number of columns in an org table (in list form)"
  (let ((n 0))
    (while (eql (nth n tbl) 'hline)
      (setq n (1+ n)))
    (length (nth n tbl))))

;; simple-call-tree-info: DONE
(defun org-table-nrows (tbl)
  "Returns the number of rows in an org table (in list form) not counting hlines.
To count the number of rows including hlines use `length'."
  (length (remove 'hline tbl)))

;; simple-call-tree-info: CHECK
(defun org-table-query-dimension (&optional where)
  "Print and return the number of columns, data lines, cells, hlines, height & width (in chars) of org-table at point.
If WHERE arg is supplied report values for table at that position in the buffer."
  (interactive)
  (save-excursion
    (if where (goto-char where))
    (if (not (org-at-table-p)) (error "No org-table here"))
    (org-table-analyze)
    (let ((ndlines (length (seq-filter 'numberp org-table-dlines)))
	  (nhlines (length (seq-filter 'numberp org-table-hlines)))
	  (ncols org-table-current-ncol)
	  begin end)
      (goto-char (line-beginning-position))
      (search-forward "|")
      (setq begin (point))
      (goto-char (line-end-position))
      (setq end (point))
      (message "Num cols: %d, Num data lines: %d, Num cells: %d, Num hlines: %d, Height: %d, Width: %d"
	       ncols ndlines (* ndlines ncols) nhlines (+ ndlines nhlines) (1+ (- end begin)))
      (list ncols ndlines (* ndlines ncols) nhlines (+ ndlines nhlines) (1+ (- end begin))))))

;; simple-call-tree-info: DONE  
(defun org-table-lisp-to-string (lst &optional insert)
  "Convert an org table stored in list LST into a string.
LST should be a list of lists as returned by `org-table-to-lisp'.
If optional arg INSERT is non-nil then insert and align the table at point."
  (if lst
      (let* ((ncols (-max (mapcar (lambda (x) (if (listp x) (length x) 1)) lst)))
             (str (mapconcat (lambda(x)
                               (if (eq x 'hline) (concat "|" (s-repeat ncols "+|") "\n")
                                 (concat "| " (mapconcat 'identity x " | " ) "  |\n" )))
                             lst "")))
        (if (not insert) str
          (insert str)
          (org-table-align)))))

;; simple-call-tree-info: DONE
(defun org-table-strings-to-orgtable (lst rowsize &optional order)
  "Convert a list of strings into a list of lists of strings representing an org table.
LST is the list of strings, and ROWSIZE is the number of items to be placed in each row.
If (length LST) is not a multiple of ROWSIZE then blanks (nils) will be used to fill the
leftover columns.
The optional argument ORDER is a list of indices indicating the order in which the items
in LST should be added to the table (note 0 indexes the first item). The items will be added
row by row, but you may transpose the table afterwards using `org-table-transpose'."
  (let* ((lst (if order (-select-by-indices order lst) lst))
         (rem (% (length lst) rowsize)))
    (-snoc (-partition rowsize lst)
           (nconc (subseq lst (* -1 rem))
                  (make-list (- rowsize rem) nil)))))

;; simple-call-tree-info: DONE  
(defun org-table-insert (lst)
  "Insert org table (represented as a list of lists) at point."
  (org-table-lisp-to-string lst t))

;; simple-call-tree-info: CHECK
(defun org-table-insert-hlines (lst rows)
  "Insert hlines into an org table LST (represented as a list of lists).
The hlines will be inserted at the row numbers in the list ROWS (starting
with row 0). The hlines are inserted in order from smallest row to largest
so that the resulting list has hlines at the indices in ROWS."
  (let ((rows (sort rows '<)))
    (dolist (row rows)
      (setq lst (-insert-at row 'hline lst))))
  lst)

;; simple-call-tree-info: CHECK  
(defun org-table-transpose (tbl)
  "Transpose an org table (represented as a list of lists).
Rows will be changed to columns and vice-versa, after removing hlines."
  (let* ((table (delq 'hline tbl)))
    (mapcar (lambda (p)
              (let ((tp table))
                (mapcar
                 (lambda (rown)
                   (prog1
                       (pop (car tp))
                     (!cdr tp)))
                 table)))
            (car table))))

;; simple-call-tree-info: CHECK  
(cl-defun org-table-create-new (nrows ncols &optional (val ""))
  "Create a blank table with NROWS rows and NCOLS columns as a list of lists.
Optional argument VAL (a string) will be used to fill the cells of the table."
  (loop for n from 0 to (1- nrows)
        collect (make-list ncols val)))

;; TODO: could use -interleave, mapcar and flatten to do this (preferred if not slower)
;; simple-call-tree-info: TODO
(defun org-table-cbind (tbls &optional padding)
  "Join tables in TBLS (each represented as a lists of lists) together horizontally to form columns of new table.
If the tables have different numbers of rows then extra cells will be added to the end of some columns
to fill the gaps. These extra cells will be filled with the empty string by default, or you can supply a different
padding string with the optional PADDING arg.
hlines will be removed from the tables before joining them."
  (let* ((tbls (mapcar (cur 'delq 'hline) (delq nil tbls)))
	 (maxrows (-max (mapcar 'length tbls))))
    (loop for n from 0 to (1- maxrows)
	  collect (loop for tbl in tbls
			append (or (nth n tbl)
				   (make-list (org-table-ncols tbl) padding))))))

;; simple-call-tree-info: DONE
(defun org-table-rbind (tbls &optional padding)
  "Join tables in TBLS (each represented as a lists of lists) together vertically to form rows of new table.
If the tables have different numbers of columns then extra cells will be added to the end of some
rows to fill the gaps. These extra cells will be filled with the empty string by default, or you can supply
a different padding string with the optional PADDING arg.
hlines will be removed from the tables before joining them."
  (let* ((tbls (mapcar (cur 'delq 'hline) (delq nil tbls)))
	 (widths (mapcar 'org-table-ncols tbls))
	 (maxwidth (-max widths)))
    (loop for tbl in tbls
	  for width in widths
	  append (if (< width maxwidth) 
		     (org-table-cbind
		      (list tbl
			    (org-table-create-new (length tbl)
						  (- maxwidth width) padding)))
		   tbl))))

;; simple-call-tree-info: DONE
(defun org-table-rbind-named (tblnames &optional namescol padding)
  "Join tables with names/IDs in list TBLNAMES together vertically to form rows of new table, and return as a list.
If the optional argument NAMESCOL is non-nil an extra column will be added containing the name/ID of the table
corresponding to each row. If NAMESCOL is 'last the new column will be made the final column, for any other non-nil
value it will be made the first column.
If the PADDING arg is supplied it should be a string to use for padding extra cells (see `org-table-rbind')."
  (let* ((tables (cl-mapcar 'org-table-fetch tblnames (make-list (length tblnames) t)))
         (newtbl (org-table-rbind tables padding)))
    (if namescol
        (let ((newcol (loop for tbl in tables
                            for name in tblnames
                            for lst = (make-list (org-table-nrows tbl) (list name))
                            nconc lst)))
          (if (org-table-equal namescol 'last)
              (org-table-cbind (list newtbl newcol))
            (org-table-cbind (list newcol newtbl))))
      newtbl)))

;; simple-call-tree-info: REFACTOR
(defun org-table-filter-list (lst &optional cols rows filter)
  "Filter out rows and columns of a matrix LST represented as a list of lists.

Optional arguments ROWS & COLS are used to indicate which rows and columns to use.
By default all rows/cols are used. The optional FILTER argument is used to filter these selected rows.
Read on for more details.

The ROWS/COLS args may be lists containing a mixture of integers and sublists of 2 or 3 integers.
Individual integers in the list indicate individual rows/columns and sublists indicate ranges.
The `number-sequence' function is applied to sublists to obtain a sequence of integers, e.g. (2 10 2) results
in rows/columns 2, 4, 6, 8, & 10. The integer 0 indicates the final row/column and negative integers count
backward from the last row/column. Integers larger than the largest row/column or smaller than minus that
number indicate the last/first row/column respectively.
So for example \"((1 10) 20 (-9 0))\" selects the first 10 rows/cols, followed by the 20th row/col, followed by
the last 10 rows/cols. If ROWS/COLS is nil then all rows/cols (respectively) are returned.

FILTER should be an sexp that returns non-nil for rows to be included in the output.
It will be mapped over the rows indicated by the ROWS arg, or all rows if this is missing or nil.
The following variables may be used in the sexp:

n       : The number of rows processed so far (e.g: this can be used to limit the amount of output).
c<N>    : The string contained in column <N> of the current row, or nil if it is empty.
c<N>n   : The number contained in column <N> of the current row (using `string-to-number').
row     : A list containing all the items in the current row in order, as strings."
  (let* ((table (delq 'hline lst))
         (tablelength (length table))
         (tablewidth (org-table-ncols table))
         ;; deal with negative cols values
         (cols2 (delq nil (-tree-map (lambda (x) (if (> x 0) x (+ tablewidth x))) cols)))
         ;; expand pairs/triples into number sequences
         (cols3 (loop for x in cols2
                      if (listp x) nconc (number-sequence (first x) (second x) (third x))
                      else nconc (list x)))
         ;; now do the same for rows
         (rows2 (delq nil (-tree-map (lambda (x) (if (> x 0) x (+ tablelength x))) rows)))
         (rows3 (loop for x in rows2
                      if (listp x) nconc (number-sequence (first x) (second x) (third x))
                      else nconc (list x)))
         ;; select the required rows
         (table2 (if rows3 (-select-by-indices (mapcar '1- rows3) table)
		   table))
         ;; create the filter
         (filter2 (if filter
                      `(lambda (row)
                         (let* ,(mapcan (lambda (x)
                                          (let* ((varname (concat "c" (number-to-string x)))
                                                 (var (intern varname))
                                                 (numvar (intern (concat varname "n"))))
                                            `((,var (if (not (equal (nth ,(1- x) row) ""))
                                                        (nth ,(1- x) row)))
                                              (,numvar (string-to-number (nth ,(1- x) row))))))
                                        (number-sequence 1 (length (car table2))))
                           (setq n (1+ n))
                           ,filter))))
         (n -1)
         ;; apply the filter
         (tbllines (remove-if-not filter2 table2)))
    ;; select the required columns
    (if cols3 (mapcar (lambda (line) (-select-by-indices (mapcar '1- cols3) line))
		      tbllines)
      tbllines)))

;; (cl-defun org-table-transform-list (lst &key col row)
;;   "Apply transformations to a list of lists."
;;   )

;; simple-call-tree-info: DONE  
(cl-defun org-table-inherit-params (params &optional (propsname (plist-get params :name)))
  "Combine dynamic block PARAMS with those inherited from the corresponding org property named PROPSNAME.
By default PROPSNAME is obtained from the :name property in PARAMS (i.e. the name of the dynamic block type).
The inherited property should be in the same form as a parameter list for the corresponding dynamic block."
  (let* ((inherited (if propsname (org-entry-get (point) propsname t))))
    (if inherited (org-combine-plists (read (concat "(" inherited ")")) params) params)))

;;;###autoload
;; simple-call-tree-info: DONE  
(defun org-dblock-write:tablefilter (params)
  "Org dynamic block function to filter lines/columns of org tables.

The plist may contain the following parameters:

 :tblnames = the name or ID of the table to filter (quoted),
             or a list of such names/IDs (quoted & surrounded by brackets)
 :rows = a list of integers and ranges (surrounded by brackets) indicating which rows of
         the source table to use (see below for details). If nil then all rows are used.
 :cols = as above but indicating which columns to display in the result. If nil then all columns are displayed.
 :filter = an sexp for filtering the rows to be displayed (see below). If nil then all rows are displayed.
 :noerrors = if non-nil or if placed at the end of the parameter list then any rows which give an error when
             put through the filter will not be displayed. 
 :namescol = if non-nil then an extra column containing the name of the table corresponding to each row
             will be added. By default this new column will be the first column, but if the parameter
             value is last then the new column will be made the last column.

Any of these parameter may be inherited from an org property named :TABLEFILTER: (including :tblnames).
The value of this property should have the same form as the dynamic block parameter list, e.g:
   :PROPERTIES:
   :TABLEFILTER: :cols ((2 -1)) :rows ((1 10)) :namescol last :noerrors :tblnames (\"tbl1\" \"tbl2\")
   :END:
Parameters listed on the dynamic block statement get priority over inherited parameters.

The :rows and :cols arguments should be lists containing a mixture of integers and sublists of 2 or 3 integers.
Individual integers in the list indicate individual rows/columns and sublists indicate ranges.
The `number-sequence' function is applied to sublists to obtain a sequence of integers, e.g. (2 10 2) results
in rows/columns 2, 4, 6, 8, & 10. The integer 0 indicates the final row/column and negative integers count
backward from the last row/column. Integers larger than the largest row/column or smaller than minus that
number indicate the last/first row/column respectively.
So for example \":row ((1 10) 20 (-9 0))\" selects the first 10 rows, followed by the 20th row, followed by
the last 10 rows (and similarly for columns if :cols was used instead of :rows).

The :filter argument should be an sexp that returns non-nil for rows to be included in the output.
It will be mapped over the rows indicated by the :rows arg, or all rows if this is missing or nil.
The following variables may be used in the sexp:

n              : The number of rows processed so far (e.g: this can be used to limit the amount of output).
c<N>           : The string contained in column <N> of the current row.
c<N>n          : The number contained in column <N> of the current row (using `string-to-number').
row            : A list containing all the items in the current row in order, as strings.

Additionally the filter may make use of function bindings in `org-table-filter-function-bindings' (which see).

Some examples now follow.

From the table named \"account\" select all even rows from 2 to 100 inclusive, and columns 1-5 and 10.

#+BEGIN: tablefilter :tblname \"account\" :rows ((2 100 2)) :cols ((1 5) 10)

Select the first 10 rows where the first column is a date within 10 days of today, and the second column
is a number greater than 1000, returning just columns 2 & 3:

#+BEGIN: tablefilter :tblname \"account\" :filter (and (<= n 10) (> (days-to-now c1) -10) (> (num c2) 1000)) :cols (2 3)

Return the rows among the first 100 which match the word \"shopping\" in column 3 (note that c3 is or'ed with \"\" so
that rows in which c3 is empty dont cause an error):

#+BEGIN: tablefilter :tblname \"account\" :filter (string-match \"shopping\" (or c3 \"\")) :rows ((1 100))
"
  (destructuring-bind
      (tblnames tblname namescol noerrors noerror cols rows filter)
      (mapcar (cur 'plist-get (org-table-inherit-params params))
              '(:tblnames :tblname :namescol :noerrors :noerror :cols :rows :filter))
    (setq tblnames (or tblnames tblname) noerrors (or noerrors noerror))
    (org-table-insert
     ;; Note: for some reason this doesn't work if `flet' is replaced by `cl-flet'
     (eval `(flet ,(mapcar 'car org-table-filter-function-bindings)
	      (org-table-filter-list (if (listp tblnames) (org-table-rbind-named tblnames namescol "")
				       (org-table-fetch tblnames t))
				     cols rows (if noerrors
						   `(condition-case nil ,filter (error nil))
						 filter)))))))

;; simple-call-tree-info: DONE
(defun org-table-move-cell nil
  "Prompt for a direction and move the current cell in that direction.
The cell will swap places with the one in the direction chosen."
  (interactive)
  (org-table--move-cell (intern (completing-read "Direction: " '("up" "down" "left" "right"))))
  (org-table-align))

;; simple-call-tree-info: DONE
(defvar org-table-jump-condition (cons 'right t)
  "Cons cell used by `org-table-jump-next' to determine next cell to jump to.

The car should be a symbol to specify the direction of traversal across the org-table:
  - up/down : move up/down the current column & then to the next column on the left/right
  - left/right move left/right across the current row & then to the next row up/down.

The cdr can be either:

 - 1) A keyword matching an element of `org-table-jump-condition-presets', e.g. :empty
      (called a \"keyword condition\" in the following text).
 - 2) A regexp for matching the contents of the desired cell, e.g. \"foo\".
 - 3) A list containing a regexp followed by one or two numbers indicating the row & column
      offset (relative to the current cell) of the cell to match the regexp against, e.g.
      (\"foo\" 1 1) = cell 1 row above and 1 column to the left of a cell containing \"foo\"
 - 4) A cons cell containing the line & column number to jump to. The car may be any expression
      that evaluates to a number, and the cdr may be a number or symbol that evaluates to a number;
      e.g. (1 . 1) = top-left cell, ((1- numdlines) . numcols) = cell above bottom-right cell
      (note: you can also use the gotocell function described below).
 - 5) An sexp that evaluates to non-nil when the desired cell has been reached; this sexp may contain
      previously mentioned keyword conditions (which will be replaced by their corresponding forms)
      and any of the functions and variables listed below,
      e.g: (and :empty (matchfield \"bar\" 1 0)) = empty cell above a cell containing \"bar\"
 - 6) A list whose first element is either & or | to indicate the logical conjunction/disjunction of
      the subsequent elements. The subsequent elements may include any of the previously mentioned forms
      (keyword condition, regexp, cons cell, sexp) or recursive calls to &/|. For example:
      (& :empty (\"bar\" 1)) = empty cell above cell containing \"bar\"
      (| (& :empty \"bar\" 1) (& :nonempty (\"foo\" 0 -1)) (numdlines . numcols)) = same as previous match,
      but also match non-empty cells to the right of cells containing \"foo\", or the last cell in the table.
 - 7) A list containing the symbol `jmpseq' followed by a sequence of any of the previously mentioned items.
      Each call to `org-table-jump-next' will jump to the next item in this sequence. For example:
      (jmpseq (& :empty \"bar\" 1) (& :nonempty \"foo\" 0 -1) (numdlines . numcols)) = first jump to the next
      empty cell above one containing \"bar\", then jump to the next non-empty cell to the right of one
      containing \"foo\", then jump to the last cell, and repeat.

Each sexp can make use of the functions defined in `org-table-filter-function-bindings' which by default includes
the following functions. Many of these functions have optional ROFFSET & COFFSET args which refer to row & column 
offsets from the current cell, and allow you to act on cells neighbouring the current one. If ROFFSET or COFFSET
is left blank then they default to 0. Also note that cell refers to the position in a table and field to its contents.

 (field &optional ROFFSET COFFSET) = return the field in a cell.
 (matchfield REGEX &optional ROFFSET COFFSET) = call `string-match' with REGEX on a field.
 (setfield VALUE &optional ROFFSET COFFSET NOPROMPT) = change the contents of a cell.
 (replace-in-field REGEXP REP &optional ROFFSET COFFSET) = replace matches to REGEXP with REP in a field.
 (field2num &optional ROFFSET COFFSET) = read a field as a number and return it.
 (changenumber FUNC &optional roffset coffset noprompt) = apply FUNC to the number in a field and replace with the result.
 (flatten NROWS NCOLS FUNC REPS) = a wrapper around `org-table-flatten-columns'.
 (hline-p ROFFSET) = test if the row at ROFFSET rows beneath the current one, counting hlines, is an hline (horizontal line).
 (countcells DIR ROFFSET COFFSET &rest REGEXS) = moving in direction DIR (up/down/left/right) from a given cell offset, 
  return a list containing counts of sequential matches to the 1st regexp, followed by the 2nd regexp, etc.
 (checkcounts COUNTS BOUNDS) = a wrapper around `org-table-check-bounds'.
 (sumcounts DIR ROFFSET COFFSET &rest REGEXS) = similar to countcells but returns total No. of matches.
 (getvar KEY) = get the value associated with KEY in `org-table-jump-state'.
 (setvar KEY VAL) = set the value associated with KEY in `org-table-jump-state' to VAL.
 (checkvar KEY &rest VALS) = return t if value associated with KEY in `org-table-jump-state' is among VALS, and nil otherwise.
 (gotocell LINE &optional COL) = Jump immediately to cell in specified LINE & COL. If either arg is nil use the current line/column.

Be careful with the setfield, replace-in-field, changenum & flatten functions, only use them if your other jump conditions 
are satisfied otherwise you may end up changing more than you want.
getvar, setvar & checkvar are used for communicating state across invocations of `org-table-jump-next', which can be
used for creating more complex jump patterns.
You can also make use of the following variables:

 table = the org-table as a list of lists (as returned by `org-table-to-lisp')
 table-dlines = list of indices of data lines in table
 table-hlines = list of indices of horizontal lines in table
 numdlines = the number of data lines in the table
 numhlines = the number of horizontal lines in the table
 numlines = numdlines + numhlines
 numcols = the number of columns
 currentcol = the current column number
 currentline = the current data line number (i.e. excluding horizontal lines)
 startcol = the column that point was in at the start
 startline = the data line number that point was in at the start
 movedir = the current direction of field traversal ('up,'down,'left or 'right)
 numcells = the total number of cells in the table
 cellcount = the number of cells checked since the last match
 startpos = the position of point before starting.")

;; simple-call-tree-info: DONE
(defvar org-table-jump-condition-history nil
  "History list of `org-table-set-jump-condition'")

(defvar org-table-jump-state nil
  "State variable (alist) for use by `org-table-jump-next'.")

;; simple-call-tree-info: CHECK
(defcustom org-table-jump-condition-presets
  '(("1st<>last cell" . (jmpseq :first :last))
    ("1st<>last row" . (jmpseq :top :bottom))
    ("1st<>last col" . (jmpseq :left :right))
    (:first . (gotocell 1 1))
    (:last . (gotocell numdlines numcols))
    (:top . (gotocell 1))
    (:bottom . (gotocell numdlines))
    (:left . (gotocell nil 1))
    (:right . (gotocell nil numcols))
    (:hline-above . (hline-p -1))
    (:hline-below . (hline-p 1))
    (:empty . (matchfield "^\\s-*$"))
    (:nonempty . (matchfield "\\S-"))
    (:empty-above . (and (not (hline-p -1))
			 (checkcounts (countcells 'up 0 0 "\\S-" "^\\s-*$") '((1 1) 1))))
    (:empty-below . (and (not (hline-p 1))
			 (checkcounts (countcells 'down 0 0 "\\S-" "^\\s-*$") '((1 1) 1))))
    (:empty-left . (and (matchfield "\\S-")
			(matchfield "^\\s-*$" 0 -1)))
    (:empty-right . (and (matchfield "\\S-")
			 (matchfield "^\\s-*$" 0 1)))
    ("every other" . (> cellcount 1))
    ("replace empty" . (and (matchfield "^\\s-*$")
			    (setfield "-")))
    ("replace empty (no prompt)" .
     (and (matchfield "^\\s-*$")
	  (setfield "-" 0 0 t)))
    ("enter manually" . enter)
    ("edit preset" . edit))
  "Named presets for `org-table-jump-condition'.
Each element is a cons cell (KEY . SEXP) where KEY is either a keyword or a string description of the
condition evaluated by SEXP. If it is a keyword then it may also be used instead of the corresponding
 SEXP in `org-table-jump-condition' (either bare or within another sexp).
The SEXP may make use of functions defined in `org-table-filter-function-bindings'."
  :group 'org-table
  :type '(alist :key-type (string :tag "Description")
		:value-type (sexp :tag "Condition")))

;; simple-call-tree-info: DONE this is used in `one-key-regs-custom-register-types'
(defun org-table-describe-jump-condition (condition maxchars)
  "Return a string containing a description of jump CONDITION of length at most MAXCHARS."
  (let* ((name (or (car (rassoc condition org-table-jump-condition-presets))
		   (format "%s" condition)))
	 (str (if (keywordp name)
		  (substring (symbol-name name) 1)
		name)))
    (substring str 0 (min maxchars (length str)))))

(defun org-table-show-jump-condition nil
  "Display a message in the minibuffer showing the current jump condition."
  (interactive)
  (let ((msg "org-table-jump set to direction = %s, condition = %s"))
    (message msg
	     (car org-table-jump-condition)
	     (org-table-describe-jump-condition (cdr org-table-jump-condition)
						(- (frame-text-cols)
						   (length msg)
						   10)))))

;; simple-call-tree-info: TODO make selection using one-key instead of completing-read?
(defun org-table-set-jump-condition (condition)
  "Set the CONDITION for `org-table-jump-condition'.
If CONDITION is a string or keyword select the corresponding condition from `org-table-jump-condition-presets'.
When called interactively prompt the user to select from `org-table-jump-condition-presets'.
If the user chooses \"enter manually\" then they are prompted to enter an sexp, and if they
choose \"edit preset\" then they are prompted to choose an existing condition and edit it
in the minibuffer."
  (interactive (let* ((name (and org-table-jump-condition-presets
				 (completing-read
				  (substitute-command-keys
				   "Set jump condition for \\[org-table-jump-next]: ")
				  org-table-jump-condition-presets)))
		      (condition (if (string-match "^:" name)
				     (intern-soft name)
				   (cdr (assoc name org-table-jump-condition-presets)))))
		 (list (if (memq condition '(enter edit))
			   (read (read-string
				  "Condition (sexp): "
				  (when (eq condition 'edit)
				    (prin1-to-string
				     (cdr (assoc (completing-read
						  "Preset: "
						  (remove-if (lambda (x) (memq (cdr x) '(edit enter)))
							     org-table-jump-condition-presets))
						 org-table-jump-condition-presets))))
				  'org-table-jump-condition-history))
			 condition))))
  (setcdr org-table-jump-condition (or (and (stringp condition)
					    (cdr (assoc condition org-table-jump-condition-presets)))
				       condition)))

;; simple-call-tree-info: CHECK
(defun org-table-set-jump-direction (direction)
  "Set the DIRECTION for `org-table-jump-condition'; 'up, 'down, 'left or 'right.
When called interactively prompt the user to press a key for the DIRECTION."
  (interactive (list (let ((key (read-key "Press key for search direction: ")))
		       (case key
			 ((113 119 101 114 116 121 117 105 111 112) 'up)
			 ((122 120 99 118 98 110 109) 'down)
			 ((97 115 100 102 103) 'left)
			 ((104 106 107 108 59 39) 'right)
			 (t key)))))
  (setcar org-table-jump-condition direction))

;; simple-call-tree-info: CHECK
(defun org-table-set-relative-field (value noprompt line col)
  "Set contents of field at position LINE, COL to VALUE.
Prompt the user unless NOPROMPT is non-nil.
Careful! only use after you've checked the cell satisfies your other jump conditions."
  (save-excursion
    (org-table-goto-line line)
    (org-table-goto-column col)
    (when (or noprompt
	      (y-or-n-p (format "Change field in line %d, column %d to \"%s\""
				line col value)))
      (org-table-blank-field)
      (insert value)))
  (org-table-goto-column col))

;; simple-call-tree-info: TODO; fix documentation
(defun org-table-count-matching-fields (table dlines direction row col nrows ncols &rest regexs)
  "Count fields in TABLE matching REGEXS sequentially in a given DIRECTION.
TABLE should be a list of lists are returned by `org-table-to-lisp'.
DLINES should be a list of indices of the rows of TABLE that correspond to data line (i.e. not hlines).
DIRECTION can be ('up, 'down, 'left or 'right) to indicate the direction of movement from the starting cell. 
You can use the variable `movedir' in `org-table-jump-condition' for the current direction of movement.
Starting with cell in ROW & COL position, fields are traversed sequentially in the given DIRECTION,
and matched against the first regexp, when the first mismatch occurs the next regexp is tried 
and used for matching subsequent fields until a mismatch etc. until there is a mismatch with the last regexp.
The return value is a list of counts of matches for each regexp.
This can be used for finding cells based on the content of neighbouring cells."
  (let* ((counts (make-list (length regexs) 0))
	 (row (1- row)) (col (1- col)) (r 0) (c 0)
	 (incfn `(lambda nil ,(case direction
				(up '(decf r))
				(down '(incf r))
				(left '(decf c))
				(right '(incf c))
				(t (error "Invalid direction"))))))
    (dotimes (i (length regexs))
      (while (let ((col2 (+ col c))
		   (row2 (+ row r)))
	       (and (<= 0 col2 ncols)
		    (<= 0 row2 nrows)
		    (string-match (nth i regexs)
				  (nth col2
				       (nth (nth row2 dlines) table)))))
	(incf (nth i counts))
	(funcall incfn)))
    counts))

;; simple-call-tree-info: CHECK  
(defun org-table-check-bounds (counts bounds)
  "Check BOUNDS of each number in COUNTS.
COUNTS should be a list of numbers, and BOUNDS a list of the same length containing
a mixture of numbers and lists of length 2. Each number c in COUNTS is checked against
the corresponding element b in BOUNDS as follows: if b is a single number check (>= c b),
if b is a list check (<= (first b) c (second b)). 
If any bound is not satisfied nil is returned, otherwise non-nil."
  (-all-p 'identity (cl-mapcar (lambda (c b)
				 (if (numberp b)
				     (>= c b)
				   (<= (first b) c (second b))))
			       counts bounds)))

(defcustom org-table-timestamp-patterns
  '("yyyy/MM/dd HH:mm:ss" "yyyy/MM/dd HH:mm" "yyyy/MM/dd" 
    "yyyy-MM-dd HH:mm:ss" "yyyy-MM-dd HH:mm" "yyyy-MM-dd" 
    "yyyy:MM:dd HH:mm:ss" "yyyy:MM:dd HH:mm" "yyyy:MM:dd" 
    "dd/MM/yyyy HH:mm:ss" "dd/MM/yyyy HH:mm" "dd/MM/yyyy" 
    "dd-MM-yyyy HH:mm:ss" "dd-MM-yyyy HH:mm" "dd-MM-yyyy" 
    "dd:MM:yyyy HH:mm:ss" "dd:MM:yyyy HH:mm" "dd:MM:yyyy")
  "List of java style date-time matching patterns as accepted by `datetime-matching-regexp' and related functions.
If you set this outside the customization framework you should make sure to also update `org-table-timestamp-regexp',
and `org-table-timestamp-parsers',
e.g: (setq 'org-table-timestamp-patterns '(\"yy/MM/dd\")
	   'org-table-timestamp-regexp 
	   (substring
	    (cl-loop for pattern in patterns
		     concat (concat \"\\(?:\" (datetime-matching-regexp 'java pattern) \"\\)\\|\"))
	    0 -2)
	   'org-table-timestamp-parsers
	   (cl-loop for pattern in patterns
		    collect (datetime-parser-to-float 'java pattern :timezone 'system)))"
  :group 'org-table
  :type '(repeat (string :tag "Pattern" :help-echo "A java-style date-time pattern"))
  :require 'datetime
  :set (lambda (sym patterns)
	 (setq sym patterns)
	 (setq org-table-timestamp-regexp
	       (concat "\\(?:" (substring
				(cl-loop for pattern in patterns
					 concat (concat "\\(" (datetime-matching-regexp 'java pattern) "\\)\\|"))
				0 -2) "\\)")
	       org-table-timestamp-parsers
	       (cl-loop for pattern in patterns
			collect (datetime-parser-to-float 'java pattern :timezone 'system)))))

(defvar org-table-timestamp-regexp
  (concat "\\(?:"
	  (substring
	   (cl-loop for pattern in org-table-timestamp-patterns
		    concat (concat "\\(" (datetime-matching-regexp 'java pattern) "\\)\\|"))
	   0 -2) "\\)")
  "A regular expression for matching any of the patterns in `org-table-timestamp-patterns'.
This will be automatically update when `org-table-timestamp-patterns' is updated.
Each pattern will be enclosed in parentheses and separated by \\| so you can obtain the required 
substring match using `match-string'.")

(defvar org-table-timestamp-parsers
  (cl-loop for pattern in org-table-timestamp-patterns
	   collect (datetime-parser-to-float 'java pattern :timezone 'system))
  "A list of timestamp parsers corresponding to patterns in `org-table-timestamp-patterns'.
This will be automatically updated when `org-table-timestamp-patterns' is updated.")

(defcustom org-table-timestamp-format (car org-time-stamp-formats)
  "Default format for timestamps output by `org-table-convert-timestamp'."
  :group 'org-table
  :type 'string)

;;;###simple-call-tree-info: CHECK
(defun org-table-convert-timestamp (field &optional outfmt patterns)
  "Replace timestamp in FIELD with datetime in format specified by OUTFMT, and return a new string.
Optional arg PATTERNS is a list of java style date-time matching patterns. 
By default `org-table-timestamp-patterns' is used. 
OUTFMT is a POSIX format string as used by `format-time-string' for the replacement timestamp which
by default is `org-table-timestamp-format'."
  (let* ((regex (if patterns
		    (concat "\\(?:" (substring
				     (cl-loop for pattern in patterns
					      concat (concat "\\(" (datetime-matching-regexp 'java pattern) "\\)\\|"))
				     0 -2) "\\)")
		  org-table-timestamp-regexp))
	 (parsers (if patterns
		      (cl-loop for pattern in patterns
			       collect (datetime-parser-to-float
					'java pattern :timezone 'system))
		    org-table-timestamp-parsers)))
    (when (string-match regex field)
      (let* ((pos (cl-position-if-not
		   'null (mapcar (lambda (i) (match-string i field))
				 (number-sequence 1 (length parsers)))))
	     (substr (match-string (1+ pos) field))
	     (unixts (funcall (nth pos parsers) substr)))
	(string-replace substr
			(format-time-string (or outfmt org-table-timestamp-format) unixts)
			field)))))

;; simple-call-tree-info: CHECK
(defun org-table-jump-flatten-cells (nrows ncols func reps)
  "Wrapper of `org-table-flatten-columns' for setting NROWS, NCOLS, FUNC & REPS when used in `org-table-jump-condition'."
  (let ((nrows (if (eq nrows 'prompt)
		   (read-number "Number of rows (-ve numbers count backwards): ")
		 (or nrows
		     (and (not current-prefix-arg)
			  (cdr (assoc 'flattenrows org-table-jump-state)))
		     (and (not (listp current-prefix-arg))
			  (prefix-numeric-value current-prefix-arg))
		     (read-number "Number of rows (-ve numbers count backwards): "))))
	(ncols (if (eq ncols 'prompt)
		   (read-number "Number of rows (-ve numbers count backwards): ")
		 (or ncols
		     (and current-prefix-arg
			  (listp current-prefix-arg)
			  (read-number "Number of rows (-ve numbers count backwards): "))
		     (cdr (assoc 'flattencols org-table-jump-state))
		     1)))
	(func (if (eq func 'prompt)
		  (ido-choose-function org-table-flatten-functions nil
				       "User function with one arg (list of fields): " t)
		(or func
		    (cdr (assoc 'flattenfunc org-table-jump-state))
		    (lambda (lst) (mapconcat 'identity lst " ")))))
	(reps (if (eq reps 'prompt)
		  (read-number "Number or repetitions: " 1)
		(or (cdr (assoc 'flattenreps org-table-jump-state))
		    1))))
    (setf (alist-get 'flattenrows org-table-jump-state) nrows)
    (setf (alist-get 'flattencols org-table-jump-state) ncols)
    (setf (alist-get 'flattenfunc org-table-jump-state) func)
    (setf (alist-get 'flattenreps org-table-jump-state) reps)
    (org-table-flatten-columns nrows ncols func reps)))

;; simple-call-tree-info: CHECK
(defun org-table-parse-jump-condition (jmpcnd)
  (pcase jmpcnd
    ((pred keywordp)
     (cdr (assoc jmpcnd org-table-jump-condition-presets)))
    ((pred stringp) `(matchfield ,jmpcnd))
    ((and (pred listp)
	  (app car (pred stringp)))
     `(matchfield ,@jmpcnd))
    ((and (pred consp)
	  (app cdr (pred (lambda (x) (funcall (-orfn 'symbolp 'integerp) x)))))
     `(gotocell ,(car jmpcnd) ,(cdr jmpcnd)))
    ((and (pred listp)
	  (app car op)
	  (app cdr conds)
	  (app car (or '& '|)))
     `(,(case op (| 'or) (& 'and)) ,@(mapcar 'org-table-parse-jump-condition conds)))
    (_ jmpcnd)))

;; simple-call-tree-info: DONE
(defun org-table-jump-next (steps &optional stopcond movedir)
  "Jump to the STEPS next field in the org-table at point matching `org-table-jump-condition'.
If STEPS is negative jump to the -STEPS previous field. 
If STOPCOND &/or MOVEDIR are non-nil set `org-table-jump-condition' to these values.
STOPCOND can be anything accepted by `org-table-set-jump-condition' (which see).
When called interactively STEPS will be set to the numeric value of prefix arg (1 by default).
If a single \\[universal-argument] prefix is used, prompt for STOPCOND (which can be a preset one,
or if \"enter manually\" or \"edit preset\" is chosen then a new one edited in the minibuffer), 
and if more than one \\[universal-argument] prefix is used also prompt for MOVEDIR. 
In both these cases STEPS is set to 1."
  (interactive "p")
  (let ((doprompt (and (not (null current-prefix-arg))
		       (listp current-prefix-arg))))
    (if stopcond (org-table-set-jump-condition stopcond)
      (if doprompt (call-interactively 'org-table-set-jump-condition)))
    (if movedir (org-table-set-jump-direction movedir)
      (if (and doprompt (>= steps 16))
	  (call-interactively 'org-table-set-jump-direction)))
    (when doprompt (setq steps 1) (org-table-show-jump-condition)))
  (unless (org-at-table-p) (error "Point is not in an org-table"))
  (let* ((table (org-table-to-lisp))
	 (table-hlines (-find-indices 'symbolp (org-table-to-lisp)))
	 (table-dlines (-find-indices 'listp (org-table-to-lisp)))
	 (numlines (length table))
	 (numhlines (length table-hlines))
	 (numdlines (length table-dlines))
	 (numcols (length (nth (car table-dlines) table)))
	 (numcells (* numdlines numcols))
	 (startcol (org-table-current-column))
	 (currentcol startcol)
	 (startline (org-table-current-line))
	 (currentline startline)
	 (movedir (car org-table-jump-condition))
	 ;; right & down movement count cells from top-left
	 ;; left & up movement count cells from bottom-right
	 (offsetr (+ (1- startcol) (* (1- startline) numcols)))
	 (offsetl (+ (- numcols startcol) (* (- numdlines startline) numcols)))
	 (offsetd (+ (1- startline) (* (1- startcol) numdlines)))
	 (offsetu (+ (- numdlines startline) (* (- numcols startcol) numdlines)))
	 (move-right (function (lambda (x) (setq currentcol (1+ (% (+ x offsetr) numcols))
						 currentline (1+ (mod (/ (+ x offsetr) numcols) numdlines))))))
	 (move-left (function (lambda (x) (setq currentcol (- numcols (% (+ x offsetl) numcols))
						currentline (- numdlines (mod (/ (+ x offsetl) numcols) numdlines))))))
	 (move-down (function (lambda (x) (setq currentline (1+ (% (+ x offsetd) numdlines))
						currentcol (1+ (mod (/ (+ x offsetd) numdlines) numcols))))))
	 (move-up (function (lambda (x) (setq currentline (- numdlines (% (+ x offsetu) numdlines))
					      currentcol (- numcols (mod (/ (+ x offsetu) numdlines) numcols)))))) 
	 (movefn (case movedir
		   (up (if (> steps 0) move-up move-down))
		   (down (if (> steps 0) move-down move-up))
		   (left (if (> steps 0) move-left move-right))
		   (right (if (> steps 0) move-right move-left))
		   (t (error "Invalid direction"))))
	 (cellcount 0)
	 (matchcount 0)
	 (startpos (point))
	 (startfield (nth (1- startcol) (nth (nth (1- startline) table-dlines) table))))
    (eval `(cl-labels ,(mapcar 'car org-table-filter-function-bindings)
	     (symbol-macrolet ,(mapcar '-cons-to-list
				       (--filter (keywordp (car it)) org-table-jump-condition-presets))
	       (while (< matchcount (abs steps))
		 (setq cellcount 1)
		 (funcall movefn cellcount)
		 (while (and (< cellcount numcells)
			     (not ,(let ((jmpcond
					  (let* ((jmplst (cdr org-table-jump-condition))
						 (jmpidx (cdr (assoc 'jmpidx org-table-jump-state)))
						 (newjmpidx (if (> steps 0) (1+ jmpidx)
							      (1- jmpidx))))
					    (if (and (listp jmplst)
						     (eq (car jmplst) 'jmpseq))
						(prog1 (nth (1+ (mod (or newjmpidx 0)
								     (1- (length jmplst))))
							    jmplst)
						  (setf (alist-get 'jmpidx org-table-jump-state)
							(mod newjmpidx (1- (length jmplst)))))
					      (setf (alist-get 'jmpidx org-table-jump-state) 0)
					      jmplst))))
				     (org-table-parse-jump-condition jmpcond))))
		   (incf cellcount)
		   (funcall movefn cellcount))
		 (incf matchcount)))))
    (org-table-goto-line currentline)
    (org-table-goto-column currentcol)
    (cons currentline currentcol)))

;; simple-call-tree-info: DONE  
(defun org-table-jump-prev (steps &optional stopcond movedir)
  "Like `org-table-jump-next' but jump STEPS in opposite direction.
STOPCOND & MOVEDIR args are same as for `org-table-jump-next'."
  (interactive "p")
  (org-table-jump-next (- steps) stopcond movedir))

;; simple-call-tree-info: CHECK
(defun org-table-browse-tables (&optional namedp)
  "Browse tables in the current buffer using `occur'.
If NAMEDP is non-nil only list named tables."
  (interactive "P")
  (occur (if namedp "\\+\\(TBL\\|tbl\\)\\(NAME\\|name\\):"
	   "^\\s-*[^|].*\n\\s-*|.*"))
  (pop-to-buffer "*Occur*"))


;; IDEAS:
;; - table specific jump sequences; stored in #+TBLJMP: footer and loaded automatically by new command `org-table-jump-default'
;;    different conditions/sequences for different prefix keys.
;;    Have an option in org-table-set-jump-condition which selects the table specific jump condition, and a command to save
;;    the current jump condition/sequence to the #+TBLJMP footer.

(provide 'org-table-jb-extras)

;; (org-readme-sync)
;; (magit-push)

;;; org-table-jb-extras.el ends here
