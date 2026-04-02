;;; org-milestone-table.el --- Milestone timeline tables for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author: Gavin Hughes
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.5"))
;; Homepage: https://github.com/gavinhughes/org-milestone-table
;; Keywords: outlines, calendar, convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; org-milestone-table provides commands for managing milestone timeline
;; tables inside Org mode buffers.  Define milestones with IDs, predecessor
;; relationships and date offsets, then compute dates automatically.
;;
;; Usage:
;;   M-x org-milestone-table-new       Insert a blank milestone table
;;   M-x org-milestone-table-update-timeline  Compute dates from predecessors
;;   M-x org-milestone-table-add-missing-ids  Auto-number rows without IDs
;;   M-x org-milestone-table-sort-by-date     Sort rows by computed date
;;
;; Predecessor syntax:
;;   3+10d   means: 10 days after milestone 3
;;   3-2w    means: 2 weeks before milestone 3
;;   3+1m    means: 1 month after milestone 3
;;   5       means: same date as milestone 5 (shorthand for 5+0d)
;;   1+5d,2+3d  means: the later of (1+5d, 2+3d)

;;; Code:

(require 'org)
(require 'org-table)
(require 'calendar)
(require 'cl-lib)

(declare-function evil-insert-state "evil-states" ())

(defvar omt--errors nil
  "Temporary error accumulator for `omt--resolve'.
Declared as a special variable so dynamic binding works with `symbol-value'.")

(defgroup org-milestone-table nil
  "Milestone timeline tables for Org mode."
  :group 'org
  :prefix "org-milestone-table-"
  :link '(url-link "https://github.com/gavinhughes/org-milestone-table"))

;;;###autoload
(defun org-milestone-table-new ()
  "Insert an empty milestone table at point."
  (interactive)
  (insert "| ID | Pred | Date | Status | Milestone |
|----+------+------+--------+-----------|
|    |      |      |        |           |
")
  (forward-line -1)
  (org-table-goto-column 1)
  (when (bound-and-true-p evil-mode)
    (evil-insert-state)))

(defun omt--parse-pred-list (pred)
  "Split PRED on commas, trim each element, return list of strings."
  (mapcar #'string-trim (split-string pred "," t "[ \t]+")))

(defun omt--parse-header ()
  "Parse header row at point.  Return (ncols col-id col-pred col-date)."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (cells (omt--row-cells line))
         (ncols (length cells))
         (col-id nil)
         (col-pred nil)
         (col-date nil))
    (cl-loop for cell in cells
             for i from 0
             do (let ((name (downcase (string-trim cell))))
                  (cond
                   ((string= name "id") (setq col-id i))
                   ((or (string= name "pred")
                        (string= name "predecessors")) (setq col-pred i))
                   ((string= name "date") (setq col-date i)))))
    (unless col-id (user-error "No ID column"))
    (unless col-pred (user-error "No Pred column"))
    (unless col-date (user-error "No Date column"))
    (list ncols col-id col-pred col-date)))

(defun omt--row-cells (line)
  "Split table LINE into cell strings, preserving empty cells.
Strips the leading and trailing | delimiters, then splits on |.
Each cell is trimmed of whitespace."
  (let ((s (string-trim line)))
    ;; Remove leading |
    (when (string-prefix-p "|" s)
      (setq s (substring s 1)))
    ;; Remove trailing |
    (when (string-suffix-p "|" s)
      (setq s (substring s 0 (1- (length s)))))
    (mapcar #'string-trim (split-string s "|"))))

(defun omt--parse-date (s)
  "Parse YYYY-MM-DD into (month day year) or nil."
  (when (and (stringp s)
             (string-match
              "\\`\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\'"
              s))
    (list (string-to-number (match-string 2 s))
          (string-to-number (match-string 3 s))
          (string-to-number (match-string 1 s)))))

(defun omt--format-date (greg)
  "Format gregorian (month day year) as YYYY-MM-DD."
  (let ((mm (car greg))
        (dd (car (cdr greg)))
        (yy (car (cdr (cdr greg)))))
    (format "%04d-%02d-%02d" yy mm dd)))

(defun omt--normalize-pred (p)
  "Normalize pred P.  Bare number N becomes N+0d."
  (cond
   ((string-match "\\`[0-9]+[+-][0-9]+[dwm]\\'" p) p)
   ((string-match "\\`[0-9]+\\'" p) (concat p "+0d"))
   (t p)))

(defun omt--fuzzy-id-p (id)
  "Return non-nil if ID is a fuzzy milestone ID (digits followed by `?').
Examples: \"15?\" => t, \"15\" => nil, \"?\" => nil, \"15??\" => nil."
  (and (stringp id)
       (string-match-p "\\`[0-9]+\\?\\'" id)))

(defun omt--fuzzy-id-base (id)
  "Return the base ID string for a fuzzy ID (strip trailing `?').
Assumes ID already satisfies `omt--fuzzy-id-p'."
  (substring id 0 (1- (length id))))

(defun omt--apply-offset (base op n unit)
  "Apply offset to BASE absolute date.  Return new absolute date."
  (let ((plus (string= op "+")))
    (if (string= unit "m")
        (let* ((greg (calendar-gregorian-from-absolute base))
               (mm (car greg))
               (dd (car (cdr greg)))
               (yy (car (cdr (cdr greg))))
               (tot (+ (* yy 12) (1- mm)))
               (ntot (if plus (+ tot n) (- tot n)))
               (new-yy (/ ntot 12))
               (new-mm (1+ (mod ntot 12)))
               (maxd (calendar-last-day-of-month new-mm new-yy))
               (new-dd (min dd maxd)))
          (calendar-absolute-from-gregorian (list new-mm new-dd new-yy)))
      (let* ((days (if (string= unit "w") (* n 7) n))
             (off (if plus days (- days))))
        (+ base off)))))

(defun omt--resolve (row tbl errs-sym visited)
  "Resolve date for ROW.  TBL is id-to-row hash.
ERRS-SYM is a symbol whose value accumulates errors.
VISITED is list of IDs seen so far for cycle detection."
  (cl-block omt--resolve
    (let ((id (plist-get row :id))
          (pred (plist-get row :pred))
          (date (plist-get row :date)))
      ;; Cycle check — fuzzy IDs are soft placeholders and do not
      ;; participate in the dependency graph, so skip them here.
      (when (and id (not (omt--fuzzy-id-p id)) (member id visited))
        (push (format "Cycle at ID %s" id) (symbol-value errs-sym))
        (cl-return-from omt--resolve nil))
      (let ((vis (if (and id (not (omt--fuzzy-id-p id))) (cons id visited) visited)))
        (cond
         ;; No predecessor: use literal date
         ((null pred)
          (when date
            (let ((pd (omt--parse-date date)))
              (if pd
                  (calendar-absolute-from-gregorian pd)
                (push (format "Bad date: %s" date) (symbol-value errs-sym))
                nil))))
         ;; Has predecessor(s)
         (t
          (let ((preds (omt--parse-pred-list pred))
                (dates nil)
                (failed nil))
            (dolist (p preds)
              (let ((pn (omt--normalize-pred p)))
                (if (not (string-match
                          "\\`\\([0-9]+\\)\\([+-]\\)\\([0-9]+\\)\\([dwm]\\)\\'"
                          pn))
                    (progn
                      (push (format "Bad pred: %s" p) (symbol-value errs-sym))
                      (setq failed t))
                  (let* ((rid (match-string 1 pn))
                         (op (match-string 2 pn))
                         (n (string-to-number (match-string 3 pn)))
                         (unit (match-string 4 pn))
                         (rrow (gethash rid tbl)))
                    (if (null rrow)
                        (progn
                          (push (format "Unknown ID: %s" rid)
                                (symbol-value errs-sym))
                          (setq failed t))
                      (let ((rabs (omt--resolve rrow tbl errs-sym vis)))
                        (if (null rabs)
                            (progn
                              (push (format "Cannot resolve ID %s" rid)
                                    (symbol-value errs-sym))
                              (setq failed t))
                          (push (omt--apply-offset rabs op n unit)
                                dates))))))))
            (if (or failed (null dates))
                nil
              (apply #'max dates)))))))))

(defun omt--display-errors (errors)
  "Display ERRORS in a dedicated read-only buffer and pop it up."
  (let ((buf (get-buffer-create "*Milestone Table Errors*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%d error(s) in milestone table:\n\n"
                        (length errors)))
        (dolist (e errors)
          (insert (format "  \u2022 %s\n" e))))
      (special-mode)
      (goto-char (point-min)))
    (display-buffer buf
                   '((display-buffer-at-bottom)
                     (window-height . 0.25)))))

;;;###autoload
(defun org-milestone-table-update-timeline ()
  "Update dates in org milestone table at point."
  (interactive)
  (save-excursion
    (unless (org-at-table-p)
      (user-error "Not in a table"))
    (goto-char (org-table-begin))
    (let* ((hdr (omt--parse-header))
           (ncols (car hdr))
           (col-id (car (cdr hdr)))
           (col-pred (car (cdr (cdr hdr))))
           (col-date (car (cdr (cdr (cdr hdr)))))
           (rows nil)
           (id-to-row (make-hash-table :test 'equal))
           (omt--errors nil)
           (errs-sym 'omt--errors))
      ;; Skip header and hlines
      (forward-line 1)
      (while (and (looking-at "^[ \t]*|")
                  (looking-at "^[ \t]*|[-+]"))
        (forward-line 1))
      ;; Parse data rows
      (while (and (looking-at "^[ \t]*|")
                  (not (looking-at "^[ \t]*|[-+]")))
        (let* ((lb (point))
               (ln (buffer-substring-no-properties lb (line-end-position)))
               (cs (omt--row-cells ln)))
          (when (>= (length cs) ncols)
            (let* ((ids (string-trim (nth col-id cs)))
                   (pds (string-trim (nth col-pred cs)))
                   (dts (string-trim (nth col-date cs)))
                   (r (list :line-beg lb
                            :id (unless (string-empty-p ids) ids)
                            :pred (unless (string-empty-p pds) pds)
                            :date (unless (string-empty-p dts) dts))))
              (push r rows)
              (when (plist-get r :id)
                (if (gethash (plist-get r :id) id-to-row)
                    (push (format "Duplicate ID: %s" (plist-get r :id))
                          (symbol-value errs-sym))
                  (puthash (plist-get r :id) r id-to-row))))))
        (forward-line 1))
      (setq rows (nreverse rows))
      ;; Resolve and collect updates
      (let ((updates nil))
        (dolist (r rows)
          (when (plist-get r :pred)
            (let ((a (omt--resolve r id-to-row errs-sym nil)))
              (when a
                (push (cons (plist-get r :line-beg)
                            (omt--format-date
                             (calendar-gregorian-from-absolute a)))
                      updates)))))
        (if omt--errors
            (progn
              (setq omt--errors (delete-dups (nreverse omt--errors)))
              (omt--display-errors omt--errors))
          ;; Apply in reverse buffer order
          (setq updates (sort updates (lambda (a b) (> (car a) (car b)))))
          (let ((col-re
                 (concat "^[ \t]*|"
                         (apply #'concat
                                (make-list col-date "[^|]*|"))
                         "\\([^|]*\\)|")))
            (dolist (upd updates)
              (goto-char (car upd))
              (when (looking-at col-re)
                (let* ((beg (match-beginning 1))
                       (end (match-end 1))
                       (old (buffer-substring beg end))
                       (nd (cdr upd))
                       (tw (length old))
                       (nc (if (string-match "^\\([ \t]*\\)\\S-" old)
                               (let* ((ld (match-string 1 old))
                                      (cs (concat ld nd))
                                      (pad (max 1 (- tw (length cs)))))
                                 (concat cs (make-string pad ?\s)))
                             (let* ((ln (max 1 (/ (- tw (length nd)) 2)))
                                    (tn (max 1 (- tw (length nd) ln))))
                               (concat (make-string ln ?\s)
                                       nd
                                       (make-string tn ?\s))))))
                  (goto-char beg)
                  (delete-region beg end)
                  (insert nc)))))
          (goto-char (org-table-begin))
          (org-table-align)
          (message "Updated %d date(s)." (length updates)))))))

;;;###autoload
(defun org-milestone-table-add-missing-ids ()
  "Add sequential IDs to rows in the milestone table that lack one.
Finds the maximum existing numeric ID and assigns incrementing IDs
starting from max+1."
  (interactive)
  (save-excursion
    (unless (org-at-table-p)
      (user-error "Not in a table"))
    (goto-char (org-table-begin))
    (let* ((hdr (omt--parse-header))
           (ncols (car hdr))
           (col-id (car (cdr hdr)))
           (max-id 0)
           (rows-without-id nil))
      ;; Skip header and hlines
      (forward-line 1)
      (while (and (looking-at "^[ \t]*|")
                  (looking-at "^[ \t]*|[-+]"))
        (forward-line 1))
      ;; Scan data rows
      (while (and (looking-at "^[ \t]*|")
                  (not (looking-at "^[ \t]*|[-+]")))
        (let* ((lb (point))
               (ln (buffer-substring-no-properties lb (line-end-position)))
               (cs (omt--row-cells ln))
               (ids (when (>= (length cs) ncols)
                      (string-trim (nth col-id cs)))))
          (cond
           ((and ids (not (string-empty-p ids))
                 (string-match-p "\\`[0-9]+\\'" ids))
            (setq max-id (max max-id (string-to-number ids))))
           ((or (null ids) (string-empty-p ids))
            (push lb rows-without-id))))
        (forward-line 1))
      ;; Assign IDs in reverse buffer order to preserve positions
      (setq rows-without-id (nreverse rows-without-id))
      (let ((next-id (1+ max-id))
            (col-re (concat "^[ \t]*|"
                            (apply #'concat
                                   (make-list col-id "[^|]*|"))
                            "\\([^|]*\\)|"))
            (count 0)
            (assignments nil))
        ;; Build assignments in buffer order (top to bottom)
        (dolist (lb rows-without-id)
          (push (cons lb next-id) assignments)
          (setq next-id (1+ next-id)))
        ;; Apply in reverse buffer order to preserve positions
        (dolist (pair (sort assignments (lambda (a b) (> (car a) (car b)))))
          (let ((lb (car pair)))
            (setq next-id (cdr pair))
          (goto-char lb)
          (when (looking-at col-re)
            (let* ((beg (match-beginning 1))
                   (end (match-end 1))
(old (buffer-substring beg end))
                   (id-str (number-to-string next-id))
                   (tw (length old))
                   (nc (if (string-match "^\\([ \t]*\\)\\S-" old)
                           (let* ((ld (match-string 1 old))
                                  (cs (concat ld id-str))
                                  (pad (max 1 (- tw (length cs)))))
                             (concat cs (make-string pad ?\s)))
                         (let* ((ln (max 1 (/ (- tw (length id-str)) 2)))
                                (tn (max 1 (- tw (length id-str) ln))))
                           (concat (make-string ln ?\s)
                                   id-str
                                   (make-string tn ?\s))))))
              (goto-char beg)
              (delete-region beg end)
              (insert nc)
              (cl-incf count)))))
        (goto-char (org-table-begin))
        (org-table-align)
        (message "Added %d ID(s), starting from %d." count (1+ max-id))))))

;;;###autoload
(defun org-milestone-table-sort-by-date ()
  "Sort data rows of the milestone table at point by the Date column.
Rows without a date are sorted to the end.  Rows whose ID matches the
fuzzy pattern (digits followed by `?', indicating an unknown predecessor)
are placed immediately before the row whose ID is the base number; if that
base row is absent, they are placed before the trailing block of undated rows."
  (interactive)
  (save-excursion
    (unless (org-at-table-p)
      (user-error "Not in a table"))
    (goto-char (org-table-begin))
    (let* ((hdr (omt--parse-header))
           (ncols (nth 0 hdr))
           (col-id   (nth 1 hdr))
           (col-date (nth 3 hdr))
           (data-start nil)
           (data-end nil)
           (row-lines nil))
      ;; Skip header and hlines
      (forward-line 1)
      (while (and (looking-at "^[ \t]*|")
                  (looking-at "^[ \t]*|[-+]"))
        (forward-line 1))
      (setq data-start (point))
      ;; Collect triples: (abs-date id line-text)
      (while (and (looking-at "^[ \t]*|")
                  (not (looking-at "^[ \t]*|[-+]")))
        (let* ((ln (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
               (cs (omt--row-cells ln))
               (dts (when (>= (length cs) ncols)
                      (string-trim (nth col-date cs))))
               (ids (when (>= (length cs) ncols)
                      (let ((s (string-trim (nth col-id cs))))
                        (unless (string-empty-p s) s))))
               (abs-date (when (and dts (not (string-empty-p dts)))
                           (let ((pd (omt--parse-date dts)))
                             (when pd
                               (calendar-absolute-from-gregorian pd))))))
          (push (list abs-date ids ln) row-lines))
        (forward-line 1))
      (setq data-end (point))
      (setq row-lines (nreverse row-lines))
      ;; Partition into fuzzy rows and normal rows
      (let ((fuzzy-rows nil)
            (normal-rows nil))
        (dolist (r row-lines)
          (if (omt--fuzzy-id-p (nth 1 r))
              (push r fuzzy-rows)
            (push r normal-rows)))
        (setq fuzzy-rows (nreverse fuzzy-rows))
        (setq normal-rows (nreverse normal-rows))
        ;; Sort normal rows: dated ascending, undated last
        (setq normal-rows
              (sort normal-rows
                    (lambda (a b)
                      (cond
                       ((and (nth 0 a) (nth 0 b)) (< (nth 0 a) (nth 0 b)))
                       ((nth 0 a) t)
                       (t nil)))))
        ;; Splice each fuzzy row before its base row
        (let ((result normal-rows))
          (dolist (frow fuzzy-rows)
            (let* ((base (omt--fuzzy-id-base (nth 1 frow)))
                   (splice-pos nil)
                   (fallback nil)
                   (i 0))
              (dolist (r result)
                (when (and (null splice-pos) (equal (nth 1 r) base))
                  (setq splice-pos i))
                (when (and (null fallback) (null (nth 0 r)))
                  (setq fallback i))
                (cl-incf i))
              (let ((pos (or splice-pos fallback i)))
                (setq result (append (cl-subseq result 0 pos)
                                     (list frow)
                                     (cl-subseq result pos))))))
          ;; Replace data rows
          (delete-region data-start data-end)
          (goto-char data-start)
          (dolist (r result)
            (insert (nth 2 r) "\n"))
          (goto-char (org-table-begin))
          (org-table-align)
          (message "Sorted %d row(s) by date." (length row-lines)))))))

(defun org-milestone-table-dwim ()
  "If point is in a milestone table, update it and return t.
Runs `org-milestone-table-add-missing-ids',
`org-milestone-table-update-timeline', and
`org-milestone-table-sort-by-date' in sequence.
Intended for use on `org-ctrl-c-ctrl-c-hook'."
  (when (and (org-at-table-p)
             (save-excursion
               (goto-char (org-table-begin))
               (condition-case nil
                   (progn (omt--parse-header) t)
                 (user-error nil))))
    (org-milestone-table-add-missing-ids)
    (org-milestone-table-update-timeline)
    (org-milestone-table-sort-by-date)
    t))

;;;###autoload
(with-eval-after-load 'org
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-milestone-table-dwim))

(provide 'org-milestone-table)
;;; org-milestone-table.el ends here
