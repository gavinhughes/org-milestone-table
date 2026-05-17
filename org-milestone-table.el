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

(defcustom org-milestone-table-highlight-critical-path nil
  "When non-nil, highlight critical-path rows after each timeline update.
The critical path can always be toggled manually with
`org-milestone-table-toggle-critical-path'."
  :type 'boolean
  :group 'org-milestone-table)

(defface org-milestone-table-critical-path
  '((((class color) (background dark))
     :background "#5c3300" :foreground "#ffcc88")
    (((class color) (background light))
     :background "#fff3cd" :foreground "#7a4100")
    (t :inherit highlight))
  "Face used to highlight critical-path rows in milestone tables.
Customize the background and foreground colors to suit your theme."
  :group 'org-milestone-table)

(defcustom org-milestone-table-date-input-style 'mdy
  "Date input style for shorthand dates in the Date column.
`mdy' (the default) parses N/N and N/N/N as month/day and
month/day/year.  `dmy' parses them as day/month and day/month/year.
Two-digit years are mapped to 2000+YY.  The canonical stored form
is always ISO 8601 (YYYY-MM-DD)."
  :type '(choice (const :tag "Month/Day/Year (US)" mdy)
                 (const :tag "Day/Month/Year (EU)" dmy))
  :group 'org-milestone-table)

(defcustom org-milestone-table-done-statuses '("done" "Done" "DONE")
  "Status values that mark a milestone as done.
Rows whose Status column matches any string in this list (compared
case-insensitively) have their Milestone cell rendered with
`org-milestone-table-done' and are hidden by
`org-milestone-table-toggle-hide-done'."
  :type '(repeat string)
  :group 'org-milestone-table)

(defface org-milestone-table-done
  '((t :strike-through t))
  "Face used on the Milestone cell of rows whose Status is a done value."
  :group 'org-milestone-table)

(defvar-local omt--table-states nil
  "Per-table state.
Each entry is (TABLE-BEG-MARKER CRITICAL-OVS CRITICAL-IDS DONE-OVS HIDE-DONE-P).")

(defun omt--table-state (table-beg)
  "Return or create the state list for TABLE-BEG in `omt--table-states'."
  (or (cl-find-if (lambda (e) (= (marker-position (car e)) table-beg))
                  omt--table-states)
      (let ((entry (list (copy-marker table-beg) nil nil nil nil)))
        (push entry omt--table-states)
        entry)))

(defsubst omt--state-done-ovs (entry)
  "Return the done-row overlays slot of state ENTRY."
  (nth 3 entry))

(defsubst omt--state-hide-done-p (entry)
  "Return the hide-done flag slot of state ENTRY."
  (nth 4 entry))

(defun omt--current-table-overlays ()
  "Return critical-path overlays for the table at point."
  (cadr (omt--table-state (org-table-begin))))

;;;###autoload
(defun org-milestone-table-new ()
  "Insert an empty milestone table at point."
  (interactive)
  (insert "| ID | Pred | Date | Milestone |
|----+------+------+-----------|
|    |      |      |           |
")
  (forward-line -1)
  (org-table-goto-column 1)
  (when (bound-and-true-p evil-mode)
    (evil-insert-state)))

(defun omt--parse-pred-list (pred)
  "Split PRED on commas, trim each element, return list of strings."
  (mapcar #'string-trim (split-string pred "," t "[ \t]+")))

(defun omt--parse-header ()
  "Parse header row at point.
Return (ncols col-id col-pred col-date col-status col-milestone).
COL-STATUS and COL-MILESTONE are nil when the respective column is absent."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (cells (omt--row-cells line))
         (ncols (length cells))
         (col-id nil)
         (col-pred nil)
         (col-date nil)
         (col-status nil)
         (col-milestone nil))
    (cl-loop for cell in cells
             for i from 0
             do (let ((name (downcase (string-trim cell))))
                  (cond
                   ((string= name "id") (setq col-id i))
                   ((or (string= name "pred")
                        (string= name "predecessor")
                        (string= name "predecessors")) (setq col-pred i))
                   ((string= name "date") (setq col-date i))
                   ((string= name "status") (setq col-status i))
                   ((string= name "milestone") (setq col-milestone i)))))
    (unless col-id (user-error "No ID column"))
    (unless col-pred (user-error "No Pred column"))
    (unless col-date (user-error "No Date column"))
    (list ncols col-id col-pred col-date col-status col-milestone)))

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

(defun omt--expand-shorthand-date (s)
  "Return canonical YYYY-MM-DD if S is shorthand, else S unchanged.
Returns nil if S looks like shorthand but is out of range.
Style is controlled by `org-milestone-table-date-input-style'.
Bare M/D (no year) infers the year forward-looking from today."
  (if (or (not (stringp s))
          (not (string-match
                "\\`\\([0-9]+\\)/\\([0-9]+\\)\\(?:/\\([0-9]+\\)\\)?\\'"
                s)))
      s
    (let* ((a (string-to-number (match-string 1 s)))
           (b (string-to-number (match-string 2 s)))
           (c-str (match-string 3 s))
           (style org-milestone-table-date-input-style)
           (mm (if (eq style 'dmy) b a))
           (dd (if (eq style 'dmy) a b))
           (yy (cond
                ((null c-str)
                 (let* ((today (decode-time))
                        (cy (nth 5 today))
                        (cm (nth 4 today))
                        (cd (nth 3 today)))
                   (if (or (< cm mm)
                           (and (= cm mm) (<= cd dd)))
                       cy
                     (1+ cy))))
                ((<= (length c-str) 2)
                 (+ 2000 (string-to-number c-str)))
                (t (string-to-number c-str)))))
      (and (>= mm 1) (<= mm 12)
           (>= dd 1)
           (<= dd (calendar-last-day-of-month mm yy))
           (format "%04d-%02d-%02d" yy mm dd)))))

(defun omt--parse-date (s)
  "Parse YYYY-MM-DD into (month day year) or nil."
  (when (and (stringp s)
             (string-match
              "\\`\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\'"
              s))
    (list (string-to-number (match-string 2 s))
          (string-to-number (match-string 3 s))
          (string-to-number (match-string 1 s)))))

(defun omt--parse-date-cell (s)
  "Parse Date cell S into a plist.
Returns (:fixed BOOL :date STR-OR-NIL :raw S :canonicalized BOOL).
:fixed is non-nil when S contains the marker `F' anywhere.
:date is the trimmed, shorthand-expanded date (canonical YYYY-MM-DD
when shorthand was used) or nil if empty.
:canonicalized is non-nil when shorthand expansion changed the value
and the cell should be rewritten in place.
Date validity is not checked here; pass :date to `omt--parse-date'."
  (let* ((raw (or s ""))
         (fixed (and (stringp raw) (string-match-p "F" raw)))
         (stripped (if fixed
                       (string-trim (replace-regexp-in-string "F" "" raw))
                     (string-trim raw)))
         (expanded (and (not (string-empty-p stripped))
                        (omt--expand-shorthand-date stripped))))
    (list :fixed (and fixed t)
          :date (cond
                 ((string-empty-p stripped) nil)
                 ((null expanded) stripped)
                 (t expanded))
          :raw raw
          :canonicalized (and expanded
                              (not (string= expanded stripped))))))

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
          (date (plist-get row :date))
          (fixed (plist-get row :fixed)))
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
                        (when rabs
                          (push (omt--apply-offset rabs op n unit)
                                dates))))))))
            (cond
             ((or failed (null dates)) nil)
             ;; Fixed date with predecessors: must be >= predecessor-derived.
             (fixed
              (let ((pred-abs (apply #'max dates))
                    (pd (omt--parse-date date)))
                (cond
                 ((null pd)
                  (push (format "Bad date: %s" (or date ""))
                        (symbol-value errs-sym))
                  nil)
                 (t
                  (let ((fixed-abs (calendar-absolute-from-gregorian pd)))
                    (if (< fixed-abs pred-abs)
                        (progn
                          (push (format
                                 "Fixed date %s for ID %s conflicts with predecessors (earliest %s)"
                                 date (or id "?")
                                 (omt--format-date
                                  (calendar-gregorian-from-absolute pred-abs)))
                                (symbol-value errs-sym))
                          nil)
                      fixed-abs))))))
             (t (apply #'max dates))))))))))

(defun omt--collect-pred-ids (pred)
  "Return list of milestone ID strings referenced in PRED."
  (let ((ids nil))
    (dolist (p (omt--parse-pred-list pred))
      (let ((pn (omt--normalize-pred p)))
        (when (string-match "\\`\\([0-9]+\\)[+-]" pn)
          (push (match-string 1 pn) ids))))
    ids))

(defun omt--compute-critical-path (id-to-row id-to-abs)
  "Return a hash set of IDs on the critical path.
ID-TO-ROW maps string ID -> row plist.
ID-TO-ABS maps string ID -> absolute date integer."
  (let ((referenced (make-hash-table :test 'equal)))
    (maphash (lambda (_id row)
               (when (plist-get row :pred)
                 (dolist (rid (omt--collect-pred-ids (plist-get row :pred)))
                   (puthash rid t referenced))))
             id-to-row)
    (let ((max-abs nil)
          (leaves nil))
      (maphash (lambda (id abs)
                 (unless (or (omt--fuzzy-id-p id)
                             (gethash id referenced))
                   (push (cons id abs) leaves)
                   (when (or (null max-abs) (> abs max-abs))
                     (setq max-abs abs))))
               id-to-abs)
      (if (null max-abs)
          (make-hash-table :test 'equal)
        (let ((critical (make-hash-table :test 'equal))
              (queue nil))
          (dolist (pair leaves)
            (when (= (cdr pair) max-abs)
              (puthash (car pair) t critical)
              (push (car pair) queue)))
          (while queue
            (let* ((cur-id (pop queue))
                   (cur-row (gethash cur-id id-to-row))
                   (cur-abs (gethash cur-id id-to-abs))
                   (pred-str (and cur-row (plist-get cur-row :pred))))
              (when pred-str
                (dolist (p (omt--parse-pred-list pred-str))
                  (let ((pn (omt--normalize-pred p)))
                    (when (string-match
                           "\\`\\([0-9]+\\)\\([+-]\\)\\([0-9]+\\)\\([dwm]\\)\\'"
                           pn)
                      (let* ((rid  (match-string 1 pn))
                             (op   (match-string 2 pn))
                             (n    (string-to-number (match-string 3 pn)))
                             (unit (match-string 4 pn))
                             (rabs (gethash rid id-to-abs)))
                        (when (and rabs
                                   (= (omt--apply-offset rabs op n unit)
                                      cur-abs))
                          (unless (gethash rid critical)
                            (puthash rid t critical)
                            (push rid queue))))))))))
          critical)))))

(defun omt--apply-critical-overlays (critical id-to-row)
  "Highlight critical-path rows with overlays.
CRITICAL is a hash set of IDs.  ID-TO-ROW maps ID -> row plist."
  (let ((entry (omt--table-state (org-table-begin))))
    (mapc #'delete-overlay (cadr entry))
    (setcar (cdr entry) nil)
    (maphash (lambda (id _)
               (let* ((row (gethash id id-to-row))
                      (lb  (and row (plist-get row :line-beg))))
                 (when lb
                   (let* ((le (save-excursion
                                (goto-char lb)
                                (line-end-position)))
                          (ov (make-overlay lb le)))
                     (overlay-put ov 'face 'org-milestone-table-critical-path)
                     (overlay-put ov 'org-milestone-table-critical t)
                     (push ov (cadr entry))))))
             critical)))

(defun omt--status-done-p (status)
  "Return non-nil if STATUS matches `org-milestone-table-done-statuses'.
Comparison is case-insensitive."
  (and status
       (let ((s (downcase (string-trim status))))
         (cl-some (lambda (d) (string= s (downcase d)))
                  org-milestone-table-done-statuses))))

(defun omt--cell-bounds (line-beg col-index)
  "Return (CELL-BEG . CELL-END) for the trimmed contents of cell COL-INDEX.
LINE-BEG is the position of the row's first character.  COL-INDEX is the
0-based column index used by `omt--row-cells'.  Returns nil if the cell
can't be located or is empty after trimming."
  (save-excursion
    (goto-char line-beg)
    (let ((le (line-end-position)))
      (when (search-forward "|" le t (1+ col-index))
        (let ((open (point))
              (close-pipe (save-excursion (search-forward "|" le t))))
          (when close-pipe
            (let ((close (1- close-pipe)))
              (goto-char open)
              (skip-chars-forward " \t" close)
              (let ((beg (point)))
                (goto-char close)
                (skip-chars-backward " \t" beg)
                (when (< beg (point))
                  (cons beg (point)))))))))))

(defun omt--apply-done-overlays (id-to-row col-status col-milestone)
  "Add overlays to each row in ID-TO-ROW with a done status.
COL-STATUS is the Status column index, or nil if no Status column.
COL-MILESTONE is the Milestone column index, or nil if absent.

When COL-MILESTONE is non-nil, a strikethrough overlay is applied to the
trimmed Milestone cell of each done row.  When the table state's
hide-done flag is set, a separate full-line invisibility overlay is
applied to each done row regardless of COL-MILESTONE.  Replaces any
existing done overlays for the current table."
  (let* ((entry (omt--table-state (org-table-begin)))
         (hide-p (omt--state-hide-done-p entry)))
    (mapc #'delete-overlay (omt--state-done-ovs entry))
    (setf (nth 3 entry) nil)
    (when col-status
      (maphash
       (lambda (_id row)
         (let ((status (plist-get row :status))
               (lb     (plist-get row :line-beg)))
           (when (and lb (omt--status-done-p status))
             (let ((le (save-excursion
                         (goto-char lb)
                         (line-end-position))))
               (when col-milestone
                 (let ((bounds (omt--cell-bounds lb col-milestone)))
                   (when bounds
                     (let ((ov (make-overlay (car bounds) (cdr bounds))))
                       (overlay-put ov 'face 'org-milestone-table-done)
                       (overlay-put ov 'org-milestone-table-done t)
                       (push ov (nth 3 entry))))))
               (when hide-p
                 (let ((ov (make-overlay lb le)))
                   (overlay-put ov 'invisible t)
                   (overlay-put ov 'org-milestone-table-done t)
                   (push ov (nth 3 entry))))))))
       id-to-row))))

(defun omt--reparse-id-positions ()
  "Return a fresh hash table mapping ID -> plist with :id, :line-beg, :status.
The :status key is included only when a Status column is present.
Used to reapply overlays after the table rows have been reordered."
  (save-excursion
    (goto-char (org-table-begin))
    (let* ((hdr (omt--parse-header))
           (ncols (nth 0 hdr))
           (col-id (nth 1 hdr))
           (col-status (nth 4 hdr))
           (result (make-hash-table :test 'equal)))
      (forward-line 1)
      (while (and (looking-at "^[ \t]*|") (looking-at "^[ \t]*|[-+]"))
        (forward-line 1))
      (while (and (looking-at "^[ \t]*|") (not (looking-at "^[ \t]*|[-+]")))
        (let* ((lb (point))
               (ln (buffer-substring-no-properties lb (line-end-position)))
               (cs (omt--row-cells ln)))
          (when (>= (length cs) ncols)
            (let ((id (string-trim (nth col-id cs)))
                  (status (when col-status
                            (string-trim (nth col-status cs)))))
              (unless (string-empty-p id)
                (puthash id
                         (if col-status
                             (list :id id :line-beg lb :status status)
                           (list :id id :line-beg lb))
                         result)))))
        (forward-line 1))
      result)))

(defun omt--refresh-critical-overlays ()
  "Reapply critical-path overlays using the current buffer positions.
Uses the critical-ids stored by the last `org-milestone-table-update-timeline'."
  (let ((critical-ids (caddr (omt--table-state (org-table-begin)))))
    (when critical-ids
      (omt--apply-critical-overlays critical-ids (omt--reparse-id-positions)))))

(defun omt--refresh-done-overlays ()
  "Reapply done-row overlays using the current buffer positions."
  (save-excursion
    (goto-char (org-table-begin))
    (let* ((hdr (omt--parse-header))
           (col-status (nth 4 hdr))
           (col-milestone (nth 5 hdr)))
      (when col-status
        (omt--apply-done-overlays (omt--reparse-id-positions)
                                  col-status col-milestone)))))

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
           (col-status (nth 4 hdr))
           (col-milestone (nth 5 hdr))
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
                   (dc (omt--parse-date-cell dts))
                   (sts (when col-status
                          (string-trim (nth col-status cs))))
                   (r (list :line-beg lb
                            :id (unless (string-empty-p ids) ids)
                            :pred (unless (string-empty-p pds) pds)
                            :date (plist-get dc :date)
                            :fixed (plist-get dc :fixed)
                            :canonicalized (plist-get dc :canonicalized)
                            :status sts)))
              (push r rows)
              (when (plist-get r :id)
                (if (gethash (plist-get r :id) id-to-row)
                    (push (format "Duplicate ID: %s" (plist-get r :id))
                          (symbol-value errs-sym))
                  (puthash (plist-get r :id) r id-to-row))))))
        (forward-line 1))
      (setq rows (nreverse rows))
      ;; Resolve and collect updates
      (let ((updates nil)
            (id-to-abs (make-hash-table :test 'equal)))
        (dolist (r rows)
          (let ((a (omt--resolve r id-to-row errs-sym nil)))
            (when a
              (when (and (plist-get r :id)
                         (not (omt--fuzzy-id-p (plist-get r :id))))
                (puthash (plist-get r :id) a id-to-abs))
              (when (and (plist-get r :pred)
                         (not (plist-get r :fixed)))
                (push (cons (plist-get r :line-beg)
                            (omt--format-date
                             (calendar-gregorian-from-absolute a)))
                      updates))
              (when (and (plist-get r :canonicalized)
                         (or (plist-get r :fixed)
                             (null (plist-get r :pred))))
                (push (cons (plist-get r :line-beg)
                            (if (plist-get r :fixed)
                                (concat "F " (plist-get r :date))
                              (plist-get r :date)))
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
          (let ((cp (omt--compute-critical-path id-to-row id-to-abs)))
            (setcar (cddr (omt--table-state (org-table-begin))) cp)
            (when org-milestone-table-highlight-critical-path
              (omt--apply-critical-overlays cp id-to-row)))
          (omt--apply-done-overlays id-to-row col-status col-milestone)
          (message "Updated %d date(s)." (length updates)))))))

;;;###autoload
(defun org-milestone-table-add-missing-ids ()
  "Add sequential IDs to rows in the milestone table that lack one.
Finds the maximum existing numeric ID and assigns incrementing IDs
starting from max+1."
  (interactive)
  (let ((orig-pt (point)))
    (with-undo-amalgamate
      (when (listp buffer-undo-list)
        (push orig-pt buffer-undo-list))
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
        (message "Added %d ID(s), starting from %d." count (1+ max-id))))))))

(defun omt--topo-sort-undated (rows)
  "Sort undated ROWS so predecessors appear before their dependents.
ROWS is a list of (abs-date id pred-str line-text) quadruples, all undated.
Rows without IDs are appended after the sorted block unchanged.
Cycles are tolerated: affected rows fall to the end."
  (let ((id-set     (make-hash-table :test 'equal))
        (in-degree  (make-hash-table :test 'equal))
        (dependents (make-hash-table :test 'equal))
        (id-rows nil)
        (no-id-rows nil))
    (dolist (r rows)
      (if (nth 1 r) (push r id-rows) (push r no-id-rows)))
    (setq id-rows    (nreverse id-rows))
    (setq no-id-rows (nreverse no-id-rows))
    (dolist (r id-rows)
      (puthash (nth 1 r) r id-set)
      (puthash (nth 1 r) 0 in-degree))
    (dolist (r id-rows)
      (when (nth 2 r)
        (dolist (pid (omt--collect-pred-ids (nth 2 r)))
          (when (gethash pid id-set)
            (puthash (nth 1 r) (1+ (gethash (nth 1 r) in-degree)) in-degree)
            (puthash pid (cons (nth 1 r) (gethash pid dependents)) dependents)))))
    (let ((queue nil)
          (result nil)
          (processed (make-hash-table :test 'equal)))
      (dolist (r id-rows)
        (when (= (gethash (nth 1 r) in-degree) 0)
          (push (nth 1 r) queue)))
      (while queue
        (let* ((cur-id (pop queue))
               (cur-row (gethash cur-id id-set)))
          (push cur-row result)
          (puthash cur-id t processed)
          (dolist (dep-id (gethash cur-id dependents))
            (let ((d (1- (gethash dep-id in-degree))))
              (puthash dep-id d in-degree)
              (when (= d 0)
                (push dep-id queue))))))
      ;; Append any remaining rows (cycles)
      (dolist (r id-rows)
        (unless (gethash (nth 1 r) processed)
          (push r result)))
      (append (nreverse result) no-id-rows))))

;;;###autoload
(defun org-milestone-table-sort-by-date ()
  "Sort data rows of the milestone table at point by the Date column.
Undated rows that are listed as predecessors by a dated row are placed
just before the earliest such dated row.  Remaining undated rows are
appended at the end, ordered by their predecessor relationships.  Rows
whose ID matches the fuzzy pattern (digits followed by `?') are placed
immediately before the row whose ID is the base number; if that base row
is absent, they are placed before the trailing block of undated rows."
  (interactive)
  (save-excursion
    (unless (org-at-table-p)
      (user-error "Not in a table"))
    (goto-char (org-table-begin))
    (let* ((hdr (omt--parse-header))
           (ncols (nth 0 hdr))
           (col-id   (nth 1 hdr))
           (col-pred (nth 2 hdr))
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
      ;; Collect quadruples: (abs-date id pred-str line-text)
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
               (pds (when (>= (length cs) ncols)
                      (let ((s (string-trim (nth col-pred cs))))
                        (unless (string-empty-p s) s))))
               (abs-date (when (and dts (not (string-empty-p dts)))
                           (let ((pd (omt--parse-date dts)))
                             (when pd
                               (calendar-absolute-from-gregorian pd))))))
          (push (list abs-date ids pds ln) row-lines))
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
        ;; Build predecessor map for same-date tiebreaking.
        ;; Maps id -> list of direct predecessor ids.
        (let ((pred-map (make-hash-table :test 'equal)))
          (dolist (r normal-rows)
            (when (and (nth 1 r) (nth 2 r))
              (puthash (nth 1 r) (omt--collect-pred-ids (nth 2 r)) pred-map)))
          ;; Sort normal rows: dated ascending, undated last.
          ;; Tiebreak equal dates by predecessor relationship (ancestor first).
          (cl-labels ((ancestor-p (anc-id desc-id visited)
                        (unless (gethash desc-id visited)
                          (puthash desc-id t visited)
                          (let ((preds (gethash desc-id pred-map)))
                            (or (member anc-id preds)
                                (cl-some (lambda (p)
                                           (ancestor-p anc-id p visited))
                                         preds))))))
            (setq normal-rows
                  (sort normal-rows
                        (lambda (a b)
                          (cond
                           ((and (nth 0 a) (nth 0 b))
                            (if (= (nth 0 a) (nth 0 b))
                                (and (nth 1 a) (nth 1 b)
                                     (ancestor-p (nth 1 a) (nth 1 b)
                                                 (make-hash-table :test 'equal)))
                              (< (nth 0 a) (nth 0 b))))
                           ((nth 0 a) t)
                           (t nil)))))))
        ;; Order undated rows: anchor those referenced by dated rows just before
        ;; their earliest dated dependent; topo-sort the rest at the end.
        (let* ((dated-rows     (cl-remove-if-not #'car normal-rows))
               (undated-rows   (cl-remove-if     #'car normal-rows))
               (undated-id-set (make-hash-table :test 'equal))
               (anchor-map     (make-hash-table :test 'equal)))
          (dolist (r undated-rows)
            (when (nth 1 r) (puthash (nth 1 r) t undated-id-set)))
          (dolist (r dated-rows)
            (when (nth 2 r)
              (dolist (pid (omt--collect-pred-ids (nth 2 r)))
                (when (gethash pid undated-id-set)
                  (let ((cur (gethash pid anchor-map)))
                    (when (or (null cur) (< (nth 0 r) cur))
                      (puthash pid (nth 0 r) anchor-map)))))))
          (let ((anchor-groups (make-hash-table :test 'equal))
                (free-rows nil))
            (dolist (r undated-rows)
              (let ((anchor (and (nth 1 r) (gethash (nth 1 r) anchor-map))))
                (if anchor
                    (puthash anchor
                             (cons r (gethash anchor anchor-groups))
                             anchor-groups)
                  (push r free-rows))))
            (setq free-rows (nreverse free-rows))
            (let ((seen (make-hash-table :test 'equal))
                  (result nil))
              (dolist (r dated-rows)
                (unless (gethash (nth 0 r) seen)
                  (let ((group (gethash (nth 0 r) anchor-groups)))
                    (when group
                      (dolist (gr (omt--topo-sort-undated (nreverse group)))
                        (push gr result))))
                  (puthash (nth 0 r) t seen))
                (push r result))
              (setq normal-rows
                    (append (nreverse result)
                            (omt--topo-sort-undated free-rows))))))
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
            (insert (nth 3 r) "\n"))
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
    (let* ((col (org-table-current-column))
           (row-offset (- (line-number-at-pos)
                          (line-number-at-pos (org-table-begin)))))
      (org-milestone-table-add-missing-ids)
      (org-milestone-table-update-timeline)
      (org-milestone-table-sort-by-date)
      (omt--refresh-critical-overlays)
      (omt--refresh-done-overlays)
      (goto-char (org-table-begin))
      (forward-line row-offset)
      (when (org-at-table-p)
        (org-table-goto-column col)))
    t))

;;;###autoload
(defun org-milestone-table-toggle-critical-path ()
  "Toggle critical-path row highlighting in the milestone table at point.
Always runs `org-milestone-table-update-timeline' first."
  (interactive)
  (let* ((entry (omt--table-state (org-table-begin)))
         (was-showing (cadr entry)))
    (org-milestone-table-update-timeline)
    (setq entry (omt--table-state (org-table-begin)))
    (if was-showing
        (progn
          (mapc #'delete-overlay (cadr entry))
          (setcar (cdr entry) nil)
          (message "Critical path highlighting off."))
      (unless (cadr entry)
        (omt--refresh-critical-overlays))
      (message "Critical path highlighting on."))))

;;;###autoload
(defun org-milestone-table-toggle-hide-done ()
  "Toggle visibility of rows whose Status is a done value.
The strikethrough on done rows remains visible either way; this command
only flips whether those rows are folded out of view."
  (interactive)
  (unless (org-at-table-p) (user-error "Not in a table"))
  (save-excursion
    (goto-char (org-table-begin))
    (let* ((hdr (omt--parse-header))
           (col-status (nth 4 hdr)))
      (unless col-status (user-error "No Status column"))
      (let* ((entry (omt--table-state (org-table-begin)))
             (new (not (omt--state-hide-done-p entry))))
        (setf (nth 4 entry) new)
        (omt--refresh-done-overlays)
        (message "Hide done rows: %s" (if new "on" "off"))))))

;;;###autoload
(with-eval-after-load 'org
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-milestone-table-dwim))

(provide 'org-milestone-table)
;;; org-milestone-table.el ends here
