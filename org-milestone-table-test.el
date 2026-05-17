;;; org-milestone-table-test.el --- Tests for org-milestone-table -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for org-milestone-table.

;;; Code:

(require 'ert)
(require 'org-milestone-table)

;;; --- Helper ---

(defmacro omt-test-with-table (table-string &rest body)
  "Insert TABLE-STRING into a temp Org buffer and execute BODY.
Point is placed at the beginning of the table."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,table-string)
     (goto-char (point-min))
     (when (re-search-forward "^[ \t]*|" nil t)
       (beginning-of-line))
     ,@body))

;;; --- omt--row-cells ---

(ert-deftest omt-test-row-cells-basic ()
  "Parse a simple table row into cells."
  (let ((cells (omt--row-cells "| a | b | c |")))
    (should (equal cells '("a" "b" "c")))))

(ert-deftest omt-test-row-cells-empty ()
  "Parse a row with empty cells."
  (let ((cells (omt--row-cells "|  |  |  |")))
    (should (equal cells '("" "" "")))))

(ert-deftest omt-test-row-cells-whitespace ()
  "Cells are trimmed of whitespace."
  (let ((cells (omt--row-cells "|  foo  |  bar  |")))
    (should (equal cells '("foo" "bar")))))

;;; --- omt--parse-date ---

(ert-deftest omt-test-parse-date-valid ()
  "Parse a valid YYYY-MM-DD date."
  (should (equal (omt--parse-date "2025-03-15") '(3 15 2025))))

(ert-deftest omt-test-parse-date-nil-on-empty ()
  "Return nil for empty string."
  (should (null (omt--parse-date ""))))

(ert-deftest omt-test-parse-date-nil-on-bad ()
  "Return nil for malformed date."
  (should (null (omt--parse-date "not-a-date"))))

(ert-deftest omt-test-parse-date-nil-on-partial ()
  "Return nil for partial date."
  (should (null (omt--parse-date "2025-03"))))

;;; --- omt--expand-shorthand-date ---

(ert-deftest omt-test-expand-shorthand-mdy-full ()
  "M/D/YYYY expands to canonical ISO with style mdy."
  (let ((org-milestone-table-date-input-style 'mdy))
    (should (equal (omt--expand-shorthand-date "5/17/2026") "2026-05-17"))))

(ert-deftest omt-test-expand-shorthand-mdy-two-digit-year ()
  "Two-digit year maps to 2000+YY."
  (let ((org-milestone-table-date-input-style 'mdy))
    (should (equal (omt--expand-shorthand-date "5/17/26") "2026-05-17"))))

(ert-deftest omt-test-expand-shorthand-mdy-zero-pad ()
  "Single-digit month and day are zero-padded in the canonical form."
  (let ((org-milestone-table-date-input-style 'mdy))
    (should (equal (omt--expand-shorthand-date "1/5/2026") "2026-01-05"))))

(ert-deftest omt-test-expand-shorthand-mdy-bare-future ()
  "Bare M/D uses current year when the date is today or later."
  (let ((org-milestone-table-date-input-style 'mdy))
    (cl-letf (((symbol-function 'decode-time)
               (lambda (&rest _) '(0 0 0 1 6 2026 0 nil 0))))
      (should (equal (omt--expand-shorthand-date "8/15") "2026-08-15")))))

(ert-deftest omt-test-expand-shorthand-mdy-bare-past ()
  "Bare M/D uses next year when the date has already passed."
  (let ((org-milestone-table-date-input-style 'mdy))
    (cl-letf (((symbol-function 'decode-time)
               (lambda (&rest _) '(0 0 0 1 10 2026 0 nil 0))))
      (should (equal (omt--expand-shorthand-date "3/1") "2027-03-01")))))

(ert-deftest omt-test-expand-shorthand-mdy-bare-today ()
  "Bare M/D matching today still uses current year (>= today)."
  (let ((org-milestone-table-date-input-style 'mdy))
    (cl-letf (((symbol-function 'decode-time)
               (lambda (&rest _) '(0 0 0 17 5 2026 0 nil 0))))
      (should (equal (omt--expand-shorthand-date "5/17") "2026-05-17")))))

(ert-deftest omt-test-expand-shorthand-out-of-range-day ()
  "Out-of-range day returns nil."
  (let ((org-milestone-table-date-input-style 'mdy))
    (should (null (omt--expand-shorthand-date "2/30/2025")))))

(ert-deftest omt-test-expand-shorthand-out-of-range-month ()
  "Out-of-range month returns nil."
  (let ((org-milestone-table-date-input-style 'mdy))
    (should (null (omt--expand-shorthand-date "13/1/2025")))))

(ert-deftest omt-test-expand-shorthand-dmy-full ()
  "D/M/YYYY expands correctly with style dmy."
  (let ((org-milestone-table-date-input-style 'dmy))
    (should (equal (omt--expand-shorthand-date "17/5/2026") "2026-05-17"))))

(ert-deftest omt-test-expand-shorthand-dmy-rejects-mdy ()
  "With dmy, 5/17 means day=5 month=17 which is out of range."
  (let ((org-milestone-table-date-input-style 'dmy))
    (should (null (omt--expand-shorthand-date "5/17/2026")))))

(ert-deftest omt-test-expand-shorthand-passthrough-iso ()
  "Already-ISO dates pass through unchanged."
  (should (equal (omt--expand-shorthand-date "2026-05-17") "2026-05-17")))

(ert-deftest omt-test-expand-shorthand-passthrough-nonmatch ()
  "Non-matching strings pass through unchanged."
  (should (equal (omt--expand-shorthand-date "not a date") "not a date")))

;;; --- omt--format-date ---

(ert-deftest omt-test-format-date ()
  "Format gregorian triple as YYYY-MM-DD."
  (should (equal (omt--format-date '(3 15 2025)) "2025-03-15")))

(ert-deftest omt-test-format-date-padding ()
  "Single-digit month/day are zero-padded."
  (should (equal (omt--format-date '(1 5 2025)) "2025-01-05")))

;;; --- omt--normalize-pred ---

(ert-deftest omt-test-normalize-pred-bare-number ()
  "Bare number N becomes N+0d."
  (should (equal (omt--normalize-pred "3") "3+0d")))

(ert-deftest omt-test-normalize-pred-with-offset ()
  "Already-qualified pred is unchanged."
  (should (equal (omt--normalize-pred "3+5d") "3+5d")))

(ert-deftest omt-test-normalize-pred-minus ()
  "Negative offset is unchanged."
  (should (equal (omt--normalize-pred "3-2w") "3-2w")))

(ert-deftest omt-test-normalize-pred-month ()
  "Month offset is unchanged."
  (should (equal (omt--normalize-pred "1+3m") "1+3m")))

;;; --- omt--parse-pred-list ---

(ert-deftest omt-test-parse-pred-list-single ()
  "Single predecessor."
  (should (equal (omt--parse-pred-list "3+5d") '("3+5d"))))

(ert-deftest omt-test-parse-pred-list-multiple ()
  "Multiple predecessors separated by commas."
  (should (equal (omt--parse-pred-list "1+5d, 2+3d") '("1+5d" "2+3d"))))

(ert-deftest omt-test-parse-pred-list-whitespace ()
  "Whitespace around commas is trimmed."
  (should (equal (omt--parse-pred-list "1+5d , 2+3d , 3") '("1+5d" "2+3d" "3"))))

;;; --- omt--apply-offset ---

(ert-deftest omt-test-apply-offset-days-plus ()
  "Add days to a date."
  (let* ((base (calendar-absolute-from-gregorian '(3 15 2025)))
         (result (omt--apply-offset base "+" 10 "d"))
         (greg (calendar-gregorian-from-absolute result)))
    (should (equal greg '(3 25 2025)))))

(ert-deftest omt-test-apply-offset-days-minus ()
  "Subtract days from a date."
  (let* ((base (calendar-absolute-from-gregorian '(3 15 2025)))
         (result (omt--apply-offset base "-" 5 "d"))
         (greg (calendar-gregorian-from-absolute result)))
    (should (equal greg '(3 10 2025)))))

(ert-deftest omt-test-apply-offset-weeks ()
  "Add weeks to a date."
  (let* ((base (calendar-absolute-from-gregorian '(3 1 2025)))
         (result (omt--apply-offset base "+" 2 "w"))
         (greg (calendar-gregorian-from-absolute result)))
    (should (equal greg '(3 15 2025)))))

(ert-deftest omt-test-apply-offset-months ()
  "Add months to a date."
  (let* ((base (calendar-absolute-from-gregorian '(1 31 2025)))
         (result (omt--apply-offset base "+" 1 "m"))
         (greg (calendar-gregorian-from-absolute result)))
    ;; Jan 31 + 1m = Feb 28 (clamped)
    (should (equal greg '(2 28 2025)))))

(ert-deftest omt-test-apply-offset-months-minus ()
  "Subtract months from a date."
  (let* ((base (calendar-absolute-from-gregorian '(3 31 2025)))
         (result (omt--apply-offset base "-" 1 "m"))
         (greg (calendar-gregorian-from-absolute result)))
    ;; Mar 31 - 1m = Feb 28 (clamped)
    (should (equal greg '(2 28 2025)))))

;;; --- omt--parse-header ---

(ert-deftest omt-test-parse-header ()
  "Parse header row to find column indices."
  (omt-test-with-table "| ID | Pred | Date | Milestone |\n|----+------+------+-----------|\n"
    (let ((hdr (omt--parse-header)))
      (should (= (nth 0 hdr) 4))   ; ncols
      (should (= (nth 1 hdr) 0))   ; col-id
      (should (= (nth 2 hdr) 1))   ; col-pred
      (should (= (nth 3 hdr) 2))))) ; col-date

(ert-deftest omt-test-parse-header-reordered ()
  "Header columns can be in any order."
  (omt-test-with-table "| Date | Milestone | ID | Pred |\n|------+-----------+----+------|\n"
    (let ((hdr (omt--parse-header)))
      (should (= (nth 1 hdr) 2))   ; col-id
      (should (= (nth 2 hdr) 3))   ; col-pred
      (should (= (nth 3 hdr) 0))))) ; col-date

(ert-deftest omt-test-parse-header-predecessor-alias ()
  "Column header \"Predecessor\" is accepted as the pred column."
  (omt-test-with-table "| ID | Predecessor | Date | Milestone |\n|----+-------------+------+-----------|\n"
    (let ((hdr (omt--parse-header)))
      (should (= (nth 2 hdr) 1)))))   ; col-pred

(ert-deftest omt-test-parse-header-missing-id ()
  "Error when ID column is missing."
  (omt-test-with-table "| Pred | Date | Milestone |\n|------+------+-----------|\n"
    (should-error (omt--parse-header) :type 'user-error)))

;;; --- omt--resolve ---

(ert-deftest omt-test-resolve-no-pred ()
  "Row with no predecessor returns its literal date."
  (let ((tbl (make-hash-table :test 'equal))
        (errors nil)
        (row (list :id "1" :pred nil :date "2025-03-15")))
    (puthash "1" row tbl)
    (let ((result (omt--resolve row tbl 'errors nil)))
      (should (= result (calendar-absolute-from-gregorian '(3 15 2025))))
      (should (null errors)))))

(ert-deftest omt-test-resolve-simple-chain ()
  "Resolve a simple predecessor chain."
  (let ((tbl (make-hash-table :test 'equal))
        (errors nil)
        (r1 (list :id "1" :pred nil :date "2025-01-01"))
        (r2 (list :id "2" :pred "1+10d" :date nil)))
    (puthash "1" r1 tbl)
    (puthash "2" r2 tbl)
    (let* ((result (omt--resolve r2 tbl 'errors nil))
           (greg (calendar-gregorian-from-absolute result)))
      (should (equal greg '(1 11 2025)))
      (should (null errors)))))

(ert-deftest omt-test-resolve-cycle-detection ()
  "Cycle in predecessors produces an error."
  (let ((tbl (make-hash-table :test 'equal))
        (r1 (list :id "1" :pred "2+0d" :date nil))
        (r2 (list :id "2" :pred "1+0d" :date nil)))
    (puthash "1" r1 tbl)
    (puthash "2" r2 tbl)
    (defvar omt--test-errors nil)
    (setq omt--test-errors nil)
    (omt--resolve r1 tbl 'omt--test-errors nil)
    (should (not (null omt--test-errors)))))

(ert-deftest omt-test-resolve-max-of-multiple-preds ()
  "Multiple predecessors: result is the max (latest) date."
  (let ((tbl (make-hash-table :test 'equal))
        (errors nil)
        (r1 (list :id "1" :pred nil :date "2025-01-01"))
        (r2 (list :id "2" :pred nil :date "2025-06-01"))
        (r3 (list :id "3" :pred "1+0d,2+0d" :date nil)))
    (puthash "1" r1 tbl)
    (puthash "2" r2 tbl)
    (puthash "3" r3 tbl)
    (let* ((result (omt--resolve r3 tbl 'errors nil))
           (greg (calendar-gregorian-from-absolute result)))
      (should (equal greg '(6 1 2025)))
      (should (null errors)))))

;;; --- Integration: org-milestone-table-update-timeline ---

(ert-deftest omt-test-update-timeline-basic ()
  "Full integration: update a simple two-row table."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone   |
|----+------+------------+-------------|
| 1  |      | 2025-01-01 | Start       |
| 2  | 1+5d |            | Five days   |
"
    (org-milestone-table-update-timeline)
    (goto-char (point-min))
    (should (search-forward "2025-01-06" nil t))))

(ert-deftest omt-test-update-timeline-chain ()
  "Three-row chain resolves correctly."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone |
|----+------+------------+-----------|
| 1  |      | 2025-01-01 | Start     |
| 2  | 1+7d |            | Week      |
| 3  | 2+7d |            | Two weeks |
"
    (org-milestone-table-update-timeline)
    (goto-char (point-min))
    (should (search-forward "2025-01-08" nil t))
    (should (search-forward "2025-01-15" nil t))))

;;; --- org-milestone-table-add-missing-ids ---

(ert-deftest omt-test-add-missing-ids ()
  "Rows without IDs get sequential IDs."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone |
|----+------+------------+-----------|
| 1  |      | 2025-01-01 | Start     |
|    |      |            | No ID     |
|    |      |            | Also none |
"
    (org-milestone-table-add-missing-ids)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      (should (string-match-p "|\\s-+2\\s-+|" content))
      (should (string-match-p "|\\s-+3\\s-+|" content)))))

;;; --- org-milestone-table-sort-by-date ---

(ert-deftest omt-test-sort-by-date ()
  "Rows are sorted by date ascending."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone |
|----+------+------------+-----------|
| 2  |      | 2025-06-01 | Later     |
| 1  |      | 2025-01-01 | Earlier   |
"
    (org-milestone-table-sort-by-date)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      (should (< (string-match "Earlier" content)
                 (string-match "Later" content))))))

(ert-deftest omt-test-sort-same-date-predecessor-first ()
  "When two rows share a date, the predecessor sorts before its dependent."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone  |
|----+------+------------+------------|
| 2  | 1+0  | 2025-06-01 | Dependent  |
| 1  |      | 2025-06-01 | Predecessor |
"
    (org-milestone-table-sort-by-date)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      (should (< (string-match "Predecessor" content)
                 (string-match "Dependent" content))))))

(ert-deftest omt-test-sort-by-date-no-date-last ()
  "Rows without dates sort to the end."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone |
|----+------+------------+-----------|
|    |      |            | No date   |
| 1  |      | 2025-01-01 | Has date  |
"
    (org-milestone-table-sort-by-date)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      (should (< (string-match "Has date" content)
                 (string-match "No date" content))))))

;;; --- omt--fuzzy-id-p ---

(ert-deftest omt-test-fuzzy-id-p-true ()
  "\"15?\" is a fuzzy ID."
  (should (omt--fuzzy-id-p "15?")))

(ert-deftest omt-test-fuzzy-id-p-false-plain-number ()
  "\"15\" is not fuzzy."
  (should-not (omt--fuzzy-id-p "15")))

(ert-deftest omt-test-fuzzy-id-p-false-bare-question ()
  "\"?\" alone is not fuzzy (no leading digits)."
  (should-not (omt--fuzzy-id-p "?")))

(ert-deftest omt-test-fuzzy-id-p-false-double-question ()
  "\"15??\" is not fuzzy (only one trailing ? is allowed)."
  (should-not (omt--fuzzy-id-p "15??")))

;;; --- omt--fuzzy-id-base ---

(ert-deftest omt-test-fuzzy-id-base ()
  "Strip the trailing ? from a fuzzy ID."
  (should (equal (omt--fuzzy-id-base "15?") "15")))

;;; --- Integration: fuzzy ID sort placement ---

(ert-deftest omt-test-sort-fuzzy-id-before-base ()
  "A fuzzy row (\"15?\") sorts immediately before the base row (\"15\")."
  (omt-test-with-table
      "| ID  | Pred | Date       | Milestone     |
|-----+------+------------+---------------|
| 16  |      | 2025-03-01 | After fifteen |
| 15? |      |            | Unknown pred  |
| 15  |      | 2025-01-01 | Fifteen       |
"
    (org-milestone-table-sort-by-date)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      ;; 15? must appear before 15
      (should (< (string-match "Unknown pred" content)
                 (string-match "Fifteen" content)))
      ;; 15 must appear before 16
      (should (< (string-match "Fifteen" content)
                 (string-match "After fifteen" content))))))

(ert-deftest omt-test-sort-fuzzy-id-no-base-before-undated ()
  "A fuzzy row with no matching base goes before other undated rows."
  (omt-test-with-table
      "| ID  | Pred | Date       | Milestone    |
|-----+------+------------+--------------|
| 1   |      | 2025-01-01 | Dated        |
| 99? |      |            | Unknown base |
|     |      |            | No id either |
"
    (org-milestone-table-sort-by-date)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      (should (< (string-match "Dated" content)
                 (string-match "Unknown base" content)))
      (should (< (string-match "Unknown base" content)
                 (string-match "No id either" content))))))

;;; --- org-milestone-table-new ---

(ert-deftest omt-test-new-table-insertion ()
  "Inserting an new table produces correct structure."
  (with-temp-buffer
    (org-mode)
    (org-milestone-table-new)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      (should (string-match-p "| ID | Pred | Date | Milestone |" content))
      (should (string-match-p "|----" content)))))

(ert-deftest omt-test-update-duplicate-id-error ()
  "Duplicate IDs surface in *Milestone Table Errors* buffer."
  (omt-test-with-table
      "| ID | Pred | Date       |\n|----+------+------------|\n|  1 |      | 2025-01-01 |\n|  1 |      | 2025-02-01 |\n"
    (org-milestone-table-update-timeline)
    (let ((buf (get-buffer "*Milestone Table Errors*")))
      (should buf)
      (with-current-buffer buf
        (should (string-match-p "Duplicate ID: 1" (buffer-string)))))))

(ert-deftest omt-test-update-errors-shown-in-buffer ()
  "Validation errors appear in *Milestone Table Errors* buffer."
  (omt-test-with-table
      "| ID | Pred  | Date       |\n|----+-------+------------|\n|  1 |       | 2025-01-01 |\n|  2 | 99+1d |            |\n"
    (org-milestone-table-update-timeline)
    (let ((buf (get-buffer "*Milestone Table Errors*")))
      (should buf)
      (with-current-buffer buf
        (should (string-match-p "Unknown ID" (buffer-string)))))))

(ert-deftest omt-test-fuzzy-id-no-cycle-error ()
  "Fuzzy milestone rows do not trigger spurious cycle detection."
  (omt-test-with-table
      "| ID | Pred | Date       |\n|----+------+------------|\n|  5 |      | 2025-01-01 |\n| 5? |    5 |            |\n"
    (org-milestone-table-update-timeline)
    ;; No error buffer should appear for a valid fuzzy predecessor chain.
    (let ((buf (get-buffer "*Milestone Table Errors*")))
      (when buf
        (with-current-buffer buf
          (should-not (string-match-p "Cycle" (buffer-string))))))
    ;; The fuzzy row's date should be resolved to the base milestone's date.
    (goto-char (point-min))
    (should (search-forward "2025-01-01" nil t 2))))

;;; --- omt--collect-pred-ids ---

(ert-deftest omt-test-collect-pred-ids ()
  "Extract referenced IDs from a predecessor string."
  (let ((ids (omt--collect-pred-ids "1+5d,2+3w")))
    (should (member "1" ids))
    (should (member "2" ids))))

;;; --- omt--compute-critical-path ---

(ert-deftest omt-test-compute-critical-path-linear ()
  "Linear chain 1->2->3: all three IDs are on the critical path."
  (let ((id-to-row (make-hash-table :test 'equal))
        (id-to-abs (make-hash-table :test 'equal))
        (d1 (calendar-absolute-from-gregorian '(1 1 2025)))
        (d2 (calendar-absolute-from-gregorian '(1 6 2025)))
        (d3 (calendar-absolute-from-gregorian '(1 11 2025))))
    (puthash "1" (list :id "1" :pred nil  :date "2025-01-01") id-to-row)
    (puthash "2" (list :id "2" :pred "1+5d" :date nil)        id-to-row)
    (puthash "3" (list :id "3" :pred "2+5d" :date nil)        id-to-row)
    (puthash "1" d1 id-to-abs)
    (puthash "2" d2 id-to-abs)
    (puthash "3" d3 id-to-abs)
    (let ((cp (omt--compute-critical-path id-to-row id-to-abs)))
      (should (gethash "1" cp))
      (should (gethash "2" cp))
      (should (gethash "3" cp)))))

(ert-deftest omt-test-compute-critical-path-branch ()
  "Branching: 1->3 (+10d) beats 1->2->3 (+5d+3d); critical path is {1,3}."
  (let ((id-to-row (make-hash-table :test 'equal))
        (id-to-abs (make-hash-table :test 'equal))
        (d1 (calendar-absolute-from-gregorian '(1 1 2025)))
        (d2 (calendar-absolute-from-gregorian '(1 6 2025)))
        (d3 (calendar-absolute-from-gregorian '(1 11 2025))))
    (puthash "1" (list :id "1" :pred nil          :date "2025-01-01") id-to-row)
    (puthash "2" (list :id "2" :pred "1+5d"        :date nil)         id-to-row)
    (puthash "3" (list :id "3" :pred "1+10d,2+3d"  :date nil)         id-to-row)
    (puthash "1" d1 id-to-abs)
    (puthash "2" d2 id-to-abs)
    (puthash "3" d3 id-to-abs)
    (let ((cp (omt--compute-critical-path id-to-row id-to-abs)))
      (should     (gethash "1" cp))
      (should-not (gethash "2" cp))
      (should     (gethash "3" cp)))))

;;; --- Integration: critical path overlays ---

(ert-deftest omt-test-update-timeline-highlights-critical-path ()
  "After update-timeline with highlight enabled, critical-path rows get overlays."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone   |
|----+------+------------+-------------|
| 1  |      | 2025-01-01 | Start       |
| 2  | 1+5d |            | Five days   |
"
    (let ((org-milestone-table-highlight-critical-path t))
      (org-milestone-table-update-timeline))
    ;; At least one overlay should be present
    (should (omt--current-table-overlays))
    ;; Every overlay should carry the critical-path face
    (dolist (ov (omt--current-table-overlays))
      (should (eq (overlay-get ov 'face) 'org-milestone-table-critical-path)))))

(ert-deftest omt-test-dwim-highlights-critical-path-after-sort ()
  "After C-c C-c (dwim) with highlight enabled, critical-path overlays survive the sort step."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone   |
|----+------+------------+-------------|
| 2  | 1+5d |            | Five days   |
| 1  |      | 2025-01-01 | Start       |
"
    (let ((org-milestone-table-highlight-critical-path t))
      (org-milestone-table-dwim))
    ;; Overlays should be present and on visible (non-zero-width) regions
    (should (omt--current-table-overlays))
    (dolist (ov (omt--current-table-overlays))
      (should (eq (overlay-get ov 'face) 'org-milestone-table-critical-path))
      (should (< (overlay-start ov) (overlay-end ov))))))

(ert-deftest omt-test-update-timeline-no-highlight-when-disabled ()
  "With `org-milestone-table-highlight-critical-path' nil, no overlays are applied."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone   |
|----+------+------------+-------------|
| 1  |      | 2025-01-01 | Start       |
| 2  | 1+5d |            | Five days   |
"
    (let ((org-milestone-table-highlight-critical-path nil))
      (org-milestone-table-update-timeline))
    (should-not (omt--current-table-overlays))))

;;; --- omt--topo-sort-undated / undated ordering ---

(ert-deftest omt-test-sort-undated-predecessor-before-dependent ()
  "Undated predecessor X sorts before its undated dependent Y."
  (omt-test-with-table
      "| ID | Pred | Date | Milestone |
|----+------+------+-----------|
| 2  | 1+5d |      | Y         |
| 1  |      |      | X         |
"
    (org-milestone-table-sort-by-date)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      (should (< (string-match "| X" content)
                 (string-match "| Y" content))))))

(ert-deftest omt-test-sort-undated-chain ()
  "Undated chain A->B->C sorts in dependency order."
  (omt-test-with-table
      "| ID | Pred | Date | Milestone |
|----+------+------+-----------|
| 3  | 2+1d |      | C         |
| 1  |      |      | A         |
| 2  | 1+1d |      | B         |
"
    (org-milestone-table-sort-by-date)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      (should (< (string-match "| A" content)
                 (string-match "| B" content)))
      (should (< (string-match "| B" content)
                 (string-match "| C" content))))))

(ert-deftest omt-test-sort-undated-no-regression-dated-rows ()
  "Dated rows still sort ascending; undated follow in dependency order."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone |
|----+------+------------+-----------|
| 4  | 3+1d |            | Late-dep  |
| 2  |      | 2025-06-01 | Later     |
| 3  |      |            | Root      |
| 1  |      | 2025-01-01 | Earlier   |
"
    (org-milestone-table-sort-by-date)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      ;; Dated rows in ascending order
      (should (< (string-match "Earlier" content)
                 (string-match "Later"   content)))
      ;; Undated root before its dependent
      (should (< (string-match "Root"    content)
                 (string-match "Late-dep" content))))))

(ert-deftest omt-test-update-timeline-skips-undated-predecessor ()
  "A predecessor with no date is silently skipped; other preds still resolve."
  (omt-test-with-table
      "| ID | Pred     | Date       | Milestone |
|----+----------+------------+-----------|
| 1  |          | 2025-01-01 | Start     |
| 2  |          |            | Unknown   |
| 3  | 1+5d,2   |            | Depends   |
"
    (when (get-buffer "*Milestone Table Errors*")
      (kill-buffer "*Milestone Table Errors*"))
    (org-milestone-table-update-timeline)
    ;; No error buffer should have been (re)created
    (should-not (get-buffer "*Milestone Table Errors*"))
    ;; Row 3 resolves from row 1 only (row 2 skipped): 2025-01-06
    (goto-char (point-min))
    (should (search-forward "2025-01-06" nil t))))

(ert-deftest omt-test-sort-undated-anchored-before-dated-dependent ()
  "An undated row is spliced just before the earliest dated row listing it as pred."
  (omt-test-with-table
      "| ID | Pred     | Date       | Milestone |
|----+----------+------------+-----------|
| 37 | 36,22,23 | 2028-10-17 | Ops       |
| 22 |          | 2028-10-17 | MLA       |
| 23 |          | 2028-10-17 | FLA       |
| 36 |          |            | Permit    |
| 26 |          | 2028-04-28 | Contract  |
"
    (org-milestone-table-sort-by-date)
    (goto-char (point-min))
    (let ((content (buffer-string)))
      ;; Undated Permit must appear before dated Ops
      (should (< (string-match "Permit"   content)
                 (string-match "Ops"      content)))
      ;; Dated Contract (earlier date) still comes first
      (should (< (string-match "Contract" content)
                 (string-match "Permit"   content))))))

;;; --- org-milestone-table-toggle-critical-path ---

(ert-deftest omt-test-toggle-critical-path-off-and-on ()
  "Toggle turns highlighting on, then off, then on again without a prior update-timeline call."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone   |
|----+------+------------+-------------|
| 1  |      | 2025-01-01 | Start       |
| 2  | 1+5d |            | Five days   |
"
    (let ((org-milestone-table-highlight-critical-path nil))
      ;; First toggle: no prior state → turns on.
      (org-milestone-table-toggle-critical-path)
      (should (omt--current-table-overlays))
      ;; Second toggle: overlays present → turns off.
      (org-milestone-table-toggle-critical-path)
      (should-not (omt--current-table-overlays))
      ;; Third toggle: no overlays, data present → turns on again.
      (org-milestone-table-toggle-critical-path)
      (should (omt--current-table-overlays)))))

(ert-deftest omt-test-toggle-critical-path-no-prior-update ()
  "Toggle runs update-timeline itself and does not error when called cold."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone   |
|----+------+------------+-------------|
| 1  |      | 2025-01-01 | Start       |
"
    (let ((org-milestone-table-highlight-critical-path nil))
      (should-not (condition-case err
                      (progn (org-milestone-table-toggle-critical-path) nil)
                    (error err))))))

(ert-deftest omt-test-critical-path-independent-across-tables ()
  "Updating a second table does not remove overlays from the first table."
  (with-temp-buffer
    (org-mode)
    (insert "| ID | Pred | Date       | Milestone |
|----+------+------------+-----------|
| 1  |      | 2025-01-01 | Start     |
| 2  | 1+5d |            | Five days |

| ID | Pred | Date       | Milestone  |
|----+------+------------+------------|
| 1  |      | 2025-06-01 | Begin      |
| 2  | 1+3d |            | Three days |
")
    (let ((org-milestone-table-highlight-critical-path t))
      ;; Update the first table.
      (goto-char (point-min))
      (re-search-forward "^[ \t]*|")
      (beginning-of-line)
      (org-milestone-table-update-timeline)
      (let ((ovs-table1 (omt--current-table-overlays)))
        (should ovs-table1)
        ;; Move past table 1 and find table 2.
        (goto-char (org-table-end))
        (re-search-forward "^[ \t]*|")
        (beginning-of-line)
        (org-milestone-table-update-timeline)
        ;; First table overlays must still be live.
        (should (cl-every #'overlay-buffer ovs-table1))
        ;; Second table also has its own overlays.
        (should (omt--current-table-overlays))))))

;;; --- Status column / done overlays ---

(ert-deftest omt-test-parse-header-status-optional ()
  "Status column is optional; absent means col-status is nil."
  (omt-test-with-table "| ID | Pred | Date | Milestone |\n|----+------+------+-----------|\n"
    (let ((hdr (omt--parse-header)))
      (should (null (nth 4 hdr))))))

(ert-deftest omt-test-parse-header-status-recognized ()
  "Status column index is returned (case-insensitive header match)."
  (omt-test-with-table "| ID | Pred | Date | status | Milestone |\n|----+------+------+--------+-----------|\n"
    (let ((hdr (omt--parse-header)))
      (should (= (nth 4 hdr) 3)))))

(ert-deftest omt-test-parse-header-milestone-recognized ()
  "Milestone column index is returned (case-insensitive header match)."
  (omt-test-with-table "| ID | Pred | Date | Status | milestone |\n|----+------+------+--------+-----------|\n"
    (let ((hdr (omt--parse-header)))
      (should (= (nth 5 hdr) 4)))))

(ert-deftest omt-test-parse-header-milestone-absent ()
  "When no Milestone column is present, col-milestone is nil."
  (omt-test-with-table "| ID | Pred | Date | Status | Notes |\n|----+------+------+--------+-------|\n"
    (let ((hdr (omt--parse-header)))
      (should (null (nth 5 hdr))))))

(ert-deftest omt-test-status-done-p-case-insensitive ()
  "`omt--status-done-p' matches members of the defcustom case-insensitively."
  (should (omt--status-done-p "done"))
  (should (omt--status-done-p "Done"))
  (should (omt--status-done-p "DONE"))
  (should (omt--status-done-p "  done  "))
  (should-not (omt--status-done-p "todo"))
  (should-not (omt--status-done-p ""))
  (should-not (omt--status-done-p nil)))

(ert-deftest omt-test-status-done-p-respects-defcustom ()
  "Adding a value to `org-milestone-table-done-statuses' makes it match."
  (let ((org-milestone-table-done-statuses '("done" "cancelled")))
    (should (omt--status-done-p "Cancelled"))
    (should (omt--status-done-p "CANCELLED"))
    (should-not (omt--status-done-p "wontfix"))))

(ert-deftest omt-test-update-timeline-strikes-done-rows ()
  "After update-timeline, Done rows carry an `org-milestone-table-done' overlay
spanning only the trimmed Milestone cell."
  (omt-test-with-table
      "| ID | Pred | Date       | Status | Milestone |
|----+------+------------+--------+-----------|
| 1  |      | 2025-01-01 | Done   | Start     |
| 2  | 1+5d |            |        | Next      |
"
    (org-milestone-table-update-timeline)
    (let* ((entry (car omt--table-states))
           (done-ovs (nth 3 entry)))
      (should (= 1 (length done-ovs)))
      (let ((ov (car done-ovs)))
        (should (eq (overlay-get ov 'face) 'org-milestone-table-done))
        (should (string= "Start"
                         (buffer-substring-no-properties
                          (overlay-start ov) (overlay-end ov))))))))

(ert-deftest omt-test-update-timeline-no-overlay-on-non-done ()
  "Non-Done rows get no done overlay."
  (omt-test-with-table
      "| ID | Pred | Date       | Status | Milestone |
|----+------+------------+--------+-----------|
| 1  |      | 2025-01-01 | TODO   | Start     |
"
    (org-milestone-table-update-timeline)
    (let ((entry (car omt--table-states)))
      (should (null (nth 3 entry))))))

(ert-deftest omt-test-update-timeline-no-status-column ()
  "When the table has no Status column, no done overlays are created."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone |
|----+------+------------+-----------|
| 1  |      | 2025-01-01 | Start     |
"
    (org-milestone-table-update-timeline)
    (let ((entry (car omt--table-states)))
      (should (null (nth 3 entry))))))

(ert-deftest omt-test-toggle-hide-done-roundtrip ()
  "Toggle hides done rows via a separate overlay; second toggle restores them.
Throughout, the strike overlay on the Milestone cell remains present."
  (omt-test-with-table
      "| ID | Pred | Date       | Status | Milestone |
|----+------+------------+--------+-----------|
| 1  |      | 2025-01-01 | Done   | Start     |
"
    (org-milestone-table-update-timeline)
    (org-milestone-table-toggle-hide-done)
    (let* ((entry (car omt--table-states))
           (ovs (nth 3 entry))
           (hide (cl-find-if (lambda (o) (overlay-get o 'invisible)) ovs))
           (strike (cl-find-if (lambda (o)
                                 (eq (overlay-get o 'face)
                                     'org-milestone-table-done))
                               ovs)))
      (should (= 2 (length ovs)))
      (should hide)
      (should strike)
      (should-not (overlay-get strike 'invisible)))
    (org-milestone-table-toggle-hide-done)
    (let* ((entry (car omt--table-states))
           (ovs (nth 3 entry)))
      (should (= 1 (length ovs)))
      (should (eq (overlay-get (car ovs) 'face) 'org-milestone-table-done))
      (should-not (overlay-get (car ovs) 'invisible)))))

(ert-deftest omt-test-toggle-hide-done-no-status-column ()
  "Toggle errors gracefully when there is no Status column."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone |
|----+------+------------+-----------|
| 1  |      | 2025-01-01 | Start     |
"
    (should-error (org-milestone-table-toggle-hide-done) :type 'user-error)))

(ert-deftest omt-test-dwim-restrikes-done-after-sort ()
  "Done overlays survive the sort step inside C-c C-c."
  (omt-test-with-table
      "| ID | Pred  | Date       | Status | Milestone |
|----+-------+------------+--------+-----------|
| 2  | 1+5d  |            |        | Next      |
| 1  |       | 2025-01-01 | Done   | Start     |
"
    (org-milestone-table-dwim)
    (let* ((entry (car omt--table-states))
           (done-ovs (nth 3 entry)))
      (should (= 1 (length done-ovs)))
      (let* ((ov (car done-ovs))
             (text (buffer-substring-no-properties (overlay-start ov)
                                                   (overlay-end ov))))
        (should (string= "Start" text))))))

(ert-deftest omt-test-strike-spans-only-milestone-cell ()
  "The strike overlay covers only the trimmed Milestone cell, not the row."
  (omt-test-with-table
      "| ID | Pred | Date       | Status | Milestone | Notes |
|----+------+------------+--------+-----------+-------|
| 1  |      | 2025-01-01 | Done   | Start     | note  |
"
    (org-milestone-table-update-timeline)
    (let* ((entry (car omt--table-states))
           (ov (car (nth 3 entry)))
           (text (buffer-substring-no-properties
                  (overlay-start ov) (overlay-end ov))))
      (should (string= "Start" text))
      (should-not (string-match-p "|" text))
      (should-not (string-match-p "note" text)))))

(ert-deftest omt-test-strike-skipped-when-no-milestone-column ()
  "Done rows in a table with no Milestone column get no strike overlay,
but hide-done still creates a full-row invisibility overlay."
  (omt-test-with-table
      "| ID | Pred | Date       | Status | Foo   |
|----+------+------------+--------+-------|
| 1  |      | 2025-01-01 | Done   | thing |
"
    (org-milestone-table-update-timeline)
    (let ((entry (car omt--table-states)))
      (should (null (nth 3 entry))))
    (org-milestone-table-toggle-hide-done)
    (let* ((entry (car omt--table-states))
           (ovs (nth 3 entry)))
      (should (= 1 (length ovs)))
      (should (overlay-get (car ovs) 'invisible))
      (should-not (eq (overlay-get (car ovs) 'face)
                      'org-milestone-table-done)))))

(ert-deftest omt-test-status-defcustom-extension ()
  "Adding \"cancelled\" to the defcustom causes Cancelled rows to be struck."
  (let ((org-milestone-table-done-statuses '("done" "cancelled")))
    (omt-test-with-table
        "| ID | Pred | Date       | Status    | Milestone |
|----+------+------------+-----------+-----------|
| 1  |      | 2025-01-01 | Cancelled | Start     |
"
      (org-milestone-table-update-timeline)
      (let* ((entry (car omt--table-states))
             (done-ovs (nth 3 entry)))
        (should (= 1 (length done-ovs)))))))

;;; --- Fixed dates (F marker) ---

(ert-deftest omt-test-parse-date-cell-bare ()
  "Bare date returns :fixed nil and the date unchanged."
  (let ((p (omt--parse-date-cell "2025-01-10")))
    (should (null (plist-get p :fixed)))
    (should (equal (plist-get p :date) "2025-01-10"))))

(ert-deftest omt-test-parse-date-cell-fixed-prefix ()
  "F as prefix marks fixed and strips to bare date."
  (let ((p (omt--parse-date-cell "F2025-01-10")))
    (should (plist-get p :fixed))
    (should (equal (plist-get p :date) "2025-01-10"))))

(ert-deftest omt-test-parse-date-cell-fixed-space ()
  "F followed by space marks fixed and strips to bare date."
  (let ((p (omt--parse-date-cell "F 2025-01-10")))
    (should (plist-get p :fixed))
    (should (equal (plist-get p :date) "2025-01-10"))))

(ert-deftest omt-test-parse-date-cell-fixed-suffix ()
  "F as suffix also marks fixed."
  (let ((p (omt--parse-date-cell "2025-01-10 F")))
    (should (plist-get p :fixed))
    (should (equal (plist-get p :date) "2025-01-10"))))

(ert-deftest omt-test-parse-date-cell-empty ()
  "Empty cell yields :date nil and :fixed nil."
  (let ((p (omt--parse-date-cell "")))
    (should (null (plist-get p :fixed)))
    (should (null (plist-get p :date)))))

(ert-deftest omt-test-resolve-fixed-no-pred ()
  "Fixed date with no predecessor resolves to the literal date."
  (let ((tbl (make-hash-table :test 'equal))
        (errors nil)
        (row (list :id "1" :pred nil :date "2025-03-15" :fixed t)))
    (puthash "1" row tbl)
    (let ((result (omt--resolve row tbl 'errors nil)))
      (should (= result (calendar-absolute-from-gregorian '(3 15 2025))))
      (should (null errors)))))

(ert-deftest omt-test-resolve-fixed-with-pred-later ()
  "Fixed date later than predecessor uses the fixed date."
  (let ((tbl (make-hash-table :test 'equal))
        (errors nil)
        (r1 (list :id "1" :pred nil :date "2025-01-01"))
        (r2 (list :id "2" :pred "1+5d" :date "2025-02-01" :fixed t)))
    (puthash "1" r1 tbl)
    (puthash "2" r2 tbl)
    (let* ((result (omt--resolve r2 tbl 'errors nil))
           (greg (calendar-gregorian-from-absolute result)))
      (should (equal greg '(2 1 2025)))
      (should (null errors)))))

(ert-deftest omt-test-resolve-fixed-with-pred-equal ()
  "Fixed date equal to predecessor-derived date uses the fixed date."
  (let ((tbl (make-hash-table :test 'equal))
        (errors nil)
        (r1 (list :id "1" :pred nil :date "2025-01-01"))
        (r2 (list :id "2" :pred "1+5d" :date "2025-01-06" :fixed t)))
    (puthash "1" r1 tbl)
    (puthash "2" r2 tbl)
    (let* ((result (omt--resolve r2 tbl 'errors nil))
           (greg (calendar-gregorian-from-absolute result)))
      (should (equal greg '(1 6 2025)))
      (should (null errors)))))

(ert-deftest omt-test-resolve-fixed-with-pred-earlier ()
  "Fixed date earlier than predecessor-derived date is a conflict."
  (let ((tbl (make-hash-table :test 'equal))
        (r1 (list :id "1" :pred nil :date "2025-01-01"))
        (r2 (list :id "2" :pred "1+10d" :date "2025-01-05" :fixed t)))
    (puthash "1" r1 tbl)
    (puthash "2" r2 tbl)
    (defvar omt--test-fixed-errors nil)
    (setq omt--test-fixed-errors nil)
    (let ((result (omt--resolve r2 tbl 'omt--test-fixed-errors nil)))
      (should (null result))
      (should (cl-some (lambda (e) (string-match-p "conflicts with predecessors" e))
                       omt--test-fixed-errors)))))

(ert-deftest omt-test-resolve-fixed-bad-date ()
  "Fixed marker with unparseable date yields a Bad date error."
  (let ((tbl (make-hash-table :test 'equal))
        (r1 (list :id "1" :pred nil :date "2025-01-01"))
        (r2 (list :id "2" :pred "1+0d" :date "blah" :fixed t)))
    (puthash "1" r1 tbl)
    (puthash "2" r2 tbl)
    (defvar omt--test-fixed-bad-errors nil)
    (setq omt--test-fixed-bad-errors nil)
    (let ((result (omt--resolve r2 tbl 'omt--test-fixed-bad-errors nil)))
      (should (null result))
      (should (cl-some (lambda (e) (string-match-p "Bad date" e))
                       omt--test-fixed-bad-errors)))))

(ert-deftest omt-test-update-timeline-fixed-preserves-cell ()
  "Fixed cell text is preserved; downstream rows use the fixed anchor."
  (omt-test-with-table
      "| ID | Pred  | Date        | Milestone |
|----+-------+-------------+-----------|
| 1  |       | 2025-01-01  | Start     |
| 2  | 1+5d  | F2025-02-01 | Pinned    |
| 3  | 2+3d  |             | After     |
"
    (org-milestone-table-update-timeline)
    (goto-char (point-min))
    ;; Fixed cell preserved verbatim.
    (should (search-forward "F2025-02-01" nil t))
    ;; Downstream row computed from the fixed anchor (2025-02-01 + 3d).
    (goto-char (point-min))
    (should (search-forward "2025-02-04" nil t))))

(ert-deftest omt-test-update-timeline-fixed-conflict-errors ()
  "Fixed date earlier than predecessor-derived surfaces in errors buffer."
  (when (get-buffer "*Milestone Table Errors*")
    (kill-buffer "*Milestone Table Errors*"))
  (omt-test-with-table
      "| ID | Pred   | Date        | Milestone |
|----+--------+-------------+-----------|
| 1  |        | 2025-01-01  | Start     |
| 2  | 1+10d  | F2025-01-05 | Too early |
"
    (org-milestone-table-update-timeline)
    (let ((buf (get-buffer "*Milestone Table Errors*")))
      (should buf)
      (with-current-buffer buf
        (should (string-match-p "conflicts with predecessors"
                                (buffer-string)))))))

;;; --- Shorthand dates (integration) ---

(ert-deftest omt-test-update-timeline-shorthand-rewrites-cell ()
  "M/D/Y shorthand in a no-pred Date cell is rewritten to ISO."
  (let ((org-milestone-table-date-input-style 'mdy))
    (omt-test-with-table
        "| ID | Pred | Date      | Milestone |
|----+------+-----------+-----------|
| 1  |      | 5/17/2026 | Start     |
"
      (org-milestone-table-update-timeline)
      (goto-char (point-min))
      (should (search-forward "2026-05-17" nil t))
      (goto-char (point-min))
      (should-not (search-forward "5/17/2026" nil t)))))

(ert-deftest omt-test-update-timeline-shorthand-fixed-rewrites-cell ()
  "F + shorthand is rewritten to `F YYYY-MM-DD' and stays fixed."
  (let ((org-milestone-table-date-input-style 'mdy))
    (omt-test-with-table
        "| ID | Pred | Date        | Milestone |
|----+------+-------------+-----------|
| 1  |      | 2026-01-01  | Start     |
| 2  | 1+5d | F 5/17/2026 | Pinned    |
| 3  | 2+3d |             | After     |
"
      (org-milestone-table-update-timeline)
      (goto-char (point-min))
      (should (search-forward "F 2026-05-17" nil t))
      ;; Downstream row uses fixed anchor (2026-05-17 + 3d).
      (goto-char (point-min))
      (should (search-forward "2026-05-20" nil t)))))

(ert-deftest omt-test-update-timeline-shorthand-predecessor-source ()
  "Shorthand on a predecessor row is rewritten; dependents resolve."
  (let ((org-milestone-table-date-input-style 'mdy))
    (omt-test-with-table
        "| ID | Pred | Date      | Milestone |
|----+------+-----------+-----------|
| 1  |      | 5/17/2026 | Start     |
| 2  | 1+3d |           | After     |
"
      (org-milestone-table-update-timeline)
      (goto-char (point-min))
      (should (search-forward "2026-05-17" nil t))
      (goto-char (point-min))
      (should (search-forward "2026-05-20" nil t)))))

(ert-deftest omt-test-update-timeline-shorthand-invalid-errors ()
  "Invalid shorthand surfaces as `Bad date' in the errors buffer."
  (when (get-buffer "*Milestone Table Errors*")
    (kill-buffer "*Milestone Table Errors*"))
  (let ((org-milestone-table-date-input-style 'mdy))
    (omt-test-with-table
        "| ID | Pred | Date      | Milestone |
|----+------+-----------+-----------|
| 1  |      | 2/30/2026 | Oops      |
"
      (org-milestone-table-update-timeline)
      (let ((buf (get-buffer "*Milestone Table Errors*")))
        (should buf)
        (with-current-buffer buf
          (should (string-match-p "Bad date" (buffer-string))))))))

(provide 'org-milestone-table-test)
;;; org-milestone-table-test.el ends here
