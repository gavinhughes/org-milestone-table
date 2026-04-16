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
      (should (string-match-p "| ID | Pred | Date | Status | Milestone |" content))
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
  "After update-timeline, critical-path rows get overlays."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone   |
|----+------+------------+-------------|
| 1  |      | 2025-01-01 | Start       |
| 2  | 1+5d |            | Five days   |
"
    (org-milestone-table-update-timeline)
    ;; At least one overlay should be present
    (should omt--critical-overlays)
    ;; Every overlay should carry the critical-path face
    (dolist (ov omt--critical-overlays)
      (should (eq (overlay-get ov 'face) 'org-milestone-table-critical-path)))))

(ert-deftest omt-test-dwim-highlights-critical-path-after-sort ()
  "After C-c C-c (dwim), critical-path overlays survive the sort step."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone   |
|----+------+------------+-------------|
| 2  | 1+5d |            | Five days   |
| 1  |      | 2025-01-01 | Start       |
"
    (org-milestone-table-dwim)
    ;; Overlays should be present and on visible (non-zero-width) regions
    (should omt--critical-overlays)
    (dolist (ov omt--critical-overlays)
      (should (eq (overlay-get ov 'face) 'org-milestone-table-critical-path))
      (should (< (overlay-start ov) (overlay-end ov))))))

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
  "Toggle removes overlays, then restores them."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone   |
|----+------+------------+-------------|
| 1  |      | 2025-01-01 | Start       |
| 2  | 1+5d |            | Five days   |
"
    (org-milestone-table-update-timeline)
    (should omt--critical-overlays)
    ;; Toggle off
    (org-milestone-table-toggle-critical-path)
    (should-not omt--critical-overlays)
    ;; Toggle on
    (org-milestone-table-toggle-critical-path)
    (should omt--critical-overlays)))

(ert-deftest omt-test-toggle-critical-path-no-data ()
  "Toggle before update-timeline does not error."
  (omt-test-with-table
      "| ID | Pred | Date       | Milestone   |
|----+------+------------+-------------|
| 1  |      | 2025-01-01 | Start       |
"
    (should-not (condition-case err
                    (progn (org-milestone-table-toggle-critical-path) nil)
                  (error err)))))

(provide 'org-milestone-table-test)
;;; org-milestone-table-test.el ends here
