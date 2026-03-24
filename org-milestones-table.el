(defun org-milestone-table-empty ()
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
#+end_src


#+begin_src elisp
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
      ;; Cycle check
      (when (and id (member id visited))
        (push (format "Cycle at ID %s" id) (symbol-value errs-sym))
        (cl-return-from omt--resolve nil))
      (let ((vis (if id (cons id visited) visited)))
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
           (errors nil)
           (errs-sym 'errors))
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
                (puthash (plist-get r :id) r id-to-row)))))
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
        (if errors
            (progn
              (setq errors (delete-dups (nreverse errors)))
              (message "Errors:\n%s"
                       (mapconcat #'identity errors "\n")))
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
            (count 0))
        (dolist (lb (reverse rows-without-id))
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
              (setq next-id (1+ next-id))
              (cl-incf count))))
        (goto-char (org-table-begin))
        (org-table-align)
        (message "Added %d ID(s), starting from %d." count (1+ max-id))))))

(defun org-milestone-table-sort-by-date ()
  "Sort data rows of the milestone table at point by the Date column.
Rows without a date are sorted to the end."
  (interactive)
  (save-excursion
    (unless (org-at-table-p)
      (user-error "Not in a table"))
    (goto-char (org-table-begin))
    (let* ((hdr (omt--parse-header))
           (ncols (car hdr))
           (col-date (car (cdr (cdr (cdr hdr)))))
           (data-start nil)
           (data-end nil)
           (row-lines nil))
      ;; Skip header and hlines
      (forward-line 1)
      (while (and (looking-at "^[ \t]*|")
                  (looking-at "^[ \t]*|[-+]"))
        (forward-line 1))
      (setq data-start (point))
      ;; Collect data rows
      (while (and (looking-at "^[ \t]*|")
                  (not (looking-at "^[ \t]*|[-+]")))
        (let* ((ln (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
               (cs (omt--row-cells ln))
               (dts (when (>= (length cs) ncols)
                      (string-trim (nth col-date cs))))
               (abs-date (when (and dts (not (string-empty-p dts)))
                           (let ((pd (omt--parse-date dts)))
                             (when pd
                               (calendar-absolute-from-gregorian pd))))))
          (push (cons abs-date ln) row-lines))
        (forward-line 1))
      (setq data-end (point))
      (setq row-lines (nreverse row-lines))
      ;; Sort: rows with dates first (ascending), then rows without dates
      (setq row-lines
            (sort row-lines
                  (lambda (a b)
                    (cond
                     ((and (car a) (car b)) (< (car a) (car b)))
                     ((car a) t)
                     (t nil)))))
      ;; Replace data rows
      (delete-region data-start data-end)
      (goto-char data-start)
      (dolist (rl row-lines)
        (insert (cdr rl) "\n"))
      (goto-char (org-table-begin))
      (org-table-align)
      (message "Sorted %d row(s) by date." (length row-lines)))))
