(require 'avy)
(require 'cl-lib)

(defvar avy-flash--overlays nil
  "List of current overlays.")

(defvar avy-flash--jump-table nil
  "Alist mapping keys to jump candidates (key . candidate).")

(defconst avy-flash--lower-keys
  (number-sequence ?a ?z))
(defconst avy-flash--upper-keys
  (number-sequence ?A ?Z))

(defun avy-flash--clean ()
  "Clean up all overlays and state."
  (dolist (ov avy-flash--overlays)
    (delete-overlay ov))
  (setq avy-flash--overlays nil)
  (avy--done))

(defun avy-flash--filter-keys (search-str)
  "Return (valid-lowers valid-uppers) excluding keys that extend SEARCH-STR.
Checks if `search-str + key` has matches in the VISIBLE text."
  (let ((valid-lowers nil)
        (valid-uppers nil))
    
    (dolist (key avy-flash--lower-keys)
      (let* ((next-str (concat search-str (char-to-string key)))
             ;; Force strict case for exclusion check relative to search string
             (has-match (condition-case nil
                            (avy--regex-candidates (regexp-quote next-str))
                          (error nil))))
        (unless has-match
          (push key valid-lowers))))
    
    (dolist (key avy-flash--upper-keys)
      (let* ((next-str (concat search-str (char-to-string key)))
             (has-match (condition-case nil
                            (avy--regex-candidates (regexp-quote next-str))
                          (error nil))))
        (unless has-match
          (push key valid-uppers))))
    
    (list (nreverse valid-lowers) (nreverse valid-uppers))))

(defun avy-flash--assign-labels (candidates valid-lowers valid-uppers)
  "Assign one key per candidate from VALID-LOWERS then VALID-UPPERS.
Returns an alist of ((key . candidate) ...).
Excess candidates are left unlabeled."
  (let ((jump-table nil)
        (keys (append valid-lowers valid-uppers)))
    (while (and candidates keys)
      (push (cons (pop keys) (pop candidates)) jump-table))
    (nreverse jump-table)))

(defun avy-flash--create-interaction-overlays (jump-table)
  "Create overlays for labels in JUMP-TABLE."
  (dolist (item jump-table)
    (let* ((key (car item))
           (cand (cdr item))
           (beg (caar cand))
           (end (cdar cand))
           (wnd (cdr cand))
           ;; Overlay at the END of the match
           (ov (make-overlay end end (window-buffer wnd))))
      (overlay-put ov 'window wnd)
      (overlay-put ov 'priority 200)
      (overlay-put ov 'after-string (propertize (char-to-string key)
                                                'face 'avy-lead-face))
      (push ov avy-flash--overlays))))

(defun avy-flash--update (search-str)
  "Update candidates, label assignments, and overlays.
Strictly single-character labels."
  (avy-flash--clean)
  
  (let* ((case-fold-search nil) ;; Strict case sensitivity
         (candidates (if (string= search-str "")
                         nil
                       (condition-case nil
                           (avy--regex-candidates (regexp-quote search-str))
                         (error nil)))))
    
    (when candidates
      ;; Sort by distance from point
      (let ((pt (point)))
        (setq candidates 
              (sort candidates (lambda (a b)
                                 (< (abs (- (caar a) pt))
                                    (abs (- (caar b) pt)))))))
      
      ;; Create match highlights (for ALL matches, labeled or not)
      (dolist (cand candidates)
        (let* ((beg (caar cand))
               (end (cdar cand))
               (wnd (cdr cand))
               (ov (make-overlay beg end (window-buffer wnd))))
          (overlay-put ov 'window wnd)
          (overlay-put ov 'face 'avy-goto-char-timer-face)
          (push ov avy-flash--overlays)))

      ;; Calculate valid keys and assign labels
      (let* ((valid-pair (avy-flash--filter-keys search-str))
             (valid-lowers (car valid-pair))
             (valid-uppers (cadr valid-pair))
             (jump-table (avy-flash--assign-labels candidates valid-lowers valid-uppers)))
        
        (setq avy-flash--jump-table jump-table) ;; Expose for read-event loop
        (avy-flash--create-interaction-overlays jump-table)))
    
    candidates))

;;;###autoload
(defun avy-flash-jump ()
  "Jump to search matches with single-character dynamic labeling."
  (interactive)
  (let ((windows (avy-window-list)))
    (avy-with avy-flash-jump
      (let ((search-str "")
            (candidates nil)
            (done nil)
            (avy-flash--jump-table nil)) ; Bind dynamically
        (unwind-protect
            (progn
              (avy--make-backgrounds windows)
              (while (not done)
                (setq candidates (avy-flash--update search-str))
                (let* ((prompt (format "avy-flash: %s" 
                                      (if (string= search-str "")
                                          "..."
                                        (propertize search-str 'face 'avy-goto-char-timer-face))))
                       (event (read-event prompt)))
                  (cond
                   ;; Escape / C-g
                   ((memq event avy-escape-chars)
                    (setq done t))
                   
                   ;; Backspace
                   ((memq event avy-del-last-char-by)
                    (setq search-str (substring search-str 0 (max 0 (1- (length search-str))))))
                   
                   ;; RET: jump to first match if exists
                   ((= event 13)
                    (when candidates
                      (setq done t)
                      (let ((res (car candidates)))
                        (funcall avy-pre-action res)
                        (funcall (or avy-action avy-action-oneshot 'avy-action-goto) (caar res)))))
                   
                   ;; Character input
                   ((characterp event)
                    (let ((match-entry (assoc event avy-flash--jump-table)))
                      (if match-entry
                          ;; It IS a label -> Jump immediately
                          (let ((res (cdr match-entry)))
                            (setq done t)
                            (funcall avy-pre-action res)
                            (funcall (or avy-action avy-action-oneshot 'avy-action-goto) (caar res)))
                        
                        ;; Not a label -> Extend search
                        (setq search-str (concat search-str (string event))))))
                   
                   (t (setq done t)))))
              ;; Loop ended
              )
          (avy-flash--clean))))))

(provide 'avy-flash)
