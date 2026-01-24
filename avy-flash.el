(require 'avy)
(require 'cl-lib)
(require 'face-remap)

;; 1. Define the Dimming Face
;; We intentionally leave weight/slant unspecified so the underlying text
;; keeps its shape (bold/italic) and doesn't shift pixels when dimmed.
(defface avy-flash-dim-face
  '((t (:foreground "gray40" 
        :weight unspecified 
        :slant unspecified
        :underline nil
        :strike-through nil
        :background unspecified)))
  "Face used to dim the background text during avy-flash."
  :group 'avy)

(defvar avy-flash--overlays nil
  "List of jump target overlays.")

(defvar avy-flash--dim-overlays nil
  "List of background dimming overlays.")

(defvar avy-flash--jump-table nil
  "Alist mapping keys to jump candidates (key . candidate).")

(defconst avy-flash--lower-keys (number-sequence ?a ?z))
(defconst avy-flash--upper-keys (number-sequence ?A ?Z))

;; Cache for propertized strings
(defvar avy-flash--string-cache (make-hash-table :test 'eql))

(defun avy-flash--get-propertized-char (char)
  "Return a cached propertized string for CHAR.
   CRITICAL FIX: We force the face to inherit the buffer's default family/height.
   This prevents the 'Monospace Label in Variable Pitch Text' issue that causes shifting."
  (or (gethash char avy-flash--string-cache)
      (puthash char 
               (propertize (char-to-string char) 
                           'face '(:inherit avy-lead-face 
                                   :family unspecified 
                                   :height unspecified 
                                   :weight normal))
               avy-flash--string-cache)))

(defun avy-flash--clean ()
  "Clean up all overlays."
  (mapc #'delete-overlay avy-flash--overlays)
  (setq avy-flash--overlays nil)
  (mapc #'delete-overlay avy-flash--dim-overlays)
  (setq avy-flash--dim-overlays nil))

(defun avy-flash--dim-buffer ()
  "Aggressively dim the buffer using high-priority overlays."
  (dolist (wnd (avy-window-list))
    (with-selected-window wnd
      (let ((ov (make-overlay (window-start) (window-end))))
        (overlay-put ov 'window wnd)
        (overlay-put ov 'face 'avy-flash-dim-face)
        (overlay-put ov 'priority 100) 
        (push ov avy-flash--dim-overlays)))))

(defun avy-flash--filter-keys (candidates)
  "Return (valid-lowers valid-uppers) excluding keys that extend CANDIDATES."
  (let ((forbidden-chars (make-hash-table :test 'eql)))
    (let ((candidates-by-window (make-hash-table :test 'eq)))
      (dolist (cand candidates)
        (let ((wnd (cdr cand)))
          (push cand (gethash wnd candidates-by-window))))
      
      (maphash 
       (lambda (wnd cands)
         (with-current-buffer (window-buffer wnd)
           (dolist (cand cands)
             (let ((char (char-after (cdar cand))))
               (when char
                 (puthash char t forbidden-chars))))))
       candidates-by-window))
    
    (list 
     (cl-delete-if (lambda (k) (gethash k forbidden-chars)) 
                   (copy-sequence avy-flash--lower-keys))
     (cl-delete-if (lambda (k) (gethash k forbidden-chars)) 
                   (copy-sequence avy-flash--upper-keys)))))

(defun avy-flash--assign-labels (candidates valid-lowers valid-uppers)
  "Assign one key per candidate from VALID-LOWERS then VALID-UPPERS."
  (let ((jump-table nil))
    (dolist (keys (list valid-lowers valid-uppers))
      (while (and candidates keys)
        (push (cons (pop keys) (pop candidates)) jump-table)))
    (nreverse jump-table)))

(defun avy-flash--create-interaction-overlays (jump-table)
  "Create overlays for labels in JUMP-TABLE."
  (dolist (item jump-table)
    (let* ((key (car item))
           (cand (cdr item))
           (pt (cdar cand))
           (wnd (cdr cand)))
      (unless (= pt (buffer-size (window-buffer wnd)))
        (with-current-buffer (window-buffer wnd)
          ;; Replace character at `pt` with the label
          (let ((ov (make-overlay pt (1+ pt) (window-buffer wnd))))
            (overlay-put ov 'window wnd)
            (overlay-put ov 'priority 200)
            (overlay-put ov 'display (avy-flash--get-propertized-char key))
            (push ov avy-flash--overlays)))))))

(defun avy-flash--update (search-str)
  "Update candidates, label assignments, and overlays."
  (mapc #'delete-overlay avy-flash--overlays)
  (setq avy-flash--overlays nil)
  
  (let* ((case-fold-search nil)
         (candidates (if (string= search-str "")
                         nil
                       (condition-case nil
                           (avy--regex-candidates (regexp-quote search-str))
                         (error nil)))))
    
    (when candidates
      (let ((pt (point)))
        (setq candidates 
              (sort candidates (lambda (a b)
                                 (< (abs (- (caar a) pt))
                                    (abs (- (caar b) pt)))))))
      
      ;; Match highlights
      (dolist (cand candidates)
        (let* ((beg (caar cand))
               (end (cdar cand))
               (wnd (cdr cand))
               (ov (make-overlay beg end (window-buffer wnd))))
          (overlay-put ov 'window wnd)
          (overlay-put ov 'priority 150)
          (overlay-put ov 'face 'avy-goto-char-timer-face)
          (push ov avy-flash--overlays)))

      (let* ((valid-pair (avy-flash--filter-keys candidates))
             (valid-lowers (car valid-pair))
             (valid-uppers (cadr valid-pair))
             (jump-table (avy-flash--assign-labels candidates valid-lowers valid-uppers)))
        
        (setq avy-flash--jump-table jump-table)
        (avy-flash--create-interaction-overlays jump-table)))
    
    candidates))

;;;###autoload
(defun avy-flash-jump ()
  "Jump to search matches with single-character dynamic labeling."
  (interactive)
  (let ((windows (avy-window-list))
        (avy-all-windows nil))
    (avy-with avy-flash-jump
      (let ((search-str "")
            (candidates nil)
            (done nil)
            (avy-flash--jump-table nil))
        (unwind-protect
            (progn
              (avy-flash--dim-buffer)

              (while (not done)
                (setq candidates (avy-flash--update search-str))
                (let* ((prompt (format "avy-flash: %s" 
                                      (if (string= search-str "")
                                          "..."
                                        (propertize search-str 'face 'avy-goto-char-timer-face))))
                       (event (read-event prompt)))
                  (cond
                   ;; FIX: Handle Delete/Backspace BEFORE checking escape chars.
                   ;; This prevents Backspace from acting as Cancel.
                   ((or (eq event 'backspace) 
                        (eq event 127)
                        (memq event avy-del-last-char-by))
                    (setq search-str (substring search-str 0 (max 0 (1- (length search-str))))))
                   
                   ((memq event avy-escape-chars) (setq done t))
                   
                   ((= event 13) ;; Enter key
                    (when candidates
                      (setq done t)
                      (let ((res (car candidates)))
                        (funcall avy-pre-action res)
                        (funcall (or avy-action avy-action-oneshot 'avy-action-goto) (caar res)))))
                   
                   ((characterp event)
                    (let ((match-entry (assoc event avy-flash--jump-table)))
                      (if match-entry
                          (let ((res (cdr match-entry)))
                            (setq done t)
                            (funcall avy-pre-action res)
                            (funcall (or avy-action avy-action-oneshot 'avy-action-goto) (caar res)))
                        (setq search-str (concat search-str (string event))))))
                   (t (setq done t)))))
              )
          (avy-flash--clean)
          (avy--done))))))

(provide 'avy-flash)
