(require 'avy)
(require 'cl-lib)

;; 1. Define the Dimming Face
;; This ensures text loses color (becomes gray) but background stays transparent
(defface avy-flash-dim-face
  '((t (:foreground "gray40" :background unspecified :inherit nil)))
  "Face used to dim the background text during avy-flash."
  :group 'avy)

(defvar avy-flash--overlays nil
  "List of current overlays.")

(defvar avy-flash--jump-table nil
  "Alist mapping keys to jump candidates (key . candidate).")

(defconst avy-flash--lower-keys
  (number-sequence ?a ?z))
(defconst avy-flash--upper-keys
  (number-sequence ?A ?Z))

(defun avy-flash--clean ()
  "Clean up avy-flash specific overlays only."
  (dolist (ov avy-flash--overlays)
    (delete-overlay ov))
  (setq avy-flash--overlays nil))

(defun avy-flash--filter-keys (candidates)
  "Return (valid-lowers valid-uppers) excluding keys that extend CANDIDATES."
  (let ((forbidden-chars (make-hash-table :test 'eql)))
    (dolist (cand candidates)
      (let ((end (cdar cand))
            (wnd (cdr cand)))
        (with-current-buffer (window-buffer wnd)
          (let ((char (char-after end)))
            (when char
              (puthash char t forbidden-chars))))))
    
    (list 
     (cl-remove-if (lambda (k) (gethash k forbidden-chars)) avy-flash--lower-keys)
     (cl-remove-if (lambda (k) (gethash k forbidden-chars)) avy-flash--upper-keys))))

(defun avy-flash--assign-labels (candidates valid-lowers valid-uppers)
  "Assign one key per candidate from VALID-LOWERS then VALID-UPPERS."
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
           (ov (make-overlay end end (window-buffer wnd))))
      (overlay-put ov 'window wnd)
      (overlay-put ov 'priority 200)
      (overlay-put ov 'after-string (propertize (char-to-string key)
                                                'face 'avy-lead-face))
      (push ov avy-flash--overlays))))

(defun avy-flash--update (search-str)
  "Update candidates, label assignments, and overlays."
  (avy-flash--clean)
  
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
      
      ;; Create match highlights
      (dolist (cand candidates)
        (let* ((beg (caar cand))
               (end (cdar cand))
               (wnd (cdr cand))
               (ov (make-overlay beg end (window-buffer wnd))))
          (overlay-put ov 'window wnd)
          (overlay-put ov 'face 'avy-goto-char-timer-face)
          (push ov avy-flash--overlays)))

      ;; Calculate valid keys and assign labels
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
        (avy-all-windows nil)
        (avy-background t)
        ;; Force Avy to use our custom face for dimming
        (avy-background-face 'avy-flash-dim-face)) 
    (avy-with avy-flash-jump
      (let ((search-str "")
            (candidates nil)
            (done nil)
            (avy-flash--jump-table nil))
        (unwind-protect
            (progn
              ;; Create dimming overlays
              (avy--make-backgrounds windows)
              
              ;; FORCE PRIORITY: Iterate over all overlays in visible windows
              ;; and boost the ones using our dimming face.
              ;; This ensures they override syntax highlighting.
              (dolist (wnd windows)
                (with-current-buffer (window-buffer wnd)
                  (dolist (ov (overlays-in (window-start wnd) (window-end wnd)))
                    (when (eq (overlay-get ov 'face) 'avy-flash-dim-face)
                      (overlay-put ov 'priority 100)))))

              (while (not done)
                (setq candidates (avy-flash--update search-str))
                (let* ((prompt (format "avy-flash: %s" 
                                      (if (string= search-str "")
                                          "..."
                                        (propertize search-str 'face 'avy-goto-char-timer-face))))
                       (event (read-event prompt)))
                  (cond
                   ((memq event avy-escape-chars) (setq done t))
                   ((memq event avy-del-last-char-by)
                    (setq search-str (substring search-str 0 (max 0 (1- (length search-str))))))
                   ((= event 13)
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
