(require 'avy)
(require 'cl-lib)
(require 'face-remap) ;; FIXED: Explicitly require face-remap to silence compiler warnings

;; 1. Define the Dimming Face
(defface avy-flash-dim-face
  '((t (:foreground "gray40" 
        :weight normal 
        :slant normal
        :underline nil
        :strike-through nil
        :background unspecified)))
  "Face used to dim the background text during avy-flash."
  :group 'avy)

(defvar avy-flash--overlays nil
  "List of current overlays.")

(defvar avy-flash--jump-table nil
  "Alist mapping keys to jump candidates (key . candidate).")

(defvar avy-flash--remap-cookies nil)

;; OPTIMIZATION: Define reusable list of keys.
(defconst avy-flash--lower-keys
  (number-sequence ?a ?z))
(defconst avy-flash--upper-keys
  (number-sequence ?A ?Z))

;; OPTIMIZATION: Pre-define the list of faces to dim.
(defconst avy-flash--faces-to-dim
  '(font-lock-keyword-face
    font-lock-string-face
    font-lock-function-name-face
    font-lock-variable-name-face
    font-lock-type-face
    font-lock-constant-face
    font-lock-builtin-face
    font-lock-comment-face
    font-lock-doc-face
    font-lock-warning-face
    org-level-1 org-level-2 org-level-3 org-code org-block))

;; OPTIMIZATION: Cache for propertized strings.
(defvar avy-flash--string-cache (make-hash-table :test 'eql))

(defun avy-flash--get-propertized-char (char)
  "Return a cached propertized string for CHAR to avoid allocation."
  (or (gethash char avy-flash--string-cache)
      (puthash char 
               (propertize (char-to-string char) 'face 'avy-lead-face)
               avy-flash--string-cache)))

(defun avy-flash--clean ()
  "Clean up avy-flash specific overlays and face remappings."
  ;; OPTIMIZATION: mapc is slightly faster/cleaner for side effects than dolist
  (mapc #'delete-overlay avy-flash--overlays)
  (setq avy-flash--overlays nil)
  
  (mapc #'face-remap-remove-relative avy-flash--remap-cookies)
  (setq avy-flash--remap-cookies nil))

(defun avy-flash--dim-buffer ()
  "Aggressively dim the buffer by remapping common font-lock faces to gray."
  (push (face-remap-add-relative 'default 'avy-flash-dim-face)
        avy-flash--remap-cookies)
  
  ;; OPTIMIZATION: Iterate over the constant list defined above.
  (dolist (face avy-flash--faces-to-dim)
    (when (facep face)
      (push (face-remap-add-relative face 'avy-flash-dim-face)
            avy-flash--remap-cookies))))

(defun avy-flash--filter-keys (candidates)
  "Return (valid-lowers valid-uppers) excluding keys that extend CANDIDATES."
  (let ((forbidden-chars (make-hash-table :test 'eql)))
    ;; OPTIMIZATION: Group candidates by window to reduce context switching
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
    
    ;; OPTIMIZATION: Use cl-delete-if (destructive) on copies
    (list 
     (cl-delete-if (lambda (k) (gethash k forbidden-chars)) 
                   (copy-sequence avy-flash--lower-keys))
     (cl-delete-if (lambda (k) (gethash k forbidden-chars)) 
                   (copy-sequence avy-flash--upper-keys)))))

(defun avy-flash--assign-labels (candidates valid-lowers valid-uppers)
  "Assign one key per candidate from VALID-LOWERS then VALID-UPPERS."
  (let ((jump-table nil))
    ;; OPTIMIZATION: Avoid append; iterate sequentially
    (dolist (keys (list valid-lowers valid-uppers))
      (while (and candidates keys)
        (push (cons (pop keys) (pop candidates)) jump-table)))
    (nreverse jump-table)))

(defun avy-flash--create-interaction-overlays (jump-table)
  "Create overlays for labels in JUMP-TABLE."
  (dolist (item jump-table)
    (let* ((key (car item))
           (cand (cdr item))
           (end (cdar cand))
           (wnd (cdr cand))
           (ov (make-overlay end end (window-buffer wnd))))
      (overlay-put ov 'window wnd)
      (overlay-put ov 'priority 200)
      ;; OPTIMIZATION: Use cached string
      (overlay-put ov 'after-string (avy-flash--get-propertized-char key))
      (push ov avy-flash--overlays))))

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
      ;; OPTIMIZATION: Cache point once
      (let ((pt (point)))
        (setq candidates 
              (sort candidates (lambda (a b)
                                 (< (abs (- (caar a) pt))
                                    (abs (- (caar b) pt)))))))
      
      (dolist (cand candidates)
        (let* ((beg (caar cand))
               (end (cdar cand))
               (wnd (cdr cand))
               (ov (make-overlay beg end (window-buffer wnd))))
          (overlay-put ov 'window wnd)
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
