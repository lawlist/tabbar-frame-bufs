;;;  Download tabbar.el version 2.2 by David Ponce:

;;;  Place tabbar.el in the `load-path` or add the directory (where `tabbar.el`
;;;  resides) to the `load-path`.
;;;
;;;  EXAMPLE:  (setq load-path (append '("/Users/HOME/.emacs.d/lisp/") load-path))

(require 'tabbar)

(setq tabbar-cycle-scope 'tabs)

(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(defun my-buffer-groups ()
  "Function that gives the group names the current buffer belongs to.
It must return a list of group names, or nil if the buffer has no
group.  Notice that it is better that a buffer belongs to one group."
  (list
    (cond
      ((memq (current-buffer) (my-buffer-list (selected-frame)))
        "A")
      (t
        "N"))))

(setq tabbar-buffer-groups-function 'my-buffer-groups) ;; 'tabbar-buffer-groups

;;;  redefine tabbar-add-tab so that it alphabetizes / sorts the tabs
(defun tabbar-add-tab (tabset object &optional append)
  "Add to TABSET a tab with value OBJECT if there isn't one there yet.
If the tab is added, it is added at the beginning of the tab list,
unless the optional argument APPEND is non-nil, in which case it is
added at the end."
  (let ((tabs (tabbar-tabs tabset)))
    (if (tabbar-get-tab object tabset)
        tabs
      (let* ((tab (tabbar-make-tab object tabset))
             (tentative-new-tabset
               (if append
                 (append tabs (list tab))
                 (cons tab tabs)))
             (new-tabset
               (sort
                  tentative-new-tabset
                  #'(lambda (e1 e2)
                     (string-lessp
                       (format "%s" (car e1)) (format "%s" (car e2)))))))
        (tabbar-set-template tabset nil)
        (set tabset new-tabset)))))

;;;  AUTHOR:  Alp Aker -- https://github.com/alpaker/Frame-Bufs
;;;  @lawlist extracted/revised the function(ality) from said library.
(defun my-buffer-list (frame)
  ;; Remove dead buffers.
  (set-frame-parameter frame 'frame-bufs-buffer-list
    (delq nil (mapcar #'(lambda (x) (if (buffer-live-p x) x))
      (frame-parameter frame 'frame-bufs-buffer-list))))
  ;; Return the associated-buffer list.
  (frame-parameter frame 'frame-bufs-buffer-list))

(defun my-kill-buffer-fn ()
"This function is attached to a buffer-local `kill-buffer-hook'."
  (let ((frame (selected-frame))
        (current-buffer (current-buffer)))
    (when (memq current-buffer (my-buffer-list frame))
      (my-remove-buffer current-buffer frame))))

;;;  AUTHOR:  Alp Aker -- https://github.com/alpaker/Frame-Bufs
;;;  @lawlist extracted/revised the function(ality) from said library.
(defun my-add-buffer (&optional buf frame)
"Add BUF to FRAME's associated-buffer list if not already present."
(interactive)
  (let* ((buf (if buf buf (current-buffer)))
         (frame (if frame frame (selected-frame)))
         (associated-bufs (frame-parameter frame 'frame-bufs-buffer-list)))
    (unless (bufferp buf)
      (signal 'wrong-type-argument (list 'bufferp buf)))
    (unless (memq buf associated-bufs)
      (set-frame-parameter frame 'frame-bufs-buffer-list (cons buf associated-bufs)))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook 'my-kill-buffer-fn 'append 'local))
    (when tabbar-mode (tabbar-display-update))))

;;;  AUTHOR:  Alp Aker -- https://github.com/alpaker/Frame-Bufs
;;;  @lawlist extracted/revised the function(ality) from said library.
(defun my-remove-buffer (&optional buf frame)
"Remove BUF from FRAME's associated-buffer list."
(interactive)
  (let ((buf (if buf buf (current-buffer)))
        (frame (if frame frame (selected-frame))))
    (set-frame-parameter frame 'frame-bufs-buffer-list
      (delq buf (frame-parameter frame 'frame-bufs-buffer-list)))
    (when tabbar-mode (tabbar-display-update))))

;;;  AUTHOR:  Alp Aker -- https://github.com/alpaker/Frame-Bufs
;;;  @lawlist extracted/revised the function(ality) from said library.
(defun my-buffer-list-reset ()
    "Wipe the entire slate clean for the selected frame."
  (interactive)
    (modify-frame-parameters (selected-frame) (list (cons 'frame-bufs-buffer-list nil)))
    (when tabbar-mode (tabbar-display-update)))

(defun my-switch-tab-group ()
"Switch between tab group `A` and `N`."
(interactive)
  (let ((current-group (format "%s" (tabbar-current-tabset t)))
        (tab-buffer-list (mapcar
            #'(lambda (b)
                (with-current-buffer b
                  (list (current-buffer)
                        (buffer-name)
                        (funcall tabbar-buffer-groups-function))))
                 (funcall tabbar-buffer-list-function))))
    (catch 'done
      (mapc
        #'(lambda (group)
            (when (not (equal current-group
                          (format "%s" (car (car (cdr (cdr group)))))))
              (throw 'done (switch-to-buffer (car (cdr group))))))
        tab-buffer-list))))

(defface tabbar-selected-associated
  '((t :background "black" :foreground "yellow" :box (:line-width 2 :color "yellow")))
  "Face used for the selected tab -- associated with the `frame-bufs-buffer-list`."
  :group 'tabbar)

(defface tabbar-unselected-associated
  '((t :background "black" :foreground "white" :box (:line-width 2 :color "white")))
  "Face used for unselected tabs  -- associated with the `frame-bufs-buffer-list`."
  :group 'tabbar)

(defface tabbar-selected-unassociated
  '((t :background "black" :foreground "white" :box (:line-width 2 :color "firebrick")))
  "Face used for the selected tab -- UNassociated with the `frame-bufs-buffer-list`."
  :group 'tabbar)

(defface tabbar-unselected-unassociated
  '((t :background "black" :foreground "white" :box (:line-width 2 :color "blue")))
  "Face used for unselected tabs -- UNassociated with the `frame-bufs-buffer-list`."
  :group 'tabbar)

(setq tabbar-background-color "black")

(defsubst tabbar-line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (concat
    (propertize
      (if tabbar-tab-label-function
          (funcall tabbar-tab-label-function tab)
        tab)
      'tabbar-tab tab
      'local-map (tabbar-make-tab-keymap tab)
      'help-echo 'tabbar-help-on-tab
      'mouse-face 'tabbar-highlight
      'face
        (cond
          ((and
              (tabbar-selected-p tab (tabbar-current-tabset))
              (memq (current-buffer) (my-buffer-list (selected-frame))))
            'tabbar-selected-associated)
          ((and
              (not (tabbar-selected-p tab (tabbar-current-tabset)))
              (memq (current-buffer) (my-buffer-list (selected-frame))))
            'tabbar-unselected-associated)
          ((and
              (tabbar-selected-p tab (tabbar-current-tabset))
              (not (memq (current-buffer) (my-buffer-list (selected-frame)))))
            'tabbar-selected-unassociated)
          ((and
              (not (tabbar-selected-p tab (tabbar-current-tabset)))
              (not (memq (current-buffer) (my-buffer-list (selected-frame)))))
            'tabbar-unselected-unassociated))
      'pointer 'hand)
    tabbar-separator-value))

(define-key global-map "\C-c\C-r" 'my-buffer-list-reset)

(define-key global-map "\C-c\C-a" 'my-add-buffer)

(define-key global-map "\C-c\C-n" 'my-remove-buffer)

(define-key global-map (kbd "<M-s-right>") 'tabbar-forward)

(define-key global-map (kbd "<M-s-left>") 'tabbar-backward)

(define-key global-map [C-tab] 'my-switch-tab-group)

(tabbar-mode 1)