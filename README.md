ATTRIBUTE:  The grouping of buffers on a per frame basis is a direct implementation of the concepts and select portions of code developed / written by Alp Aker in the library frame-bufs:  https://github.com/alpaker/Frame-Bufs

The following is an example of how to use the library `tabbar.el` and group tabs/buffers dynamically on a per-frame basis by either adding tabs/buffers with `C-c C-a` or removing tabs/buffers with `C-c C-n`.  There are only two (2) groups -- associated with the current frame (i.e., `"A"`), and NOT associated with the current frame (i.e., `"N"`).  The groups are frame-local, which means that each frame can have its own grouping.  The custom grouping can be reset with `C-c C-r`.  Switch between associated and non-associated groups with `C-tab`.  Switch to the next tab/buffer in the current group with `M-s-right`.  Switch to the previous tab/buffer in the current group with `M-s-left`.

Tabs/buffers can be added or removed programmatically with `my-add-buffer` and `my-remove-buffer`.  For an example of how to open certain buffers in select frames, please see the related thread entitled **How to intercept a file before it opens and decide which frame**:  https://stackoverflow.com/a/18371427/2112489  The function `my-add-buffer` would need to be incorporated at the appropriate locations of the code in the above link if a user chooses to implement that feature.

The user may wish to create an entry in a custom `mode-line-format` that displays the name of the current tab group in the mode-line by incorporating the following snippet:  `(:eval (when tabbar-mode (format "%s" (tabbar-current-tabset t))))`  Customizing the mode-line in more detail, however, is beyond the scope of this example.

The function `tabbar-add-tab` has been modified so as to alphabetize the tabs/buffers.

The function `tabbar-line-tab` has been modified so as to provide four (4) different faces depending upon the situation.  If the tab/buffer is associated with the frame and IS selected, then use `tabbar-selected-associated` face.  If the tab/buffer is associated with the frame and NOT selected, then use `tabbar-unselected-associated` face.  If the tab/buffer is NOT associated with the frame and IS selected, then use `tabbar-selected-unassociated` face.  If the tab/buffer is NOT associated with the frame and is NOT selected, then use `tabbar-unselected-unassociated` face.

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

___

The following screenshot depicts the two possible buffer/tab groupings:  (1) on the left is a grouping of those buffers/tabs that are associated with the frame named `SYSTEM` [yellow and white tabs], with the capital letter "A" indicated in the mode-line; and (2) on the right is a grouping of those buffers/tabs that are *NOT* associated with the frame named `SYSTEM` [blue and red tabs], with a capital letter "N" indicated in the mode-line.

![Example](https://www.lawlist.com/images/tabbar_example.png)