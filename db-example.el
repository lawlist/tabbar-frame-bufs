;;  To try out this example, copy and paste the entire contents of everything
;;  in this answer to the `*scratch*' buffer and type:  M-x eval-buffer RET
;;  Then type:  M-x db-example RET
;;
;;
;;  The frame names are defined with the variable `db-frame-name'.
;;  The preconfigured frame names are:  SYSTEM, MAIN, ORG, MISCELLANEOUS.
;;
;;  Buffers are displayed in specific frames based on the `buffer-name'.
;;
;;  If the `buffer-name' matches a regexp defined `db-special-buffer',
;;  then display that buffer in the current frame.
;;
;;  If the `buffer-name' matches a regexp defined by `db-system-buffer',
;;  then display that buffer in a frame named SYSTEM.
;;
;;  If the `buffer-name' matches a regexp defined by `db-main-buffer',
;;  then display that buffer in a frame named MAIN.
;;
;;  If the `buffer-name' matches a regexp defined by `db-org-buffer',
;;  then display that buffer in a frame named ORG.
;;
;;  If the `buffer-name' does not match any of the above-mentioned regexp,
;;  then display that buffer in a frame named MISCELLANEOUS.
;;
;;
;;  The following are a few methods of setting the `display-buffer-alist':
;;
;;  (1) Set the `display-buffer-alist' explicitly with `setq':
;;
;;      (setq display-buffer-alist '((".*" . (db-pop-up-frame))))
;;
;;  (2) Add to an existing `display-buffer-alist' using `add-to-list':
;;
;;      (add-to-list 'display-buffer-alist '(".*" . (db-pop-up-frame)))
;;
;;  (3) Call the function as part of the `display-buffer' statement:
;;
;;      (display-buffer (get-buffer-create "foo") '(db-pop-up-frame))
;;
;;  (4) Use the `display-buffer-alist' on a let-bound basis:
;;
;;      (let ((display-buffer-alist '((".*" . (db-pop-up-frame)))))
;;        [any additional code] )

(defvar db-frame-name "^\\(?:SYSTEM\\|MAIN\\|ORG\\|MISCELLANEOUS\\)$"
  "Frame names that are used to help organize buffers.")

(defvar db-system-buffer '("\\*scratch\\*" "\\*bbdb\\*" "\\*bar\\*")
  "Regexp of file / buffer names displayed in frame `SYSTEM`.")

(defvar db-main-buffer
  '("\\.txt" "\\.tex" "\\.el" "\\.yasnippet" "\\*foo\\*")
  "Regexp of file / buffer names displayed in frame `MAIN`.")

(defvar db-org-buffer
  '("\\*TODO\\*" "\\*Org Agenda\\*" "\\.org_archive" "\\.org")
  "Regexp of file / buffer names displayed in frame  `ORG`.")

(defvar db-special-buffer
  '("\\*special\\*" "\\*baz\\*")
  "Regexp of file / buffer names that will display in current frame.")

(defun db-pop-up-frame (buffer alist)
  (cond
    ;; condition # 1 -- either file-visiting or no-file buffers
    ((db-regexp-match-p db-org-buffer (buffer-name buffer))
      (if (db-get-frame--drew-adams "ORG")
        (select-frame-set-input-focus (db-get-frame--drew-adams "ORG"))
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match db-frame-name
                (frame-parameter frame 'name)))
            (throw 'break (progn
              (select-frame-set-input-focus
                (db-get-frame--drew-adams (frame-parameter frame 'name)))
              (set-frame-name "ORG"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (when (not (db-get-frame--drew-adams "ORG"))
          (make-frame (list '(name . "ORG")
                            '(foreground-color . "White")
                            '(background-color . "Black")))))
      (unless (get-buffer-window buffer)
       (set-window-buffer (get-largest-window) buffer))
      (select-window (get-buffer-window buffer)))
    ;; condition # 2 -- either file-visiting or no-file buffers
    ((db-regexp-match-p db-main-buffer (buffer-name buffer))
      (if (db-get-frame--drew-adams "MAIN")
        (select-frame-set-input-focus (db-get-frame--drew-adams "MAIN"))
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match db-frame-name
                (frame-parameter frame 'name)))
            (throw 'break (progn
              (select-frame-set-input-focus
                (db-get-frame--drew-adams (frame-parameter frame 'name)))
              (set-frame-name "MAIN"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (when (not (db-get-frame--drew-adams "MAIN"))
          (make-frame (list '(name . "MAIN")
                            '(foreground-color . "White")
                            '(background-color . "Black")))))
      (unless (get-buffer-window buffer)
       (set-window-buffer (get-largest-window) buffer))
      (select-window (get-buffer-window buffer)))
    ;; condition # 3 -- either file-visiting or no-file buffers
    ((db-regexp-match-p db-system-buffer (buffer-name buffer))
      (if (db-get-frame--drew-adams "SYSTEM")
        (select-frame-set-input-focus (db-get-frame--drew-adams "SYSTEM"))
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match db-frame-name
                (frame-parameter frame 'name)))
            (throw 'break (progn
              (select-frame-set-input-focus
                (db-get-frame--drew-adams (frame-parameter frame 'name)))
              (set-frame-name "SYSTEM"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (when (not (db-get-frame--drew-adams "SYSTEM"))
          (make-frame (list '(name . "SYSTEM")
                            '(foreground-color . "White")
                            '(background-color . "Black")))))
      (unless (get-buffer-window buffer)
        (set-window-buffer (get-largest-window) buffer))
      (select-window (get-buffer-window buffer)))
    ;; condition # 4
    ;; display buffer in the existing frame
    ((db-regexp-match-p db-special-buffer (buffer-name buffer))
      (unless (get-buffer-window buffer)
        (set-window-buffer (get-largest-window) buffer))
      (select-window (get-buffer-window buffer)))
    ;; condition # 5
    ;; file-visiting buffers that do NOT match any pre-defined regexp
    ((and (not (db-regexp-match-p db-org-buffer (buffer-name buffer)))
          (not (db-regexp-match-p db-main-buffer (buffer-name buffer)))
          (not (db-regexp-match-p db-system-buffer (buffer-name buffer)))
          (not (db-regexp-match-p db-special-buffer (buffer-name buffer)))
          (buffer-file-name (get-buffer (buffer-name buffer))))
      (if (db-get-frame--drew-adams "MISCELLANEOUS")
        (select-frame-set-input-focus
            (db-get-frame--drew-adams "MISCELLANEOUS"))
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match db-frame-name
                (frame-parameter frame 'name)))
            (throw 'break (progn
              (select-frame-set-input-focus
                (db-get-frame--drew-adams (frame-parameter frame 'name)))
              (set-frame-name "MISCELLANEOUS"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (when (not (db-get-frame--drew-adams "MISCELLANEOUS"))
          (make-frame (list '(name . "MISCELLANEOUS")
                            '(foreground-color . "White")
                            '(background-color . "Black")))))
      (unless (get-buffer-window buffer)
       (set-window-buffer (get-largest-window) buffer))
      (select-window (get-buffer-window buffer)))
    ;; condition # 6
    ;; default display for no-file-visiting buffers
    (t nil )))

;; https://github.com/kentaro/auto-save-buffers-enhanced
;; `db-regexp-match-p` function modified by @sds on stackoverflow
;; http://stackoverflow.com/a/20343715/2112489
(defun db-regexp-match-p (regexps string)
  (and string
       (catch 'matched
         (let ((inhibit-changing-match-data t))
           (dolist (regexp regexps)
             (when (string-match regexp string)
               (throw 'matched t)))))))

;; Original Author:  Drew Adams -- http://www.emacswiki.org/emacs/frame-fns.el
;; @lawlist combined the functions `get-frame-name` and `get-a-frame`.
(defun db-get-frame--drew-adams (frame)
  "Return a frame, if any, named FRAME (a frame or a string).
  If none, return nil.
  If FRAME is a frame, it is returned."
  (let ((get-frame-name--drew-adams
          (lambda (&optional frame)
            (unless frame (setq frame (selected-frame)))
            (if (framep frame)
                (cdr (assq 'name (frame-parameters frame)))
              (error "Argument not a frame: `%s'" frame)))))
    (cond ((framep frame) frame)
          ((stringp frame)
           (catch 'get-a-frame-found
             (dolist (fr (frame-list))
               (when (string= frame (funcall get-frame-name--drew-adams fr))
                 (throw 'get-a-frame-found fr)))
             nil))
          (t
           (error "Arg neither a string nor a frame: `%s'" frame)))))

(defun db-example ()
"This is an example that uses the custom function `db-pop-up-frame' to
display buffers in different frames or windows depending upon the situation.
The `auto-mode-alist' is set to `nil` due to a bug in one of the versions
of `org-mode' where it attempts to recenter a window that is not visible."
(interactive)
  ;; condition # 3 | file-visiting buffer
  (let ((auto-mode-alist nil)
        (display-buffer-alist '((".*" . (db-pop-up-frame)))))

    (set-background-color "Black")
    (set-foreground-color "White")

    (let ((buffer (find-file-noselect "*bar*")))
      (display-buffer buffer)
      (my-add-buffer buffer (selected-frame)))
    (set-frame-height (selected-frame) 20)
    (set-frame-width (selected-frame) 80)
    (set-frame-position (selected-frame) 0 0)
    (message "\*bar\* appears in frame name SYSTEM.")
    (sit-for 2)

    ;; condition # 4(a) | no-file-visiting buffer
    (let ((buffer (get-buffer-create "*default*")))
      (display-buffer buffer)
      (my-add-buffer buffer (selected-frame)))
    (message "NO-FILE buffer existing frame.")
    (sit-for 2)

    ;; condition # 2(a) | file-visiting buffer
    (let ((buffer (find-file-noselect "foo.txt")))
      (display-buffer buffer)
      (my-add-buffer buffer (selected-frame)))
    (set-frame-height (selected-frame) 20)
    (set-frame-width (selected-frame) 80)
    (set-frame-position (selected-frame) 100 100)
    (message "\"foo.txt\" appears in frame name MAIN.")
    (sit-for 2)

    ;; condition # 1 | file-visiting buffer
    (let ((buffer (find-file-noselect "doe.org")))
      (display-buffer buffer)
      (my-add-buffer buffer (selected-frame)))
    (set-frame-height (selected-frame) 20)
    (set-frame-width (selected-frame) 80)
    (set-frame-position (selected-frame) 200 200)
    (message "\"doe.org\" appears in frame name ORG.")
    (sit-for 2)

    ;; condition # 4(b) | file-visiting buffer
    (let ((buffer (find-file-noselect "*special*")))
      (display-buffer buffer)
      (my-add-buffer buffer (selected-frame)))
    (message "FILE buffer existing frame.")
    (sit-for 2)

    ;; condition # 6 | no-file-visiting buffer default display
    (calendar)
    (my-add-buffer (get-buffer calendar-buffer) (selected-frame))
    (message "Default for no-file-visiting-buffers.")
    (sit-for 2)

    ;; condition # 5 | file-visiting buffer with no pre-defined regexp.
    (let ((buffer (find-file-noselect "*undefined*")))
      (display-buffer buffer)
      (my-add-buffer buffer (selected-frame)))
    (set-frame-height (selected-frame) 20)
    (set-frame-width (selected-frame) 80)
    (set-frame-position (selected-frame) 300 300)
    (message "\*IS\* buffer-filename.  \*NOT\* defined by any particular regexp.")
    (sit-for 2)

    ;; condition # 2(b) | no-file-visiting buffer
    (let ((buffer (get-buffer-create "*foo*")))
      (display-buffer buffer)
      (my-add-buffer buffer (selected-frame)))
    (set-frame-height (selected-frame) 20)
    (set-frame-width (selected-frame) 80)
    (set-frame-position (selected-frame) 400 400)
    (message "\*NOT\* buffer-filename.  \*IS\* defined by db-main-buffer.")
    (sit-for 2)
    (message "`db-example':  The function has finished!")))

(defface frame-number-face
  '((t (:background "Black" :foreground "red" )))
  "Face for `frame-number-face'."
  :group 'frame-fn)

(defface frame-name-face
  '((t ( :background "Black" :foreground "ForestGreen")))
  "Face for `frame-name-face'."
  :group 'frame-fn)

(defface frame-number-selected-face
  '((t (:background "Black" :foreground "red" :weight bold)))
  "Face for `frame-number-selected-face'."
  :group 'frame-fn)

(defface frame-name-selected-face
  '((t ( :background "Black" :foreground "ForestGreen" :weight bold)))
  "Face for `frame-name-selected-face'."
  :group 'frame-fn)

(defface lawlist-frame-alpha-num-choice-face
  '((t (:background "Black" :foreground "red")))
  "Face for `lawlist-frame-alpha-num-choice-face'."
  :group 'lawlist-frame)

(defface lawlist-frame-text-choice-face
  '((t (:background "Black" :foreground "ForestGreen")))
  "Face for `lawlist-frame-text-choice-face'."
  :group 'lawlist-frame)

(defun select-frame-number ()
"Select a frame by number -- a maximum of 9 frames are supported."
(interactive)
  (let* (
      choice
      chosen-frame
      (x-server-version (and (display-graphic-p) (x-server-version)))
      (n 0)
      (current-frame-name (frame-parameter (selected-frame) 'name))
      (frame-list (frame-list))
      (total-frames (safe-length frame-list))
      (frame-name-list
        (mapcar
          (lambda (frame) (cons frame (frame-parameter frame 'name)))
          frame-list))
      (frame-name-list-sorted
        (sort
          frame-name-list
          #'(lambda (x y) (string< (cdr x) (cdr y)))))
      (frame-number-list
        (mapcar
          (lambda (frame)
            (setq n (1+ n))
            (cons n (cdr frame)))
          frame-name-list-sorted))
      (pretty-list
        (mapconcat 'identity
          (mapcar
            (lambda (x)
              (cond
                ((and
                    (equal "DIRED" (cdr x))
                    (not (equal (cdr x) current-frame-name)))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-face)
                    "/"
                    (propertize "d" 'face 'frame-number-face)
                    "]"
                    (propertize "IRED" 'face 'frame-name-face)))
                ((and
                    (equal "DIRED" (cdr x))
                    (equal (cdr x) current-frame-name))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-selected-face)
                    "/"
                    (propertize "d" 'face 'frame-number-selected-face)
                    "]"
                    (propertize "IRED" 'face 'frame-name-selected-face)))
                ((and
                    (equal "MAIN" (cdr x))
                    (not (equal (cdr x) current-frame-name)))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-face)
                    "/"
                    (propertize "m" 'face 'frame-number-face)
                    "]"
                    (propertize "AIN" 'face 'frame-name-face)))
                ((and
                    (equal "MAIN" (cdr x))
                    (equal (cdr x) current-frame-name))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-selected-face)
                    "/"
                    (propertize "m" 'face 'frame-number-selected-face)
                    "]"
                    (propertize "AIN" 'face 'frame-name-selected-face)))
                ((and
                    (equal "MISCELLANEOUS" (cdr x))
                    (not (equal (cdr x) current-frame-name)))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-face)
                    "/"
                    (propertize "M" 'face 'frame-number-face)
                    "]"
                    (propertize "ISCELLANEOUS" 'face 'frame-name-face)))
                ((and
                    (equal "MISCELLANEOUS" (cdr x))
                    (equal (cdr x) current-frame-name))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-selected-face)
                    "/"
                    (propertize "M" 'face 'frame-number-selected-face)
                    "]"
                    (propertize "ISCELLANEOUS" 'face 'frame-name-selected-face)))
                ((and
                    (equal "ORG" (cdr x))
                    (not (equal (cdr x) current-frame-name)))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-face)
                    "/"
                    (propertize "o" 'face 'frame-number-face)
                    "]"
                    (propertize "RG" 'face 'frame-name-face)))
                ((and
                    (equal "ORG" (cdr x))
                    (equal (cdr x) current-frame-name))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-selected-face)
                    "/"
                    (propertize "o" 'face 'frame-number-selected-face)
                    "]"
                    (propertize "RG" 'face 'frame-name-selected-face)))
                ((and
                    (equal "SYSTEM" (cdr x))
                    (not (equal (cdr x) current-frame-name)))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-face)
                    "/"
                    (propertize "s" 'face 'frame-number-face)
                    "]"
                    (propertize "YSTEM" 'face 'frame-name-face)))
                ((and
                    (equal "SYSTEM" (cdr x))
                    (equal (cdr x) current-frame-name))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-selected-face)
                    "/"
                    (propertize "s" 'face 'frame-number-selected-face)
                    "]"
                    (propertize "YSTEM" 'face 'frame-name-selected-face)))
                ((and
                    (equal "WANDERLUST" (cdr x))
                    (not (equal (cdr x) current-frame-name)))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-face)
                    "/"
                    (propertize "w" 'face 'frame-number-face)
                    "]"
                    (propertize "ANDERLUST" 'face 'frame-name-face)))
                ((and
                    (equal "WANDERLUST" (cdr x))
                    (equal (cdr x) current-frame-name))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-selected-face)
                    "/"
                    (propertize "w" 'face 'frame-number-selected-face)
                    "]"
                    (propertize "ANDERLUST" 'face 'frame-name-selected-face)))
                ((and
                    (equal "LAWLIST-ZTREE" (cdr x))
                    (not (equal (cdr x) current-frame-name)))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-face)
                    "/"
                    (propertize "z" 'face 'frame-number-face)
                    "]"
                    (propertize "TREE" 'face 'frame-name-face)))
                ((and
                    (equal "LAWLIST-ZTREE" (cdr x))
                    (equal (cdr x) current-frame-name))
                  (concat
                    "["
                    (propertize (format "%s" (car x)) 'face 'frame-number-selected-face)
                    "/"
                    (propertize "z" 'face 'frame-number-selected-face)
                    "]"
                    (propertize "TREE" 'face 'frame-name-selected-face)))
                (t
                  (if (equal (cdr x) current-frame-name)
                    (concat
                      "["
                      (propertize (format "%s" (car x)) 'face 'frame-number-selected-face)
                      "] "
                      (propertize (format "%s" (cdr x)) 'face 'frame-name-selected-face))
                    (concat
                      "["
                      (propertize (format "%s" (car x)) 'face 'frame-number-face)
                      "] "
                      (propertize (format "%s" (cdr x)) 'face 'frame-name-face))))))
            frame-number-list)
          " | "))  )
    (setq choice (read-char-exclusive pretty-list))
    (cond
      ((eq choice ?0)
        (setq choice 0))
      ((eq choice ?1)
        (setq choice 1))
      ((eq choice ?2)
        (setq choice 2))
      ((eq choice ?3)
        (setq choice 3))
      ((eq choice ?4)
        (setq choice 4))
      ((eq choice ?5)
        (setq choice 5))
      ((eq choice ?6)
        (setq choice 6))
      ((eq choice ?7)
        (setq choice 7))
      ((eq choice ?8)
        (setq choice 8))
      ((eq choice ?9)
        (setq choice 9))
     ((eq choice ?d)
        (setq choice "DIRED"))
      ((eq choice ?m)
        (setq choice "MAIN"))
      ((eq choice ?M)
        (setq choice "MISCELLANEOUS"))
      ((eq choice ?o)
        (setq choice "ORG"))
      ((eq choice ?s)
        (setq choice "SYSTEM"))
      ((eq choice ?w)
        (setq choice "WANDERLUST"))
      ((eq choice ?z)
        (setq choice "LAWLIST-ZTREE")))
    (cond
      ((and
          (numberp choice)
          (not (zerop choice))
          (<= choice total-frames))
        (setq chosen-frame (car (nth (1- choice) frame-name-list-sorted))))
      ((member choice '("DIRED" "MAIN" "MISCELLANEOUS" "ORG" "SYSTEM" "WANDERLUST" "LAWLIST-ZTREE"))
        (mapc
          (lambda (x)
            (when (equal choice (cdr x))
              (setq chosen-frame (car x))))
          frame-name-list-sorted)
        (when (null chosen-frame)
          (let* (
              (debug-on-quit nil)
              (quit-message
                (format "The frame `%s` does not exist." choice)))
            (signal 'quit `(,quit-message )))))
      ((eq choice 0)
        (let* (
            (debug-on-quit nil)
            (quit-message
              (format "You must select a number between 1 and %s." total-frames)))
          (signal 'quit `(,quit-message ))))
      ((and
          (or
            (eq choice 1)
            (eq choice 2)
            (eq choice 3)
            (eq choice 4)
            (eq choice 5)
            (eq choice 6)
            (eq choice 7)
            (eq choice 8)
            (eq choice 9))
          (> choice total-frames))
        (let* (
            (debug-on-quit nil)
            (quit-message
              (format "You must select a number between 1 and %s." total-frames)))
          (signal 'quit `(,quit-message ))))
      ((or
          (eq choice ?a)
          (eq choice ?A)
          (eq choice ?b)
          (eq choice ?B)
          (eq choice ?c)
          (eq choice ?C)
          ;;  (eq choice ?d)
          (eq choice ?D)
          (eq choice ?e)
          (eq choice ?E)
          (eq choice ?f)
          (eq choice ?F)
          (eq choice ?g)
          (eq choice ?G)
          (eq choice ?g)
          (eq choice ?H)
          (eq choice ?i)
          (eq choice ?I)
          (eq choice ?j)
          (eq choice ?J)
          (eq choice ?k)
          (eq choice ?K)
          (eq choice ?l)
          (eq choice ?L)
          ;;  (eq choice ?m)
          ;;  (eq choice ?M)
          (eq choice ?n)
          (eq choice ?N)
          ;;  (eq choice ?o)
          (eq choice ?O)
          (eq choice ?p)
          (eq choice ?P)
          (eq choice ?q)
          (eq choice ?Q)
          (eq choice ?R)
          (eq choice ?r)
          ;;  (eq choice ?s)
          (eq choice ?S)
          (eq choice ?t)
          (eq choice ?T)
          (eq choice ?u)
          (eq choice ?U)
          (eq choice ?v)
          (eq choice ?V)
          ;;  (eq choice ?w)
          (eq choice ?W)
          (eq choice ?x)
          (eq choice ?X)
          (eq choice ?y)
          (eq choice ?Y)
          ;;  (eq choice ?z)
          (eq choice ?Z))
        (let* (
            (debug-on-quit nil)
            (quit-message
              (format "You chose letter `%s`, which has not yet been assigned." (char-to-string choice))))
          (signal 'quit `(,quit-message ))))
      (t
        (let* (
            (debug-on-quit nil)
            (quit-message
              (format "You chose key `%s`, which has not yet been assigned."
                (if (characterp choice)
                  (char-to-string choice)
                  choice))))
          (signal 'quit `(,quit-message )))))
    ;;; Clear echo area.
    (message nil)
    (select-frame-set-input-focus chosen-frame)
    chosen-frame))

(define-key global-map (kbd "<M-tab>") 'select-frame-number)