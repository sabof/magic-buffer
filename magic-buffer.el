;; -*- truncate-lines: nil; eval: (when (fboundp 'wrap-column-mode) (wrap-column-mode 1)); lexical-binding: t-*-
(defvar mb-sections nil)

(cl-defmacro mb-section ((number name) &rest body)
  (declare (indent defun))
  `(let* (( cons (or (assoc ,number mb-sections)
                     (car (push (list ,number) mb-sections)))))
     (setcdr cons (list ,name
                        ,(if (stringp (car body))
                             (pop body)
                             nil)
                        (lambda ()
                          ,@body)))))

;; -----------------------------------------------------------------------------

(mb-section (1 "Horizontal line")
  "The point-entered property prevents the point from staying on that location, since that would change the color."
  (insert (propertize
           ;; (concat (make-string 999 ?\s ) "\n")
           "\n"
           'display `(space :align-to right)
           ;; 'face '(:strike-through t)
           'face '(:underline t)
           'point-entered (lambda (old new)
                            (forward-line
                             (if (< old new) 1 -1)))
           ))
  (insert "\n"))

;; -----------------------------------------------------------------------------

(mb-section (2 "Differentiate displays")
  "Documentation"
  (insert (propertize "This text will have a different background, depending on the type of display\n"
                      'face 'mb-test-face1))
  )

(defface mb-test-face1
  '(( ((type graphic))
      (:background "red"))

    ( ((class color)
       (min-colors 88))
      (:background "blue"))

    ( ((class color)
       (min-colors 88))
      (:background "green"))

    ;; No-op
    ( t (:background "black")
        ))
  "a test face")

;; -----------------------------------------------------------------------------

(defun mb-diff-windows-colorize (point-a point-b window-list)
  (message "iran")
  (cl-dolist (win (get-buffer-window-list nil nil t))
    (unless (assoc win (cdr window-list))
      (let ((ov (make-overlay point-a point-b)))
        (setcdr window-list (cl-acons win ov (cdr window-list)))
        (overlay-put ov 'window win)
        (overlay-put ov 'face
                     `(:background
                       ,(apply 'format "#%02X%02X%02X"
                               (mapcar 'random (make-list 3 255))))))
      )))

(mb-section (3 "Differentiate windows")
  (let (( text "This text will have a different background color, in each window it is displayed")
        (window-list (list 'window-list))
        point-a
        point-b)
    (setq point-a (point))
    (insert text)
    (setq point-b (point))
    (add-hook 'window-configuration-change-hook
              (lambda (&rest ignore)
                (mb-diff-windows-colorize
                 point-a
                 point-b
                 window-list))
              nil t)
    (mb-diff-windows-colorize point-a point-b window-list)
    ))

;; -----------------------------------------------------------------------------

(mb-section (4 "Horizontal Centering")
  "This paragraph will be centered in any window. It will stay centered, even if the window is resized. It will brake, should the window be narrower than the text."
  (let ((text "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec
hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl,
tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus
et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec
vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla.
Nullam rutrum. Nam vestibulum accumsan nisl."))
    (cl-dolist (text (split-string text "\n"))
      (insert (propertize text
                          'display
                          `(space :align-to (+ center (,(/ (length text) -2)
                                                       . width))))
              text
              "\n"))))

;; -----------------------------------------------------------------------------
(mb-section (5 "Widgets")
  ())
;; -----------------------------------------------------------------------------

(defun magic-buffer (&rest ignore)
  (interactive)
  (switch-to-buffer (get-buffer-create "*magic-buffer*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (fundamental-mode)
    (toggle-word-wrap 1)
    (toggle-truncate-lines -1)
    (setq revert-buffer-function 'magic-buffer)
    (cl-dolist (section (cl-sort mb-sections '< :key 'car))
      (cl-destructuring-bind (number name doc function) section
        (insert (propertize (format "%s. %s:\n" number name)
                            'face 'info-title-3))
        (when doc (insert (propertize (format "%s\n\n" doc)
                                      'face 'font-lock-comment-face)))
        (funcall function)
        (goto-char (point-max))
        (unless (zerop (current-column))
          (insert "\n"))
        (insert "\n"))))
  (unless view-mode
    (view-mode 1))
  (goto-char (point-min)))

(provide 'magic-buffer)
;;; magic-buffer.el ends here
