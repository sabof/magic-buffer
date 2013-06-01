;;; magic-buffer.el --- -*- lexical-binding: t-*-
;;; Version: 0.1
;;; Author: sabof
;;; URL: https://github.com/sabof/magic-buffer

;;; Commentary:

;; The project is hosted at https://github.com/sabof/magic-buffer
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl-lib)
(require 'info) ; For title faces

(defvar mb-sections nil)
(setq mb-sections nil)
(defvar mb-counter 1)
(setq mb-counter 1)

(defmacro mb-section (name &rest body)
  (declare (indent defun))
  `(let* (( cons (car (push (list (prog1 mb-counter
                                    (cl-incf mb-counter)))
                            mb-sections))))
     (setcdr cons (list ,name
                        ,(if (stringp (car body))
                             (pop body)
                             nil)
                        (lambda ()
                          ,@body)))))

;; -----------------------------------------------------------------------------

(mb-section "Horizontal line"
  "The point-entered property prevents the point from staying on that location,\
since that would change the color of the line. Under"
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

(mb-section "Differentiate displays"
  (insert (propertize "This text will have a different background, depending on\
 the type of display. (Graphical, tty, \"full color\" tty)"
                      'face 'mb-test-face1)
          "\n")
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

    ( t (:background "gray")
        ))
  "a test face")

;; -----------------------------------------------------------------------------

(mb-section "Differentiate windows"
  (let (( text "This text will have a different background color, in each\
 window it is displayed")
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

(defun mb-diff-windows-colorize (point-a point-b window-list)
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

;; -----------------------------------------------------------------------------

(mb-section "Horizontal Centering"
  "Brakes when the window be narrower than the text."
  (let ((text "This paragraph will be centered in all windows.
It will stay centered,
even if the window is resized."))
    (cl-dolist (text (split-string text "\n"))
      (insert (propertize text
                          'display
                          `(space :align-to (- center (,(/ (length text) 2)
                                                       . width))))
              text
              "\n"))))

;; -----------------------------------------------------------------------------

(mb-section "Display on both sides of the window"
  (let (( text-left "LEFT --")
        ( text-right "-- RIGHT"))
    (insert text-left)
    (insert (propertize " " 'display
                        `(space :align-to (+ right (,(- (length text-right))
                                                    . width)))))
    (insert text-right)
    ))

;; -----------------------------------------------------------------------------

(mb-section "Variable width text flushed right"
  "Won't work should any of the lines be wider that the frame, at the moment \
of creation.
Will also break, should the size of frame's text change."
  (let (( paragraphs "Lorem ipsum dolor
Pellentesque dapibus ligula
Proin neque massa, eget, lacus
Curabitur vulputate vestibulum lorem"))
    (cl-loop for text in (split-string paragraphs "\n")
             for height = 1.0 then (+ height 0.4)
             do (let ((ori-point (point))
                      pixel-width)
                  (insert (propertize
                           text 'face `(:inherit
                                        variable-pitch
                                        :height ,height
                                        )))
                  (setq pixel-width (mb-region-pixel-width ori-point (point)))
                  (goto-char ori-point)
                  ;; (insert (format "%s -- " pixel-width))
                  (insert (propertize
                           " " 'display
                           `(space :align-to (- right (,pixel-width)))))
                  (goto-char (point-max))
                  (insert "\n")))
    ))

(defun mb-region-pixel-width (from to)
  (let (( position-x
          (lambda (pos)
            (set-window-start nil (max (point-min) (- pos 100)))
            (goto-char pos)
            (car (nth 2 (posn-at-point pos)))
            ))
        before after)
    (save-excursion
      (save-window-excursion
        (delete-other-windows)
        (unless (eq (current-buffer) (window-buffer))
          (set-window-buffer nil (current-buffer)))
        (abs (- (funcall position-x from)
                (funcall position-x to)))
        ))))

;; -----------------------------------------------------------------------------
(mb-section "Utf-8 tables"
  "Spaces might appear between characters at small font sizes. Also some fonts
don't support box characters. A table of unicode box-drawing charactres can be
found in the source code."

  ;; ─ ━ │ ┃ ┄ ┅ ┆ ┇ ┈ ┉ ┊ ┋ ┌ ┍ ┎ ┏

  ;; ┐ ┑ ┒ ┓ └ ┕ ┖ ┗ ┘ ┙ ┚ ┛ ├ ┝ ┞ ┟

  ;; ┠ ┡ ┢ ┣ ┤ ┥ ┦ ┧ ┨ ┩ ┪ ┫ ┬ ┭ ┮ ┯

  ;; ┰ ┱ ┲ ┳ ┴ ┵ ┶ ┷ ┸ ┹ ┺ ┻ ┼ ┽ ┾ ┿

  ;; ╀ ╁ ╂ ╃ ╄ ╅ ╆ ╇ ╈ ╉ ╊ ╋ ╌ ╍ ╎ ╏

  ;; ═ ║ ╒ ╓ ╔ ╕ ╖ ╗ ╘ ╙ ╚ ╛ ╜ ╝ ╞ ╟

  ;; ╠ ╡ ╢ ╣ ╤ ╥ ╦ ╧ ╨ ╩ ╪ ╫ ╬ ╭ ╮ ╯

  ;; ╰ ╱ ╲ ╳ ╴ ╵ ╶ ╷ ╸ ╹ ╺ ╻ ╼ ╽ ╾ ╿

  ;; Taken from https://en.wikipedia.org/wiki/Box_Drawing_(Unicode_block)

  (insert (propertize
           (substring "
╔══════╤══════╗ ╭──────┰──────╮
║ text │ text ║ │ text ┃ text │
╟──────┼──────╢ ┝━━━━━━╋━━━━━━┥
║ text │ text ║ │ text ┃ text │
╚══════╧══════╝ ╰──────┸──────╯
"
                      1)
           'face '(:height 2.0)
           'line-height t
           ))
  (insert "

"))
;; -----------------------------------------------------------------------------

;; (mb-section "Widgets"
;;   ())
;; -----------------------------------------------------------------------------

(defun magic-buffer (&rest ignore)
  (interactive)
  (let ((buf (get-buffer-create "*magic-buffer*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)
        (progn
          (setq truncate-lines nil)
          ;; (setq word-wrap nil) ; Bug workaround
          (setq left-fringe-width 8
                right-fringe-width 8))
        (setq revert-buffer-function 'magic-buffer)
        (cl-dolist (section (cl-sort mb-sections '< :key 'car))
          (cl-destructuring-bind (number name doc function) section
            (insert (propertize (format "%s. %s:\n" number name)
                                'face 'info-title-3))
            (if doc
                (let ((beginning (point)))
                  (insert (propertize (format "%s\n\n" doc)
                                      'face 'font-lock-comment-face))
                  (fill-region beginning (point)))
                (insert "\n"))
            (funcall function)
            (goto-char (point-max))
            (unless (zerop (current-column))
              (insert "\n"))
            (insert "\n"))))
      (unless view-mode
        (view-mode 1))
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(provide 'magic-buffer)
;;; magic-buffer.el ends here
