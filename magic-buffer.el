;;; magic-buffer.el --- -*- lexical-binding: t -*-
;;; Version: 0.1
;;; Author: sabof
;;; URL: https://github.com/sabof/magic-buffer

;;; Commentary:

;; The project is hosted at https://github.com/sabof/magic-buffer
;; The latest version, and all the relevant information can be found there.
;;
;; Some sections have comments such as this:
;;
;;     (info "(elisp) Pixel Specification")
;;
;; If you place the cursor in the end, and press C-x C-e, it will take you to
;; the related info page.

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

;;; * Library ------------------------------------------------------------------
;; Functions potentially useful in other contexts

(defun mb-in-range (number from to)
  "Test whether a number is in FROM \(inclusive\) TO \(exclusive\) range."
  (and (<= from number) (< number to)))

(defun mb-table-asciify-char (char)
  "Convert UTF8 table characters to their ASCII equivalents.
If a character is not a table character, it will be left unchanged."
  ;; All table characters belong to the range 9472 inclusive - 9600 exclusive,
  ;; The comment contains the first character of each range
  (cond ( (mb-in-range char 9472 9474) ?-) ; ─
        ( (mb-in-range char 9474 9476) ?|) ; │
        ( (mb-in-range char 9476 9478) ?-) ; ┄
        ( (mb-in-range char 9478 9480) ?|) ; ┆
        ( (mb-in-range char 9480 9482) ?-) ; ┈
        ( (mb-in-range char 9482 9484) ?|) ; ┊
        ( (mb-in-range char 9484 9500) ?-) ; ┌
        ( (mb-in-range char 9500 9508) ?|) ; ├
        ( (mb-in-range char 9508 9516) ?|) ; ┤
        ( (mb-in-range char 9516 9524) ?-) ; ┬
        ( (mb-in-range char 9524 9532) ?-) ; ┴
        ( (mb-in-range char 9532 9548) ?+) ; ┼
        ( (mb-in-range char 9548 9550) ?-) ; ╌
        ( (mb-in-range char 9550 9552) ?|) ; ╎
        ( (member char '(?═ ?╒ ?╔ ?╕ ?╗ ?╘ ?╚ ?╛ ?╝))    ?=)
        ( (member char '(?║ ?╓ ?╖ ?╙ ?╜))                ?-)
        ( (member char '(?╞ ?╠ ?╡ ?╣ ?╤ ?╦ ?╧ ?╩ ?╪ ?╬)) ?=)
        ( (member char '(?╟ ?╢ ?╥ ?╨ ?╫))                ?-)
        ( (member char '(?╭ ?╯ ?╱))                      ?/)
        ( (member char '(?╮ ?╰ ?╲))   (string-to-char "\\"))
        ( (= char ?╳) ?X)
        ( (mb-in-range char 9588 9600)
          (if (cl-evenp char) ?- ?|))
        ( t char)))

(defun mb-table-asciify-string (string)
  "Convert an UTF8 table to an ASCII table.
Accepts two numeric arguments, and will replace charactres in the
the corresponding region of the buffer."
  (cl-loop for char across string
           collect (or (mb-table-asciify-char char)
                       char)
           into char-list
           finally return (apply 'string char-list)))

(defun mb-table-asciify-region (from to)
  (save-excursion
    (goto-char from)
    (insert-and-inherit
     (mb-table-asciify-string
      (delete-and-extract-region from to)))))

(defun mb-table-insert (string)
  "Insert a table with UTF8 table borders, replacing them with ASCII
fallbacks, if needed."
  (let ((start-pos (point))
        end-pos)
    (condition-case error
        (progn
          (cl-loop for char across string
                   do
                   (cl-assert (char-displayable-p char)))
          (insert (propertize
                   string
                   'face '(:height 2.0 :family "DejaVu Sans Mono")
                   ))
          (setq end-pos (point))
          (goto-char start-pos)
          (let (( regions
                  (mapcar 'car (mb-region-pixel-dimensions-multiple
                                (cl-loop while (< (point) end-pos)
                                         collecting (cons (point) (line-end-position))
                                         until (cl-plusp (forward-line)))))))
            (cl-assert (cl-every (apply-partially '= (car regions))
                                 regions)))
          (goto-char end-pos))
      (error (mb-table-asciify-region
              start-pos end-pos)
             ))))

(defun mb-random-hex-color ()
  (apply 'format "#%02X%02X%02X"
         (mapcar 'random (make-list 3 255))))

(defmacro mb-with-adjusted-enviroment (&rest body)
  (declare (indent defun))
  ;; I could just save the scroll position, if the buffer is visible in the
  ;; selected window. It migth be a tiny bit faster.
  `(if (get 'mb-with-adjusted-enviroment 'active)
       (save-excursion
         ,@body)
       (save-excursion
         (save-window-excursion
           ;; (delete-other-windows)
           (unless (eq (current-buffer) (window-buffer))
             (set-window-buffer nil (current-buffer)))
           (unwind-protect
               (progn
                 (put 'mb-with-adjusted-enviroment 'active t)
                 ,@body)
             (put 'mb-with-adjusted-enviroment 'active nil))))))

(defun mb-posn-at-point (&optional pos)
  (unless pos
    (setq pos (point)))
  (mb-with-adjusted-enviroment
    (goto-char pos)
    (or (nth 2 (posn-at-point pos))
        (progn
          (goto-char (line-beginning-position))
          (set-window-start nil (point))
          (goto-char pos)
          (nth 2 (posn-at-point pos))))))

(defun mb-region-pixel-dimensions-multiple (alist)
  (let* (( alist (copy-tree alist))
         ( sorted-alist
           (cl-sort (copy-sequence alist) '< :key 'car))
         before after)
    (mb-with-adjusted-enviroment
      (cl-loop for cons in sorted-alist
               with to = (mb-posn-at-point (car cons))
               with from = (mb-posn-at-point (cdr cons))
               do (progn (setcar cons (abs (- (car from) (car to))))
                         (setcdr cons (abs (- (cdr from) (cdr to)))))
               finally return alist))))

(defun mb-region-pixel-dimensions (from to)
  "Find a region's pixel "
  (let (( from (mb-posn-at-point from))
        ( to (mb-posn-at-point to)))
    (cons (abs (- (car from) (car to)))
          (abs (- (cdr from) (cdr to))))))

(defun es-window-inside-pixel-width (&optional window)
  (setq window (window-normalize-window window))
  (let (( window-pixel-edges (window-inside-pixel-edges)))
    (- (nth 2 window-pixel-edges) (nth 0 window-pixel-edges))))

(defun mb-align-variable-width (&optional right)
  (mb-with-adjusted-enviroment
    (beginning-of-visual-line)
    (let* (( beginning-of-visual-line)
           ( end-of-visual-line
             (save-excursion
               (end-of-visual-line)
               (point)))
           ( region (list (point)
                          (if (= (line-end-position) end-of-visual-line)
                              (line-end-position)
                              (max (point-min) (1- end-of-visual-line)))))
           ( pixel-width (car (apply 'mb-region-pixel-dimensions region)))
           ( space-spec (if right
                            ;; Full-width lines will break, when word-wrap is
                            ;; enabled. That's why I substract one pixel in the
                            ;; end. See:
                            ;; http://debbugs.gnu.org/cgi/bugreport.cgi?2749
                            `(space :align-to (- right (,pixel-width) (1)))
                            `(space :align-to (- center (,(/ pixel-width 2)))))))
      (if (= (point) (line-beginning-position))
          (if (looking-at "[\t ]+")
              (put-text-property
               (match-beginning 0)
               (match-end 0)
               'display space-spec)
              (put-text-property
               (line-beginning-position)
               (line-end-position)
               'line-prefix (propertize " " 'display space-spec)))
          (if (looking-at "[\t ]+")     ; in the middle of a logical line
              ;; In some cases, when a line is almost equal to the window's
              ;; width, and it ends with an align-to spec, it will belong to the
              ;; next line, while being centered to the previous, resulting in
              ;; that character's disappearance.
              ;;
              ;; Or something like that. Might try to reproduce it later.
              (if right
                  (put-text-property
                   (match-beginning 0)
                   (match-end 0)
                   'display
                   `(space :width (,(- (es-window-pixel-width) pixel-width))))
                  (put-text-property
                   (match-beginning 0)
                   (match-end 0)
                   'display
                   `(space :width (,(/ (- (es-window-pixel-width) pixel-width)
                                       2)))))
              (put-text-property
               (1- (point))
               (min (1+ (point)) (point-max))
               'wrap-prefix (propertize " " 'display space-spec))))
      ;; Frame width 65
      ;; (error "test")
      pixel-width
      )))

;;; * Helpers ------------------------------------------------------------------
;; Utilities that make this presentation possible

(defvar mb-sections nil)
(setq mb-sections nil)
(defvar mb-counter 1)
(setq mb-counter 1)

(defvar mb-expamle-image
  (or (and load-file-name
           (file-exists-p
            (concat
             (file-name-directory load-file-name)
             "lady-with-an-ermine.jpg"))
           (concat
            (file-name-directory load-file-name)
            "lady-with-an-ermine.jpg"))
      (and buffer-file-name
           (file-exists-p
            (concat
             (file-name-directory buffer-file-name)
             "lady-with-an-ermine.jpg"))
           (concat
            (file-name-directory buffer-file-name)
            "lady-with-an-ermine.jpg"))
      (let (( file-name
              (concat temporary-file-directory
                      "lady-with-an-ermine.jpg")))
        (url-copy-file
         "https://raw.github.com/sabof/magic-buffer/master/lady-with-an-ermine.jpg"
         file-name t)
        file-name)))

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

(defun mb-insert-filled (string)
  (let ((beginning (point)))
    (insert string)
    (fill-region beginning (point))))

(defun mb-subsection-header (string)
  (insert "\n" (propertize string 'face 'info-title-4) "\n\n"))

(defun mb-comment (string)
  (mb-insert-filled (propertize string 'face 'font-lock-comment-face))
  (insert "\n"))

(defmacro mb-insert-info-links (&rest links)
  `(progn
     (delete-char -1)
     ,@(mapcar (lambda (link)
                 `(progn
                    (insert-text-button ,(cadr link) 'action
                                        (lambda (e) (info ,(cadr link))))
                    (insert (propertize " | " 'face 'bold))))
               links)
     (delete-char -3)
     (insert "\n\n")))

;;; * Sections ------------------------------------------------------------------

(mb-section "Horizontal line"
  "The point-entered property prevents the point from staying on that location,
since that would change the color of the line."
  (insert (propertize
           "\n"
           'display `(space :align-to (- right (1)))
           'face '(:underline t)
           'point-entered (lambda (old new)
                            (forward-line
                             (if (< old new) 1 -1)))
           ))
  (insert "\n"))

;; -----------------------------------------------------------------------------

(mb-section "Differentiate displays"
  (mb-insert-info-links
   (info "(elisp) Defining Faces")
   (info "(elisp) Display Feature Testing"))

  (defface mb-diff-terminal
    '(( ((type graphic))
        (:background "DarkRed"))

      ( ((class color)
         (min-colors 88))
        (:background "blue"))

      ( ((class color)
         (min-colors 88))
        (:background "green"))

      ( t (:background "gray")
          ))
    "a test face")

  (mb-insert-filled
   (propertize "This text will have a different background, depending on \
the type of display (Graphical, tty, \"full color\" tty)."
               'face 'mb-diff-terminal))
  (insert "\n"))

;; -----------------------------------------------------------------------------

(mb-section "Differentiate windows"
  (mb-insert-info-links
   (info "(elisp) Overlay Properties"))
  (let (( text "This text will have a different background color in each \
  window it is displayed")
        ( window-list (list 'window-list))
        ( point-a (point))
        point-b)
    (insert text)
    (setq point-b (point))
    (add-hook 'window-configuration-change-hook
              (lambda (&rest ignore)
                (cl-dolist (win (get-buffer-window-list nil nil t))
                  (unless (assoc win (cdr window-list))
                    (let ((ov (make-overlay point-a point-b)))
                      (setcdr window-list (cl-acons win ov (cdr window-list)))
                      (overlay-put ov 'window win)
                      (overlay-put ov 'face
                                   `(:background
                                     ,(mb-random-hex-color))))
                    )))
              nil t)))

;; -----------------------------------------------------------------------------

(mb-section "Aligning fixed width text"
  (mb-insert-info-links
   (info "(elisp) Pixel Specification"))
  (mb-comment "The alignment will persist on window resizing, unless the window is narrower
than the text.")
  (let* (( text-lines (split-string "Lorem ipsum dolor sit amet
Sed bibendum
Curabitur lacinia pulvinar nibh
Nam euismod tellus id erat
Sed diam
Phasellus at dui in ligula mollis ultricies"
                                    "\n")))
    (mb-subsection-header "Center")
    (cl-dolist (text text-lines)
      (let ((spec `(space :align-to (- center ,(/ (length text) 2)))))
        (insert  (propertize text 'line-prefix
                             (propertize " " 'display spec))
                 "\n")))

    (mb-subsection-header "Right")
    (cl-dolist (text text-lines)
      (let ((spec `(space :align-to (- right ,(length text) (1)))))
        (insert  (propertize text 'line-prefix
                             (propertize " " 'display spec))
                 "\n"))))

  (mb-subsection-header "Display on both sides of the window")
  (let* (( text-left "LEFT --")
         ( text-right "-- RIGHT")
         ;; There is an off-by one bug. When word-wrap is enabled, the line will
         ;; break. That's why I substract one pixel in the end. This will show
         ;; up as a single empty character on terminals.
         ( spec `(space :align-to (- right ,(length text-right) (1)))))
    (insert text-left)
    (insert (propertize " " 'display spec))
    (insert text-right)
    ))

;; -----------------------------------------------------------------------------

(mb-section "Aligning variable width text"
  (mb-insert-info-links
   (info "(elisp) Pixel Specification"))
  (mb-comment "Won't work should any of the text-lines be wider that the frame, at
the moment of creation. Will also break, should the size of
frame's text change. Generating the text properties is a lot slower
than for fixed-width fonts. There might be a better way to do
right alignement, using bidi text support.")
  (let* (( paragraphs "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
Pellentesque dapibus ligula
Proin neque massa, eget, lacus
Curabitur vulputate vestibulum lorem")
         end)
    (mb-subsection-header "Center")
    (save-excursion
      (cl-loop for text in (split-string paragraphs "\n")
               for height = 2.0 then (- height 0.4)
               for face-spec = `(:inherit variable-pitch :height ,height)
               do (insert (propertize text 'face face-spec) "\n"))
      (setq end (point)))
    ;; (vertical-motion) seems to misbehave when
    ;;
    ;;     (not (eq (current-buffer) (window-buffer)))
    ;;
    ;; (mb-with-adjusted-enviroment) ensures that the buffer is displayed. It
    ;; also reduces multiple (save-window-excurson) s to one.
    (mb-with-adjusted-enviroment
      (cl-loop while (< (point) end)
               do
               (mb-align-variable-width)
               (unless (plusp (vertical-motion 1))
                 (return))))

    (goto-char (point-max))
    (mb-subsection-header "Right")
    (cl-loop for text in (split-string paragraphs "\n")
             for height = 1.0 then (+ height 0.4)
             do (let (( ori-point (point))
                      ( face-spec  `(:inherit variable-pitch :height ,height)))
                  (insert (propertize text 'face face-spec))
                  ;; (goto-char ori-point)
                  (mb-align-variable-width 'right)
                  (insert "\n")
                  ))
    ;; Fails with
    ;; (set-frame-width nil 52)
    )
  )

;; -----------------------------------------------------------------------------

(mb-section "Re-align after variable-width font lines"
  "Similar to what `fill-column-indicator' does. A similar effect
can be achieved by setting `tab-width' to a large number, and
splitting columns with tabs, but this will affect tabs in the
whole buffer. The red line will move further to the right,
should the preceeding text be long."
  (let* (( sentances (split-string
                      "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
Donec hendrerit tempor tellus.
Donec pretium posuere tellus.
Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.
Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.
Nulla posuere.
Donec vitae dolor.
Nullam tristique diam non turpis.
Cras placerat accumsan nulla.
Nullam rutrum.
Nam vestibulum accumsan nisl."
                      "\n"))
         ( spec `(space :align-to 80)))
    (cl-dolist (sentance sentances)
      (insert (propertize sentance 'face `(:inherit
                                           variable-pitch
                                           :height ,(+ 1.0 (/ (random 10) 10.0))))
              (propertize " " 'display spec)
              (propertize " " 'face '(:background "red")
                          'display '(space :width (2)))
              " More text"
              "\n"))))

;; -----------------------------------------------------------------------------

(mb-section "Center horizontally and vertically"
  (insert-button "Show in new buffer"
                 'action (lambda (e)
                           (switch-to-buffer
                            (get-buffer-create
                             "*magic-buffer-hv-centering*"))
                           (let ((inhibit-read-only t))
                             (erase-buffer)
                             (insert "test")
                             (mb-center-line-variable-width))
                           (unless view-mode
                             (view-mode 1))
                           ))
  )

;; -----------------------------------------------------------------------------

(mb-section "Extra leading"
  "The line-height property only has effect when applied to newline characters."
  (insert (propertize "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
Integer placerat tristique nisl.
Aenean in sem ac leo mollis blandit.
Nunc eleifend leo vitae magna.
"
                      'line-height 1.5
                      )))
;; -----------------------------------------------------------------------------

(mb-section "Utf-8 tables"
  "Some fonts don't support box characters well, for example the
widths might be different. For those cases an ASCII fallback is
provided. If you know which widely used fonts apart from
\"DejaVu Sans Mono\" render correctly, please let me know.

Spaces might appear between characters, especially with smaller font sizes.

A table of unicode box characters can be found in the source code."

  ;; ─ ━ │ ┃ ┄ ┅ ┆ ┇ ┈ ┉ ┊ ┋ ┌ ┍ ┎ ┏

  ;; ┐ ┑ ┒ ┓ └ ┕ ┖ ┗ ┘ ┙ ┚ ┛ ├ ┝ ┞ ┟

  ;; ┠ ┡ ┢ ┣ ┤ ┥ ┦ ┧ ┨ ┩ ┪ ┫ ┬ ┭ ┮ ┯

  ;; ┰ ┱ ┲ ┳ ┴ ┵ ┶ ┷ ┸ ┹ ┺ ┻ ┼ ┽ ┾ ┿

  ;; ╀ ╁ ╂ ╃ ╄ ╅ ╆ ╇ ╈ ╉ ╊ ╋ ╌ ╍ ╎ ╏

  ;; ═ ║ ╒ ╓ ╔ ╕ ╖ ╗ ╘ ╙ ╚ ╛ ╜ ╝ ╞ ╟

  ;; ╠ ╡ ╢ ╣ ╤ ╥ ╦ ╧ ╨ ╩ ╪ ╫ ╬ ╭ ╮ ╯

  ;; ╰ ╱ ╲ ╳ ╴ ╵ ╶ ╷ ╸ ╹ ╺ ╻ ╼ ╽ ╾ ╿

  ;; Taken from https://en.wikipedia.org/wiki/Box_Drawing_(Unicode_block)

  (let ((table1 (substring "
╔══════╤══════╗
║ text │ text ║
╟──────┼──────╢
║ text │ text ║
╚══════╧══════╝
"
                           1))
        (table2 (substring "
╭──────┰──────╮
│ text ┃ text │
┝━━━━━━╋━━━━━━┥
│ text ┃ text │
╰──────┸──────╯
"
                           1)))

    ;; In an application, especially one that where the content changes
    ;; frequently, it would probably be better to determine whether all used
    ;; table characters have equal width with letters once, and then use them or
    ;; ASCII accordingly. This would be noticably faster.

    (mb-table-insert table1)
    (mb-table-insert table2)
    ))

;; -----------------------------------------------------------------------------

(mb-section "Quoted paragraph"
  "The red line is drawn using text-properties, so the text can
be copy-pasted with without extra spaces."
  (let (( prefix (concat " "
                         (propertize " "
                                     'display '(space :width (4))
                                     'face '(:background "DarkRed"))
                         " ")))
    (mb-insert-filled
     (propertize "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Nunc
porta vulputate tellus. Proin quam nisl, tincidunt et, mattis eget, convallis
nec, purus. Nullam tempus. Pellentesque tristique imperdiet tortor. Lorem ipsum
dolor sit amet, consectetuer adipiscing elit."
                 'wrap-prefix prefix
                 'line-prefix prefix
                 'face 'italic))
    ))

;; -----------------------------------------------------------------------------

(mb-section "Fringe indicators"
  (mb-insert-info-links
   (info "(elisp) Fringe Indicators"))
  (mb-comment "fringe-indicator-alist contains the default indicators. The easiest way to
make new ones is to use an external package called `fringe-helper'.")
  (let (( insert-fringe-bitmap
          (lambda (symbol-name)
            (insert (propertize " " 'display
                                `((left-fringe ,symbol-name font-lock-comment-face)
                                  (right-fringe ,symbol-name font-lock-comment-face)))))))
    (cl-loop for pair in fringe-indicator-alist
             for iter = 0 then (1+ iter)
             do
             (unless (zerop iter)
               (insert "\n"))
             (insert (propertize (concat "* " (symbol-name (car pair)))
                                 'face 'info-title-4)
                     "\n")
             (if (symbolp (cdr pair))
                 (progn
                   (funcall insert-fringe-bitmap (cdr pair))
                   (insert (concat "  " (symbol-name (cdr pair))) "\n"))
                 (cl-dolist (bitmap (cdr pair))
                   (progn
                     (funcall insert-fringe-bitmap bitmap)
                     (insert (concat "  " (symbol-name bitmap)) "\n"))))
             )))

;; -----------------------------------------------------------------------------

(mb-section "Pointer shapes"
  (mb-insert-info-links
   (info "(elisp) Pointer Shape"))

  (mb-comment "Hover with your mouse over the labels to change the pointer.
For some reason doesn't work when my .emacs is loaded.")
  (mapc (lambda (pointer-sym)
          (insert (propertize
                   (symbol-name pointer-sym)
                   'pointer pointer-sym
                   'face '(:background "Purple"))
                  "\n\n"))
        '(text arrow hand vdrag hdrag modeline hourglass)))

;; -----------------------------------------------------------------------------

(mb-section "Images"
  "Scrolling generally misbehaves with images. Presumably `insert-sliced-image'
was made to improve the situation, but it makes things worse on occasion."
  ;; (info "(elisp) Showing Images")
  ;; (info "(elisp) Image Descriptors")
  (let (( image-size
          ;; For terminal displays
          (ignore-errors (image-size `(image :type jpeg
                                             :file ,mb-expamle-image)))))
    (mb-subsection-header "Simple case")
    (insert-image `(image :type jpeg
                          :file ,mb-expamle-image)
                  "[you should be seeing an image]")
    (insert "\n\n")
    (when image-size
      (mb-subsection-header "Using `insert-sliced-image'")
      (insert-sliced-image `(image :type jpeg
                                   :file ,mb-expamle-image)
                           "[you should be seeing an image]"
                           nil (car image-size))
      (insert "\n"))
    (mb-subsection-header "You can also crop images, or add a number of effects")
    (insert-image `(image :type jpeg
                          :file ,mb-expamle-image)
                  "[you should be seeing an image]"
                  nil '(60 25 100 150))
    (insert " ")
    (insert-image `(image :type jpeg
                          :file ,mb-expamle-image
                          :conversion disabled)
                  "[you should be seeing an image]"
                  nil '(60 25 100 150))))

(mb-section "SVG"
  "More complex effects can be achieved through SVG"
  (mb-subsection-header "Resizing an masking")
  ;; The link probably won't work on winodws
  (let ((data (format "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"109\" height=\"150\">
          <defs>
          <clipPath id=\"circularPath\" clipPathUnits=\"objectBoundingBox\">
          <circle cx=\"0.5\" cy=\"0.5\" r=\"0.5\"/>
          </clipPath>
          </defs>
          <image id=\"image\" width=\"109\" height=\"150\" style=\"clip-path: url(#circularPath);\"
          xlink:href=\"file://%s\" />
          </svg>"
                      mb-expamle-image)))
    (insert-image `(image :type svg :data ,data)
                  ))
  (insert "\n\n")
  (mb-subsection-header "Subjecting online images to multiplication and skewing")
  (let ((data "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"400\" height=\"260\">
  <image id=\"image\" x=\"10\" y=\"10\" width=\"100\" height=\"45\" transform=\"skewX(10)\"
  xlink:href=\"http://www.gnu.org/graphics/behroze/behroze-gnu-button1.png\" />
  <image id=\"image2\" x=\"50\" y=\"35\" width=\"200\" height=\"90\" transform=\"skewX(-10)\"
  xlink:href=\"http://www.gnu.org/graphics/behroze/behroze-gnu-button1.png\" />
  <image id=\"image2\" x=\"50\" y=\"100\" width=\"300\" height=\"135\" transform=\"skewX(10)\"
  xlink:href=\"http://www.gnu.org/graphics/behroze/behroze-gnu-button1.png\" />
  </svg>"
              ))
    (insert-image `(image :type svg :data ,data))))

;; -----------------------------------------------------------------------------

(mb-section "Widgets"
  (insert-button "Click me" 'action
                 (lambda (event)
                   (message "Button clicked"))))

;; -----------------------------------------------------------------------------

;; (mb-section "Colors")

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
          (setq line-spacing 0)
          (setq left-fringe-width 8
                right-fringe-width 8))
        (setq-local revert-buffer-function 'magic-buffer)
        (insert (propertize "Magic buffer"
                            'face 'info-title-2)
                "\n")
        (mb-insert-filled
         (propertize "If you want to see the source, do `M-x find-function magic-buffer'"
                     'face 'font-lock-comment-face))
        (insert "\n\n")
        (cl-dolist (section (cl-sort (cl-copy-list mb-sections) '< :key 'car))
          (cl-destructuring-bind (number name doc function) section
            (insert "\n\n")
            (insert (propertize
                     (format "%s. %s:\n" number name)
                     'face 'info-title-3))
            (if doc
                (mb-insert-filled
                 (propertize
                  (format "%s\n\n" doc)
                  'face 'font-lock-comment-face))
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

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; \\*+"
;; End:

;;; magic-buffer.el ends here
