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
(require 'color)
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
          (insert string)
          (setq end-pos (point))
          (goto-char start-pos)
          (let (( regions (cl-loop while (< (point) end-pos)
                                   collecting (car (mb-posn-at-point
                                                    (line-end-position)))
                                   until (cl-plusp (forward-line)))))
            (cl-assert (cl-every (apply-partially '= (car regions))
                                 regions)))
          (goto-char end-pos))
      (error (mb-table-asciify-region
              start-pos end-pos)
             ))))

(defun mb-random-hex-color ()
  (apply 'format "#%02X%02X%02X"
         (mapcar 'random (make-list 3 255))))

(defun mb-kick-cursor (old new)
  (cond ( (and (< old new) (not (= (point-max) (point))))
          (forward-char 1))
        ( (and (not (< old new)) (not (= (point-min) (point))))
          (forward-char -1))))

(defmacro mb-with-adjusted-enviroment (&rest body)
  (declare (indent defun))
  `(save-excursion
     (cond ( (get 'mb-with-adjusted-enviroment 'active)
             ,@body)
           ( (eq (current-buffer) (window-buffer))
             (let (( initial-start (window-start)))
               (prog1 (progn ,@body)
                 (set-window-start nil initial-start))))
           ( t (unwind-protect
                   (save-window-excursion
                     (set-window-buffer nil (current-buffer))
                     (put 'mb-with-adjusted-enviroment 'active t)
                     ,@body)
                 (put 'mb-with-adjusted-enviroment 'active nil))))))

(defun mb-posn-at-point (&optional pos)
  (unless pos
    (setq pos (point)))
  (mb-with-adjusted-enviroment
    (goto-char pos)
    (or (posn-x-y (posn-at-point pos))
        (progn
          (goto-char (line-beginning-position))
          (set-window-start nil (point))
          (goto-char pos)
          (posn-x-y (posn-at-point pos))))))

(defun mb-region-pixel-width (from to)
  "Find a region's pixel "
  (mb-with-adjusted-enviroment
    (let (( from (car (mb-posn-at-point from)))
          ( to (car (mb-posn-at-point to))))
      (- to from))))

(defun mb-window-inside-pixel-width (&optional window)
  (setq window (window-normalize-window window))
  (let (( window-pixel-edges (window-inside-pixel-edges)))
    (- (nth 2 window-pixel-edges) (nth 0 window-pixel-edges))))

(defun mb-window-inside-pixel-height (&optional window)
  (setq window (window-normalize-window window))
  (let (( window-pixel-edges (window-inside-pixel-edges)))
    (- (nth 3 window-pixel-edges) (nth 1 window-pixel-edges))))

(defun mb-put-halign-overlay (from to &rest properties)
  (setq properties
        (append (list 'window (selected-window)
                      'mb-hcenterer t)
                properties))
  (let ((ov (make-overlay from to)))
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    ov))

(defun mb-align-variable-width (&optional right)
  (mb-with-adjusted-enviroment
    ;; Add protection against negative aligmnets
    (beginning-of-visual-line)
    (let* (( beginning-of-visual-line
             (point))
           ( end-of-visual-line
             (save-excursion
               (end-of-visual-line)
               ;; (when (and right (/= (line-end-position) (point)))
               ;;   (skip-chars-backward " " beginning-of-visual-line))
               ;; (sit-for 0.5)
               (point)))
           ( split-line (/= (line-end-position) end-of-visual-line))
           ( split-at-limit
             (and split-line
                  (/= (cdr (mb-posn-at-point end-of-visual-line))
                      (cdr (mb-posn-at-point (point))))))
           ( pixel-width
             (if split-at-limit
                 (- (mb-window-inside-pixel-width)
                    (car (mb-posn-at-point)))
                 (mb-region-pixel-width
                  (point)
                  end-of-visual-line)))
           ( align-spec (if right
                            ;; Full-width lines will break, when word-wrap is
                            ;; enabled. That's why I leave some space in the
                            ;; end. http://debbugs.gnu.org/cgi/bugreport.cgi?2749
                            `(space :align-to (- right (,pixel-width)
                                                 ,(if word-wrap 2.0 0)
                                                 ))
                            `(space :align-to (- center (,(/ pixel-width 2))
                                                 ;; Didn't seem to be necessary so far.
                                                 ;; ,(if word-wrap 1.0 0)
                                                 )))))
      ;; (message "split: %s at limit: %s" split-line split-at-limit)
      ;; (sit-for 0.5)
      (if (= (point) (line-beginning-position))
          (if (looking-at "[\t ]+")
              (progn
                (mb-put-halign-overlay
                 (match-beginning 0)
                 (match-end 0)
                 'display align-spec))
              (mb-put-halign-overlay
               (line-beginning-position)
               (line-end-position)
               'before-string
               ;; 'line-prefix
               (propertize " " 'display align-spec)))
          (if (looking-at "[\t ]+")     ; in the middle of a logical line
              ;; In some cases, when a line is almost equal to the window's
              ;; width, and it ends with an align-to spec, it will belong to the
              ;; next line, while being centered to the previous, resulting in
              ;; that character's disappearance.
              ;;
              ;; Or something like that. Might try to reproduce it later.
              (if right
                  (mb-put-halign-overlay
                   (match-beginning 0)
                   (match-end 0)
                   'display
                   `(space :width (,(- (mb-window-inside-pixel-width) pixel-width))))
                  (mb-put-halign-overlay
                   (match-beginning 0)
                   (match-end 0)
                   'display
                   `(space :width (,(/ (- (mb-window-inside-pixel-width) pixel-width)
                                       2)))))
              (mb-put-halign-overlay
               (1- (point))
               (min (1+ (point)) (point-max))
               'wrap-prefix (propertize " " 'display align-spec))))
      ;; (sit-for 0.5)

      ;; Frame width 65
      ;; (error "test")
      pixel-width
      )))

(defvar mb-buffer-auto-align-markers
  '((right/center (marker) (marker)))
  )

(defun mb-align-regions-horizontally-do (&optional all-windows)
  (cl-dolist (win (if all-windows
                      (get-buffer-window-list)
                      (list (selected-window))))
    ;; (with-selected-window win
    ;;   (cl-loop for (type start end) in mb-buffer-auto-align-markers
    ;;            do
    ;;            ))
    ))

(defun mb-auto-align-region-horizontally ()
  (add-hook 'window-configuration-change-hook 'ignore)
  (add-hook 'post-command-hook 'ignore))

(defun mb-delete-subsequence (from to list)
  (let (( after-to (nthcdr to list)))
    (if (zerop from)
        after-to
        (progn
          (setcdr (nthcdr (1- from) list) after-to)
          list))))

(defun mb-plist-remove-key (key plist)
  (let ((pos (cl-position key plist)))
    (if pos
        (mb-delete-subsequence
         pos (+ 2 pos) plist)
        plist)))

(defun mb-show-in-two-columns (outer-margins inner-border rows)
  (cl-dolist (row rows)
    (insert (propertize " " 'display `(space :align-to ,outer-margins))
            (car row)
            (apply 'propertize " "
                   'display `(space :align-to (- center (,inner-border . 0.5)))
                   (mb-plist-remove-key
                    'display (text-properties-at
                              0 (car row))))
            (propertize " "
                        'display `(space :width ,inner-border)
                        )
            (nth 1 row)
            (apply 'propertize " "
                   'display `(space :align-to (- right ,outer-margins))
                   (mb-plist-remove-key
                    'display (text-properties-at
                              0 (nth 1 row))))
            (apply 'propertize "\n" (car (last row))))))

(defun mb-content-height ()
  (let (added-newline)
    (mb-with-adjusted-enviroment
      (with-silent-modifications
        (set-window-start nil (point-min))
        (goto-char (point-max))
        (unless (or (equal (char-before) ?\n )
                    (= (point-min) (point-max)))
          (insert "\n")
          (setq added-newline t))
        (prog1 (cdr (posn-x-y (posn-at-point)))
          (when added-newline
            (delete-char -1)))
        ))))

(defvar mb-centerv nil)

(defun mb-virtualize-overlay (ov)
  (prog1 (append (list (overlay-start ov) (overlay-end ov))
                 (overlay-properties ov))
    (delete-overlay ov)))

(defun mb-realize-overlay (ov-spec)
  (cl-destructuring-bind
      (start end &rest props)
      ov-spec
    (let ((ov (make-overlay start end)))
      (while props (overlay-put ov (pop props) (pop props)))
      ov)))

(cl-defun mb-insert-boxed-text (text &key (border-width 3)
                                     (horizontal-margins 20)
                                     (padding 5)
                                     (border-color "DarkRed")
                                     (right-align-spec 'right))
  (let* (( segment-left-margin
           (lambda (&optional additonal-specs)
             (propertize " "
                         'display `(space :align-to (,horizontal-margins)
                                          ,@additonal-specs))))
         ( segment-filler
           (lambda (&optional additonal-specs)
             (propertize " "
                         'display `(space :align-to
                                          (- ,right-align-spec
                                             (,border-width)
                                             (,horizontal-margins))
                                          ,@additonal-specs))))
         ( segment-lr-border
           (lambda (&optional additonal-specs)
             (propertize " " 'display `(space :width (,border-width)
                                              ,@additonal-specs)
                         'face `(:background ,border-color))))
         ( segment-lr-padding
           (lambda ()
             (propertize " " 'display `(space :width (,padding)))))
         ( segment-tb-border
           (lambda ()
             (propertize
              (concat
               (funcall segment-left-margin `(:height (,border-width)))
               (propertize " "
                           'display `(space :align-to
                                            (- ,right-align-spec
                                               (,horizontal-margins))
                                            :height (,border-width))
                           'face `(:background ,border-color))
               (propertize "\n" 'line-height t))
              'point-entered 'mb-kick-cursor)))
         ( segment-tb-padding
           (lambda ()
             (propertize
              (concat
               (funcall segment-left-margin  `(:height (,padding)))
               (funcall segment-lr-border  `(:height (,padding)))
               (funcall segment-filler  `(:height (,padding)))
               (funcall segment-lr-border  `(:height (,padding)))
               (propertize "\n" 'line-height t))
              'point-entered 'mb-kick-cursor)))
         ( segment-line
           (lambda (text)
             (concat (propertize
                      (concat
                       (funcall segment-left-margin)
                       (funcall segment-lr-border)
                       (funcall segment-lr-padding))
                      'point-entered 'mb-kick-cursor)
                     (propertize text
                                 'wrap-prefix
                                 (concat (funcall segment-left-margin)
                                         (funcall segment-lr-border)
                                         (funcall segment-lr-padding)))
                     (propertize
                      (concat
                       (funcall segment-filler)
                       (funcall segment-lr-border)
                       "\n")
                      'point-entered 'mb-kick-cursor)))))

    (insert (funcall segment-tb-border)
            (funcall segment-tb-padding))
    (mapc (lambda (line) (insert (funcall segment-line line)))
          (split-string text "\n"))
    (insert (funcall segment-tb-padding)
            (funcall segment-tb-border))
    ))

(cl-defun mb--recenter-buffer-vertically (&optional dont-recenter)
  (let* (content-height
         ( window-height (mb-window-inside-pixel-height))
         ( inhibit-read-only t)
         ( old-overlays
           (cl-remove-if-not
            (lambda (ov)
              (and (overlay-get ov 'mb-vcenterer)
                   (overlay-get ov 'mb-hcenterer)
                   (eq (overlay-get ov 'window)
                       (selected-window))))
            (overlays-at (point-min))))
         ( old-before-string
           (and (car old-overlays)
                (overlay-get (car old-overlays) 'mb-hcenterer)
                (overlay-get (car old-overlays) 'before-string)))
         ( old-horizontal-centering
           (or (and old-before-string
                    (string-match " " old-before-string)
                    (get-text-property (match-beginning 0) 'display
                                       old-before-string))
               ""))
         new-before-string
         new-ov)
    (mapc 'delete-overlay old-overlays)
    (unless (setq content-height (mb-content-height))
      (cl-return-from mb--recenter-buffer-vertically nil))
    (setq new-before-string
          (concat (when content-height
                    (propertize "\n"
                                'line-height
                                (/ (- window-height
                                      content-height) 2)))
                  old-horizontal-centering))
    (when (not (string= "" new-before-string))
      (setq new-ov (make-overlay
                    (point-min)
                    (min (point-max)
                         (1+ (point-min)))))
      (overlay-put new-ov 'mb-centerer t)
      ;; (overlay-put new-ov 'read-only t)
      (overlay-put new-ov 'window (selected-window))
      (overlay-put new-ov 'before-string new-before-string))
    (unless dont-recenter
      (set-window-start nil (point-min)))))

(cl-defun mb-center-buffer-vertically (&optional (buffer (current-buffer)))
  "Inserts a newline character in the beginning of the buffer,
displayed in a way that will make the buffer appear vertically
centered. Not meant to be used in \"writing\" buffers, where
undo history is important."
  (with-current-buffer buffer
    (add-hook 'window-configuration-change-hook
              'mb--recenter-buffer-vertically
              nil t)
    (add-hook 'post-command-hook
              'mb--recenter-buffer-vertically
              nil t)
    (add-hook 'window-scroll-functions
              (lambda (&rest ignore)
                (mb--recenter-buffer-vertically t))
              nil t)
    ;; (add-hook 'window-scroll-functions 'mb--recenter-buffer nil t)
    ))

(define-minor-mode mb-alignment-mode
  "Doc" nil nil nil
  (if mb-alignment-mode
      (progn
        )))

;;; * Helpers ------------------------------------------------------------------
;; Utilities that make this presentation possible

(defvar mb-sections nil)
(setq mb-sections nil)
(defvar mb-counter 1)
(setq mb-counter 1)
(defvar mb--exclusive-section nil
  "Only show the section with a particular number.
Created to ease development.")
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

(defmacro mb-subsection (name &rest body)
  (declare (indent defun))
  `(progn
     (insert "\n" (propertize ,name 'face 'info-title-4) "\n")
     ,@body))

(defun mb-insert-filled (string)
  (let ((beginning (point)))
    (insert string)
    (fill-region beginning (point))))

(defun mb-comment (string)
  (mb-insert-filled (propertize string 'face '(:inherit (variable-pitch font-lock-comment-face))))
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
     (insert "\n")))

;;; * Sections ------------------------------------------------------------------

(mb-section "Horizontal line"
  "The point-entered property prevents the point from staying on that location,
since that would change the color of the line."
  (insert (propertize
           "\n"
           'display `(space :align-to (- right (1)))
           'face '(:underline t)
           'point-entered 'mb-kick-cursor
           ))
  (insert "\n"))

;; -----------------------------------------------------------------------------

(mb-section "Stipples / 2 columns / Line cursor"
  "Uses stipples that come with your unix distribution. I'll add
a cutsom stipple example later. They have some re-drawing issues after scrolling."
  (let ((ori-point (point))
        grid-strings stipple-names)
    (cl-dolist (dir x-bitmap-file-path)
      (setq stipple-names
            (nconc
             stipple-names
             (cl-remove-if (lambda (file) (member file '(".." ".")))
                           (directory-files dir)))))
    (setq stipple-names
          (sort stipple-names
                (lambda (&rest ignore)
                  (zerop (random 2)))))
    (setq stipple-names
          (last stipple-names 12))
    (while stipple-names
      (let* (( current-batch
               (list (pop stipple-names)
                     (pop stipple-names))))
        (push (nconc (mapcar
                      (lambda (name)
                        (if (not name)
                            " "
                            (propertize name 'face
                                        '(:weight
                                          bold
                                          :inherit variable-pitch))))
                      current-batch)
                     (list (list 'line-height 2.0)))
              grid-strings)
        (push (nconc (mapcar (lambda (stipple)
                               (if (not stipple)
                                   " "
                                   (propertize " "
                                               'face
                                               `(:inherit
                                                 font-lock-comment-face
                                                 :stipple ,stipple))))
                             current-batch)
                     (list (list 'line-height 2.0)))
              grid-strings)))
    (setq tmp (length grid-strings))
    (setq grid-strings (nreverse grid-strings))
    (mb-show-in-two-columns '(20) 2 grid-strings)
    (backward-char)
    (setq-local face-remapping-alist
                `((hl-line (:background
                            ,(apply 'format "#%02X%02X%02X"
                                    (mapcar (apply-partially '* 255)
                                            (color-complement
                                             (face-attribute 'cursor :background))))
                            :inverse-video t))))
    (add-text-properties ori-point (point)
                         (list 'point-entered (lambda (&rest ignore)
                                                (setq cursor-type nil)
                                                (hl-line-mode))
                               'point-left (lambda (&rest ignore)
                                             (setq cursor-type t)
                                             (hl-line-mode -1))))))

;; -----------------------------------------------------------------------------

(mb-section "Differentiate displays"
  (mb-insert-info-links
   (info "(elisp) Defining Faces")
   (info "(elisp) Display Feature Testing"))
  (insert "\n")
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
  (insert "\n")
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
  (let* (( text-lines (split-string
                       "Reperiuntur quaeritur horrent nisi, summum solido animo
Consequi quapropter e sed dolor ita fore
Censes profecto legendos neque quid, omne laudantium putanda beatus philosophi
Fieri quam ad nos et ut alios voluptatibus, statuam
Cernantur individua ista dicam tua igitur philosophia amicitia numeranda arbitratu"
                       "\n")))
    (mb-subsection "Center"
      (insert "\n")
      (cl-dolist (text text-lines)
        (let ((spec `(space :align-to (- center ,(/ (length text) 2)))))
          (insert (propertize text 'line-prefix
                              (propertize " " 'display spec))
                  "\n"))))

    (mb-subsection "Right"
      (insert "\n")
      (cl-dolist (text text-lines)
        (let ((spec `(space :align-to (- right ,(length text) (1)))))
          (insert (propertize text 'line-prefix
                              (propertize " " 'display spec))
                  "\n")))))

  (mb-subsection "Display on both sides of the window"
    (insert "\n")
    (let* (( text-left "LEFT --")
           ( text-right "-- RIGHT")
           ( spec `(space :align-to (- right ,(length text-right) (1)))))
      (insert text-left)
      (insert (propertize " " 'display spec))
      (insert text-right)
      )))

;; -----------------------------------------------------------------------------

(mb-section "Aligning variable width text"
  (mb-insert-info-links
   (info "(elisp) Pixel Specification"))
  (mb-comment "Will break, should the size of frame's text
change. If there are line breaks, the lines won't align after a
window resize. *WIP*")
  (let* (( paragraphs "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Sed id ligula quis est convallis tempor.
Pellentesque dapibus ligula
Proin neque massa, eget, lacus
Curabitur vulputate vestibulum lorem")
         start end)
    (mb-subsection "Center"
      (insert "\n")
      (setq start (point))
      (cl-loop for text in (split-string paragraphs "\n")
               for height = 2.4 then (- height 0.4)
               for face-spec = `(:inherit variable-pitch :height ,height)
               do (insert (propertize text 'face face-spec) "\n"))
      (setq end (point))
      (let (virtual-overlays)
        ;; (vertical-motion) seems to misbehave when the buffer is burried
        ;; (mb-with-adjusted-enviroment) ensures that the buffer is displayed. It
        ;; also reduces multiple (save-window-excurson)s to one.
        (mb-with-adjusted-enviroment
          (goto-char start)
          (cl-loop while (< (point) end)
                   do
                   (mb-align-variable-width)
                   (unless (cl-plusp (vertical-motion 1))
                     (return)))))
      (goto-char (point-max)))
    (mb-subsection "Right"
      (insert "\n")
      (save-excursion
        (cl-loop for text in (split-string paragraphs "\n")
                 for height = 1.0 then (+ height 0.4)
                 for face-spec = `(:inherit variable-pitch :height ,height)
                 do (insert (propertize text 'face face-spec) "\n"))
        (setq end (point)))

      (mb-with-adjusted-enviroment
        (cl-loop while (< (point) end)
                 do
                 (mb-align-variable-width 'right)
                 (unless (cl-plusp (vertical-motion 1))
                   (return)))))
    ))

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
                          'display '(space :width (3)))
              " More text"
              "\n"))))

;; -----------------------------------------------------------------------------

;; (mb-section "Center horizontally and vertically"
;;   (insert-button "Show in new buffer"
;;                  'action (lambda (e)
;;                            (switch-to-buffer
;;                             (get-buffer-create
;;                              "*magic-buffer-hv-centering*"))
;;                            (let ((inhibit-read-only t))
;;                              (erase-buffer)
;;                              (insert "test")
;;                              (mb-center-line-variable-width))
;;                            (unless view-mode
;;                              (view-mode 1))
;;                            ))
;;   )

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

    (mb-table-insert
     (propertize table1 'face '(:height 1.4 :family "DejaVu Sans Mono")))
    (mb-table-insert
     (propertize table2 'face '(:height 1.4 :family "DejaVu Sans Mono")))
    ))

;; -----------------------------------------------------------------------------

(mb-section "Decorated paragraphs"
  (let (( prefix (concat (propertize " " 'display '(space :width (20)))
                         (propertize " " 'display '(space :width (3))
                                     'face '(:background "DarkRed"))
                         (propertize " " 'display '(space :width (5))))))

    (mb-subsection "Paragraph with a single line"
      (mb-comment "The red line is drawn using text-properties, so the text can
be copy-pasted without extra spaces.")
      (insert "\n")
      (mb-insert-filled
       (propertize "Nixam aliquando efficiat, omittendis ad, aliter similia hominem exitum, temeritate.
Disserendum neque fortasse, consequantur illud et erat potest voluptas temperantiam isdem.
Quod mihi loca consilio ipsius, aliae quo voluptatis.
Quod nisi litteras fieri posuit torquate expetendam cum."
                   'wrap-prefix prefix
                   'line-prefix prefix
                   'face '(:slant italic :inherit variable-pitch)))
      (insert "\n"))

    (mb-subsection "Boxed paragraph"
      (mb-comment "Should the contained text exceed the width of the box, gaps
will appear in the right border.")
      (insert "\n")
      (mb-insert-boxed-text
       (propertize "Falsi autem ut constituto tarentinis, sapientiam.
Eoque integris ennius morborum impensa quadam quae apud provocatus, cum."
                   'face 'variable-pitch)))

    (mb-subsection "Extra leading"
      (mb-comment "The line-height property only has effect when applied to newline characters.")
      (insert "\n")
      (insert (propertize "Sollicitudines regione finiri est inpotenti patria, dolorum morati omnino latinas.
Ullam ipso tot assentior ita etiam.
Etiamsi facio illas et notissima et bonis quod semper disserendi alias.
Ab beateque omnem in humili, mandamus potest constituant amicitia, quoniam.
"
                          'face 'variable-pitch
                          'line-height 1.5
                          )))
    ))

;; -----------------------------------------------------------------------------

(mb-section "Fringe indicators"
  (mb-insert-info-links
   (info "(elisp) Fringe Indicators"))
  (mb-comment "fringe-indicator-alist contains the default indicators. The easiest way to
make new ones is to use an external package called `fringe-helper'.")
  (insert "\n")
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
             (insert (propertize (concat "✸ " (symbol-name (car pair)))
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
  (mb-comment "Hover with your mouse over the labels to change the pointer.")
  (insert "\n")
  (mapc (lambda (pointer-sym)
          (insert "  "
                  (propertize
                   (concat "  " (symbol-name pointer-sym) "  ")
                   'pointer pointer-sym
                   'face `(:height 1.0
                                   :background ,(face-attribute 'font-lock-keyword-face
                                                                :foreground)
                                   :inherit variable-pitch)
                   'mouse-face '(:height 1.0
                                         :background "#888"
                                         :foreground "black"
                                         :inherit variable-pitch))))
        '(text arrow hand vdrag hdrag modeline hourglass))

  ;; As far as I was able to tell, this line-height format translates to
  ;; ((+ TEXT-HEIGHT TOP) (+ TEXT-HEIGHT BOTTOM))
  ;; the line-height info page is wrong

  (insert (propertize "\n" 'line-height '(1.5 1.5))))

  ;; -----------------------------------------------------------------------------

  (mb-section "Images"
    (mb-insert-info-links
     (info "(elisp) Showing Images")
     (info "(elisp) Image Descriptors"))
    (mb-comment "Scrolling generally misbehaves with images. Presumably `insert-sliced-image'
was made to improve the situation, but it makes things worse on occasion.")
    (let (( image-size
            ;; For terminal displays
            (ignore-errors (image-size `(image :type jpeg
                                               :file ,mb-expamle-image)
                                       t))))
      (mb-subsection "Simple case"
        (insert "\n")
        (insert-image `(image :type jpeg
                              :file ,mb-expamle-image)
                      "[you should be seeing an image]")
        (insert "\n\n")
        (when image-size
          (mb-subsection "Using `insert-sliced-image'"
            (mb-comment "point-entered hook is used,
to prevent a box from showing around individual slices.")
            (insert "\n")
            (let (( start (point)))
              (insert-sliced-image `(image :type jpeg
                                           :file ,mb-expamle-image)
                                   "[you should be seeing an image]"
                                   nil (max 1 (1- (/ (car image-size)
                                                     (frame-char-height)))))
              (goto-char start)
              (cl-loop until (= (point) (point-max))
                       for segment in (list "You can also add text next to"
                                            "sliced images. The technique"
                                            "I'm using is problematic,"
                                            "as the distance between the"
                                            "lines is noticably larger."
                                            "There might be a better way"
                                            "to do it.")
                       do (progn
                            (goto-char (line-beginning-position))
                            (when (cl-getf (text-properties-at (point)) 'display)
                              (goto-char (line-end-position))
                              (insert "   " (propertize
                                             segment
                                             'face
                                             '(:slant italic
                                                      :inherit (variable-pitch
                                                                font-lock-comment-face)))))
                            (forward-line)))
              (goto-char (point-max))
              (add-text-properties
               start (point)
               (list 'point-entered
                     (lambda (old new)
                       (let ((props (text-properties-at (point))))
                         (when (and props
                                    (cl-getf props 'display)
                                    ;; (or (eq 'image (car (cl-getf props 'display)))
                                    ;;     (message (car (cl-getf props 'display))))
                                    )
                           (funcall 'mb-kick-cursor old new)))))))
            (insert "\n"))))

      (mb-subsection "You can also crop images, or add a number of effects"
        (insert "\n")
        (insert-image `(image :type jpeg
                              :file ,mb-expamle-image)
                      "[you should be seeing an image]"
                      nil '(60 25 100 150))
        (insert " ")
        (insert-image `(image :type jpeg
                              :file ,mb-expamle-image
                              :conversion disabled)
                      "[you should be seeing an image]"
                      nil '(60 25 100 150))
        (insert "\n"))
      (mb-subsection "Images and text"
        (insert "\n")
        (let* (( width 50)
               ( face-spec '(:height 1.5 :inherit variable-pitch))
               ( image-data
                 (format "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"%s\" height=\"%s\">
             <circle cx=\"50%%\" cy=\"50%%\" r=\"45%%\" stroke=\"#888\" stroke-width=\"2\" fill-opacity=\"0\" />
<line x1=\"30%%\" y1=\"30%%\" x2=\"70%%\" y2=\"70%%\" stroke=\"#888\" stroke-width=\"6\"/>
<line x1=\"30%%\" y1=\"70%%\" x2=\"70%%\" y2=\"30%%\" stroke=\"#888\" stroke-width=\"6\"/>
             </svg>"
                         width width)))
          (insert (propertize "A centered " 'face face-spec)
                  (propertize " "
                              'display `(image :ascent 65
                                               :type svg
                                               :data ,image-data))
                  (propertize " inline image"
                              'face face-spec)
                  "\n\n")
          (insert (propertize "An image " 'face face-spec)
                  (propertize " "
                              'display `(image :ascent 90
                                               :type svg
                                               :data ,image-data))
                  (propertize " aligned to the bottom" 'face face-spec))))))

(mb-section "SVG"
  "More complex effects can be achieved through SVG"
  (mb-subsection "Resizing an masking"
    (insert "\n")
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
    (insert "\n\n"))
  (mb-subsection "Subjecting online images to multiplication and skewing"
    (insert "\n")
    (let ((data "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"400\" height=\"260\">
  <image id=\"image\" x=\"10\" y=\"10\" width=\"100\" height=\"45\" transform=\"skewX(10)\"
  xlink:href=\"http://www.gnu.org/graphics/behroze/behroze-gnu-button1.png\" />
  <image id=\"image2\" x=\"50\" y=\"35\" width=\"200\" height=\"90\" transform=\"skewX(-10)\"
  xlink:href=\"http://www.gnu.org/graphics/behroze/behroze-gnu-button1.png\" />
  <image id=\"image2\" x=\"50\" y=\"100\" width=\"300\" height=\"135\" transform=\"skewX(10)\"
  xlink:href=\"http://www.gnu.org/graphics/behroze/behroze-gnu-button1.png\" />
  </svg>"))
      (insert-image `(image :type svg :data ,data)))))

;; -----------------------------------------------------------------------------

;; (mb-section "Widgets"
;;   (insert-button "Click me" 'action
;;                  (lambda (event)
;;                    (message "Button clicked"))))

;; -----------------------------------------------------------------------------

;; (mb-section "Colors")

;; -----------------------------------------------------------------------------

(defun magic-buffer (&rest ignore)
  (interactive)
  (let ((buf (get-buffer-create "*magic-buffer*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (delete-all-overlays)
        (fundamental-mode)
        (progn
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq line-spacing 0)
          (setq truncate-partial-width-windows nil)
          (setq left-fringe-width 8
                right-fringe-width 8))
        (setq-local revert-buffer-function 'magic-buffer)
        (insert (propertize "Magic buffer"
                            'face 'info-title-2)
                "\n")
        (mb-comment "If you want to see the source, do `M-x find-function
magic-buffer'")
        (insert "\n")
        (cl-dolist (section (if mb--exclusive-section
                                (cl-remove-if-not
                                 (apply-partially
                                  '= mb--exclusive-section)
                                 mb-sections
                                 :key 'car)
                                (cl-sort (cl-copy-list mb-sections)
                                         '< :key 'car)))
          (cl-destructuring-bind (number name doc function) section
            (insert "\n\n")
            (insert (propertize
                     (format "%s. %s:\n" number name)
                     'face 'info-title-3))
            (if doc
                (mb-insert-filled
                 (propertize
                  (format "%s\n\n" doc)
                  'face '(:inherit (variable-pitch font-lock-comment-face))))
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
