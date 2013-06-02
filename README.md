# magic-buffer
An executable cookbook, on how to (ab)use emacse's display engine. Feel free to add sections, or suggest improvements.

![screenshot](https://github.com/sabof/magic-buffer/raw/master/screenshot.png)

## Trans-network autoload
You can add the following snippet to your .emacs. It will download and install the latest version of magic-buffer on its firsts run.

    (defun magic-buffer ()
      (interactive)
      (let ((try-downloading
             (lambda ()
               (let (( lexical-binding t))
                 (with-current-buffer
                     (url-retrieve-synchronously
                      "https://raw.github.com/sabof/magic-buffer/master/magic-buffer.el")
                   (goto-char (point-min))
                   (search-forward "\n\n")
                   (delete-region (point-min) (point))
                   (setq lexical-binding t)
                   (eval-buffer))))))
        (condition-case nil
            (funcall try-downloading)
          (error (funcall try-downloading))))
      (magic-buffer))


then

    M-x magic-buffer
