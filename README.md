# magic-buffer
An executable cookbook, on how to (ab)use emacse's display engine. Feel free to add sections, or suggest improvements.

## Trans-network autoload
You can add the following snippet to your .emacs. It will download and install the latest version of magic-buffer on it's firsts run.

    (defun magic-buffer ()
      (interactive)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.github.com/sabof/magic-buffer/master/magic-buffer.el")
        (goto-char (point-min))
        (search-forward "\n\n")
        (delete-region (point-min) (point))
        (setq lexical-binding t)
        (eval-buffer))
      (magic-buffer))

then

    M-x magic-buffer
