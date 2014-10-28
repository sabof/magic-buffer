# magic-buffer
An executable cookbook, on how to (ab)use emacse's display engine. Feel free to add sections, or suggest improvements.

![screenshot](https://github.com/sabof/magic-buffer/raw/master/screenshot.png)

## Trans-network autoload
You can add the following snippet to your .emacs. It will download and install the latest version of magic-buffer on its firsts run.

    (defun magic-buffer ()
      (interactive)
      (let (( file-name
              (concat temporary-file-directory
                      "magic-buffer.el"))
            ( try-downloading
              (lambda ()
                (url-copy-file
                 "https://raw.githubusercontent.com/sabof/magic-buffer/master/magic-buffer.el"
                 file-name t)
                (require 'magic-buffer file-name))))
        (condition-case nil
            (funcall try-downloading)
          (error (funcall try-downloading))))
      (magic-buffer))

then

    M-x magic-buffer
