(require 'json)

(let ((json-object-type 'plist)
      (json-array-type  'list)
      (json-key-type    'keyword))
  (let* ((current-dir (file-name-directory
                       (directory-file-name load-file-name)))
         (read-file   (concat current-dir (nth 0 command-line-args-left)))
         (write-file  (concat current-dir (nth 1 command-line-args-left)))
         (contents))
    (if (and (file-writable-p write-file)
             (file-readable-p read-file))
        (progn
          (with-temp-file write-file
            (setq contents (prin1-to-string (json-read-file read-file)))
            (insert contents)
            (insert "\n"))
          (princ (format "Process completed!!\nRead file:  %s\nWrite file: %s\n"
                         read-file write-file)))
      (error "File open error"))))
