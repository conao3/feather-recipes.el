(require 'json)

(let ((json-object-type 'plist)
      (json-array-type  'list)
      (json-key-type    'keyword))
  (let* ((current-dir (file-name-directory
                       (directory-file-name load-file-name)))
         (read-file   (concat current-dir (nth 0 command-line-args-left)))
         (write-file  (concat current-dir (nth 1 command-line-args-left))))
    (if (and (file-readable-p read-file)
             (file-writable-p read-file)
             (file-writable-p write-file))
        (let ((hash (make-hash-table :test 'eq :size 6000))
              (obj  (json-read-file read-file))
              (key) (val))
          (while obj
            (setq key (pop obj))
            (setq val (pop obj))

            (puthash key (cdr
                          `(:dammy-symbol
                            :ver  ,(plist-get val :ver)
                            :deps ,(plist-get val :deps)
                            :url  ,(plist-get (plist-get val :props) :url)))
                     hash))

          (with-temp-file write-file
            (insert (prin1-to-string hash))
            (newline)

            (goto-char (point-min))
            (search-forward "(") (search-forward "(")
            (backward-char)
            (newline) (insert (make-string 3 ? ))
            (forward-char)
            
            (condition-case err
                (while t
                  (forward-sexp)
                  (forward-sexp)
                  (newline) (insert (make-string 3 ? )))
              (error #'ignore)))

          (with-temp-file read-file
            (insert-file-contents read-file)
            (when (= (count-lines (point-min) (point-max)) 1)
              (goto-char (point-min))
              (forward-char)

              (condition-case err
                  (while t
                    (forward-sexp) (forward-sexp)
                    (forward-char)
                    (newline) (insert " "))
                (error #'ignore))))
          
          (princ (format "Process completed!!\nRead file:  %s\nWrite file: %s\n"
                         read-file write-file)))
      
      (error "File open error"))))
