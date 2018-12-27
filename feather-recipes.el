(require 'json)

;; silent below message.
;;   `ls does not support --dired; see ‘dired-use-ls-dired’ for more details.`
(setq dired-use-ls-dired nil)

(let ((json-object-type 'plist)
      (json-array-type  'list)
      (json-key-type    'keyword))
  (let* ((current-dir (file-name-directory
                       (directory-file-name load-file-name)))
         (read-file   (concat current-dir (nth 0 command-line-args-left)))
         (write-file  (concat current-dir (nth 1 command-line-args-left)))
         (detail-p    (and (nth 2 command-line-args-left)
                           (not (string= "nil" (nth 2 command-line-args-left)))))
         (list-p      (and (nth 3 command-line-args-left)
                           (not (string= "nil" (nth 3 command-line-args-left))))))
    (if (and (file-readable-p read-file) (file-writable-p read-file)
             (file-writable-p write-file))
        (let ((hash (make-hash-table :test 'eq))
              (obj  (json-read-file read-file))
              (key) (val) (props))
          (while obj
            (setq key (pop obj))
            (setq val (pop obj))
            (setq props (plist-get val :props))
            
            (puthash (intern
                      (replace-regexp-in-string "^:" "" (symbol-name key)))
                     (cdr
                      `(:dammy-symbol
                        :fetcher ,(plist-get val :fetcher)
                        :repo    ,(plist-get val :repo)
                        :files   ,(plist-get val :files)
                        ,@(when detail-p
                            (cdr
                             `(:dammy-symbol
                               :ver         ,(plist-get val :ver)
                               :deps        ,(plist-get val :deps)
                               :description ,(plist-get props :desc)
                               :url         ,(plist-get props :url)
                               :keywords    ,(plist-get props :keywords)
                               :authors     ,(plist-get props :authors)
                               :maintainer  ,(plist-get props :maintainer))))))
                     hash))

          (with-temp-file write-file
            (insert
             (replace-regexp-in-string "@ (:ver" "feather--@ (:ver"
                                       (prin1-to-string hash)))

            (newline)

            (goto-char (point-min))
            (search-forward "(") (search-forward "(")
            (backward-char)
            (newline) (insert (make-string 3 ?\s))
            (forward-char)            

            (condition-case err
                (while t
                  (forward-sexp)
                  (forward-sexp)
                  (newline) (insert (make-string 3 ?\s)))
              (error #'ignore))

            (goto-char (point-min))
            (while (search-forward "feather--@ (:ver" nil t)
              (replace-match "@ (:ver" nil t))

            (when list-p
              (goto-char (point-min))
              (let ((kill-whole-line t)) (kill-line))

              (goto-char (point-max))
              (search-backward ")") (delete-char 1)

              (let ((end (progn (goto-char (point-max))
                                (beginning-of-line 0)
                                (forward-char 2)
                                (point))))
                (delete-rectangle (point-min) end))
              (goto-char (point-min)) (delete-char 1) (insert "'")))

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
                (error #'ignore)))))
      (error (format "File open error, %s %s %s"
                     (file-readable-p read-file)
                     (file-writable-p read-file)
                     (file-writable-p write-file))))))
