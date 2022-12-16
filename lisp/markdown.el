;;; markdown.el --- Markdown settings
;;; Commentary:
;;; Code:
(defun get-leetcode-title ()
  "GET LEETCODE TITLE IN INDEX PAGE"
  (interactive)
  (save-excursion
    (goto-char (line-end-position))
    (let* ((start (search-backward "["))
           (end (search-forward "]")))
      (buffer-substring-no-properties (+ start 1) (- end 1)))))

(defun normalize-title-as-link-text (title)
  "NORMALIZE TITLE AS LINK TEXT"
  (interactive)
  (downcase
   (replace-regexp-in-string "[^a-zA-Z0-9]" "-"
                             (replace-regexp-in-string "[,'.()]" "" title))))

(defun leetcode-problem-link (problem)
  "CREATE LEETCODE PROBLEM LINK"
  (interactive)
  (format "https://leetcode.com/problems/%s/" problem))

(defun markdown-title-with-leetcode-link (title link)
  "CREATE MARKDOWN H1 TITLE WITH LEETCODE LINK"
  (interactive)
  (let* ((url (leetcode-problem-link link)))
    (format "# [%s](%s)" title url)))

(defun ready-leetcode-markdown (title link)
  "1. COPY .template.md AS link-text.md, 2. SET TITLE AND LINK. THIS MUST BE CALLED"
  (interactive)
  (let* ((mdfile (format "%s.md" link))
         (title-text (markdown-title-with-leetcode-link title link)))
    (if (file-exists-p mdfile)
        ;; if exists, just open it
        (find-file mdfile)
      ;; otherwise, create file from template and set titles
      (copy-file ".template.md" mdfile)
      (find-file mdfile)
      (goto-char (point-min))
      (re-search-forward "^title:")
      (insert " ")
      (insert title)
      (goto-char (point-min))
      (re-search-forward "^-\\{3,\\}$")
      (re-search-forward "^-\\{3,\\}$")
      (goto-char (+ (point) 1))
      (insert "\n")
      (insert title-text)
      (save-buffer))))

(defun ready-to-leetcode (&optional remain out-link)
  "GET READY TO LEETCODE!"
  (interactive)
  (save-excursion
    (xref-push-marker-stack)
    (let* ((title (get-leetcode-title))
           (link (normalize-title-as-link-text title))
           (line-text
            (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position))))
      (if (not (string-match-p link line-text))
          ;; if link not exists, just insert.
          ;; in case of inserting out link, change link as outlink
          (progn
            (goto-char (line-end-position))
            (insert (format "(%s)"
                            (if out-link (leetcode-problem-link link) link))))
        ;; otherwise, check existing link and update
        (message "link exists")
        (string-match "(.*)" line-text)
        (let* ((matched
                (substring line-text
                           (+ (match-beginning 0) 1)
                           (- (match-end 0) 1)))
               (matched
                (substring matched
                           0
                           (string-match "#" matched))))
          ;; sanitize existing link text
          (setq link matched)))

      (unless remain
        (ready-leetcode-markdown title link)))))

(defun fill-link ()
  "JUST FILL LINK"
  (interactive)
  (ready-to-leetcode t))

(defun fill-out-link ()
  "FILL OUTGOING LINK"
  (interactive)
  (ready-to-leetcode t t))

(defun timestamp ()
  "PUT TIMESTAMP"
  (interactive)
  (insert (format-time-string "%Y-%02m-%02d %02H:%02M:%02S")))

(defun save-with-timestamp ()
  "SAVE WITH TIMESTAMP"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((section-header "^-\\{3,\\}$")
           (section-last-update-key "^last_update[ ]*:[ ]*")
           (section-beginning (re-search-forward section-header))
           (section-end (re-search-forward section-header))
           (section-string (buffer-substring-no-properties section-beginning section-end)))
      (goto-char section-beginning)
      (if (string-match-p section-last-update-key section-string)
          ;; if there is already last_update section, then kill the outdated date.
          (progn
            (re-search-forward section-last-update-key)
            (kill-line))
        ;; otherwise, insert new last_update section
        (goto-char section-end)
        (previous-line)
        (goto-char (line-end-position))
        (electric-newline-and-maybe-indent)
        (insert "last_update: "))
      (timestamp))))

(defun remove-timestamp-hook ()
  "REMOVE TIMESTAMP HOOK"
  (interactive)
  (remove-hook 'before-save-hook 'save-with-timestamp 'local))

(defun add-timestamp-hook ()
  "ADD TIMESTAMP HOOK"
  (interactive)
  (add-hook 'before-save-hook 'save-with-timestamp nil 'local))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md$" . gfm-mode)
	       ("\\.md$" . markdown-mode)
	       ("\\.markdown$" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode . (lambda () (add-timestamp-hook)))
  :bind
  (:map markdown-mode-map
        ("C-c C-t C-f" . markdown-table-align)
        ("C-c C-c C-l" . fill-out-link)
        ("C-c C-c C-r" . fill-link)
        ("M-." . ready-to-leetcode)))


(provide 'markdown)
;;; markdown.el ends here
