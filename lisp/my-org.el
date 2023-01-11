;;; my-org.el --- My Org-mode settings
;;; Commentary:
;;; Code:

(use-package sly
  :ensure t
  :commands (sly sly-connect)
  :init
  (setq sly-symbol-completion-mode nil
        sly-default-lisp 'roswell
        ros-config (concat user-emacs-directory "ros-conf.lisp")
        sly-lisp-implementations
        `((sbcl ("sbcl") :coding-system utf-8-unix)
          (abcl ("abcl") :coding-system utf-8-unix)
          (ecl ("ecl") :coding-system utf-8-unix)
          (roswell ("ros" "-Q" "-l" ,ros-config "run") :coding-system utf-8-unix)
          (qlot ("qlot" "exec" "ros" "-Q" "-l" ,ros-config "run" "-S" ".")
                :coding-system utf-8-unix))))

(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (define-pdf-cache-function pagelabels)
  (setq-default pdf-view-display-size 'fit-page)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) ((org-pdfview-open link))))))

(defun save-org-with-timestamp ()
  "Save org file with timestamp."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((section-last-update-key-re "^#\\+last_update[ ]*:[ ]*")
           (section-last-update-key "#+last_update: "))
      (if (re-search-forward section-last-update-key-re nil t)
          (kill-line)
        ;; else
        (message "No last_update key found.")
        ;; insert last_update into the first line
        (goto-char (point-min))
        (electric-newline-and-maybe-indent)
        (insert section-last-update-key))
      (timestamp))))

(defun delete-org-timestamp-hook ()
  "Delete org timestamp hook."
  (interactive)
  (remove-hook 'before-save-hook 'save-org-with-timestamp 'local))

(defun add-org-timestamp-hook ()
  "Add org timestamp hook."
  (interactive)
  (add-hook 'before-save-hook 'save-org-with-timestamp nil 'local))

(use-package org
  :pin nongnu
  :commands (org-capture org-agenda)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         :map org-mode-map ("C-M-i" . completion-at-point))
  :hook (org-mode . (lambda () (add-org-timestamp-hook)))
  :config
  (setq org-startup-indented t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))
  (add-hook 'org-mode-hook (lambda () (setq indent-tabs-mode nil)))
  (defun my-org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (org-map-entries 'org-archive-subtree "/CANCELLED" 'file)
    (org-map-entries 'org-archive-subtree "/DEFERRED" 'file)
    (org-map-entries 'org-archive-subtree "/SOMEDAY" 'file))
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t ;; Hide markup characters
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-pretty-entities t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-fontify-done-headline t
        org-fontify-todo-headline t
        org-edit-src-content-indentation 2
        org-src-preserve-indentation t
        org-hide-block-startup nil
        org-cycle-separator-lines 2
        org-hide-leading-stars t
        org-export-backends '(ascii html icalendar latex md odt)
        org-export-with-toc t
        org-highlight-latex-and-related '(native)
        org-agenda-search-view-always-boolean t
        org-agenda-timegrid-use-ampm t
        org-agenda-time-grid
        '((daily today require-timed remove-match)
          (900 930 1000 1030 1200 1230 1400 1430 1600 1630 1700 1730 1800 1830 2000 2200)
          "......." "-----------------")
        org-agenda-current-time-string "⏰ ----------------"
        org-log-done 'time
        org-log-into-drawer t
        org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)" "SOMEDAY(s)")
          (sequence "BACKLOG(b)" "ACTIVE(a)" "REVIEW(r)" "HOLD(h)" "|" "CANCELLED(c)")))

  ;; babel
  (setq org-babel-lisp-eval-fn #'sly-eval)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (awk . t)
     (C . t)
     (shell . t)
     (css . t)
     (emacs-lisp . t)
     (sed . t)
     (dot . t)
     (java . t)
     (latex . t)
     (lisp . t)
     (makefile . t)
     (js . t)
     (ocaml . t)
     (org . t)
     (python . t)
     (sass . t)
     (sql . t)
     (sqlite . t)
     ))
  (setq org-confirm-babel-evaluate nil)
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  ;; clocking
  (setq org-clock-persist 'history)
  (setq org-clock-idle-time 15)
  (setq org-clock-x11idle-program-name "xprintidle")

  ;; Refile
  (setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; font
  (let* ((my-font "Sans Serif"))
    (dolist (face '((org-document-title . 1.5)
                    (org-level-1 . 1.75)
                    (org-level-2 . 1.5)
                    (org-level-3 . 1.25)))
      (set-face-attribute (car face) nil
                          :font my-font
                          :foreground (face-foreground 'default nil 'default)
                          :inherit 'default
                          :underline nil
                          :weight 'bold
                          :height (cdr face)))))

(use-package org-timeline
  :ensure t
  :commands org-agenda
  :init (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

(use-package org-download
  :ensure t
  :bind ("C-c i" . org-download-screenshot)
  :hook ((org-mode dired-mode) . org-download-enable)
  :init
  (setq-default org-download-screenshot-method "gnome-screenshot -a -f %s")
  (setq-default org-download-image-dir "~/Pictures/org-download"))

(use-package org-modern
  :ensure t
  :commands (org-modern-mode org-modern-agenda)
  :config
  (setq org-modern-todo t
        org-modern-table nil
        org-modern-variable-pitch nil)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (global-org-modern-mode))

(use-package org-tempo
  :ensure nil
  :after org
  :config
  (let ((templates '(("sh" . "src sh")
                     ("el" . "src emacs-lisp")
                     ("vim" . "src vim")
                     ("cpp" . "src cpp :include <iostream> :namespaces std")
                     ("ml" . "src ocaml")
                     ("py" . "src python"))))
    (dolist (template templates)
      (add-to-list 'org-structure-template-alist template))))

(use-package org-noter
  :ensure t
  :after (pdf-tools)
  :init (setq org-noter-notes-search-path '("~/Documents/notes/")))

(use-package org-transclusion
  :after org
  :ensure t
  :bind ("C-c t" . org-transclusion-add))

(use-package org-protocol
  :ensure nil
  :after org)

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(provide 'my-org)
;;; my-org.el ends here
