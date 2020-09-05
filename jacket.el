;;; jacket.el --- Major mode for J  -*- lexical-binding: t -*-

(defun the-NuVoc ()
  (read (format "(progn %s)"
                (with-temp-buffer
                  (insert-file-contents "data/j.sexp")
                  (buffer-string)))))

(require 'pretty-mode)
(require 'NuVoc)
(require 'popup)
(require 'browse-url)

(defun j-find-thing (thing)
  "Find information about thing (exact match)"
  (interactive "sthing: ")
  (seq-find #'(lambda (jentity)
                (member thing (cdadr jentity)))
            j-nuvoc))

(defun j-urls (thing)
  "Look up urls related to a thing (exact match)"
  (let ((entity (j-find-thing thing)))
    (if entity
        (seq-map #'(lambda (info)
                     ;; guaranteed fields
                     (append (cdr (assoc 'description (cdr info)))
                             (cdr (assoc 'url (cdr info)))))
                 (seq-filter #'(lambda (kv)
                                 (equal (car kv) 'info))
                             (cdr entity)))
      nil)))

(defun j-names (thing)
  "Look up english names for thing"
  (seq-map #'car (j-urls thing)))

(defun joogle (thing)
  "Present a popup with links to information about thing"
  (interactive "sJOOGLE: ")
  (let ((urls (seq-map #'(lambda (url)
                           (popup-make-item (seq-elt url 0)
					    :value
					    (seq-elt url 1)))
                       (j-urls thing))))
    (when urls
      (browse-url (popup-menu* urls)))))

(defun jdocs ()
  "only works on my guix when j-docs-help addon is present"
  (interactive)
  (browse-url "~/.guix-profile/share/j/addons/docs/help/index.htm"))

; (defun jacket ()
;   "J minor mode to improve experience in emacs"
;   (interactive)
;   (add-hook 'j-mode-hook
;             (lambda ()
;               (local-set-key (kbd "C-c j") 'joogle))))

;; ;;;###autoload
;; (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . jacket))

(defvar j->apl
  '(; ("/\\.~"    . ?⌸)
    ("=\\."     . ?←)
    ("=:"       . ?←)
    ("~"        . ?⍨)
    ("\*"       . ?×))
  "Table to translate J to classic APL characters with pretty-symbols")

(defun j-psa ()
  (interactive)
  (if (null prettify-symbols-alist)
      (setq prettify-symbols-alist j->apl)
    (setq prettify-symbols-alist '())))

(defun pretty-J ()
  "Load `j->apl' table to set `prettify-symbols-alist' locally
and activate `prettify-symbols-mode'."
  (interactive)
  (j-psa)
  (prettify-symbols-mode))

(provide 'jacket)
