;; flymake-my-setting.el

;; http://d.hatena.ne.jp/sandai/20120304/p2 より設定引用
;; 下の一行はflymakeモードでエラー行に飛べるコマンドをキーに割り当ててるコードですが、
;; 個人的な理由でコメントアウトしてます。必要でしたらこのコメント削除して、アンコメントしてください
;;(define-key global-map (kbd "C-cd") 'flymake-display-err-menu-for-current-line)


;; C
;; http://d.hatena.ne.jp/nyaasan/20071216/p1
(defun flymake-c-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
					   'flymake-create-temp-inplace))
		 (local-file  (file-relative-name
					   temp-file
					   (file-name-directory buffer-file-name))))
	(list "gcc" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(add-to-list 'flymake-allowed-file-name-masks
			 '("\\.\\(c\\|h\\|y\\l\\)$" flymake-c-init))
;; C++
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
					   'flymake-create-temp-inplace))
		 (local-file  (file-relative-name
					   temp-file
					   (file-name-directory buffer-file-name))))
	(list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(add-to-list 'flymake-allowed-file-name-masks
			 '("\\.cpp$" flymake-cc-init))

;; Emacs Lisp
;; http://www.emacswiki.org/emacs/FlymakeElisp
(defun flymake-elisp-init ()
  (unless (string-match "^ " (buffer-name))
	(let* ((temp-file   (flymake-init-create-temp-buffer-copy
						 'flymake-create-temp-inplace))
		   (local-file  (file-relative-name
						 temp-file
						 (file-name-directory buffer-file-name))))
	  (list
	   (expand-file-name invocation-name invocation-directory)
	   (list
		"-Q" "--batch" "--eval"
		(prin1-to-string
		 (quote
		  (dolist (file command-line-args-left)
			(with-temp-buffer
			  (insert-file-contents file)
			  (condition-case data
				  (scan-sexps (point-min) (point-max))
				(scan-error
				 (goto-char(nth 2 data))
				 (princ (format "%s:%s: error: Unmatched bracket or quote\n"
								file (line-number-at-pos)))))))
		  )
		 )
		local-file)))))
(add-to-list 'flymake-allowed-file-name-masks
			 '("\\.el$" flymake-elisp-init))

(add-hook 'emacs-lisp-mode-hook
		  ;; workaround for (eq buffer-file-name nil)
		  (function (lambda () (if buffer-file-name (flymake-mode)))))
(add-hook 'c-mode-common-hook
		  (lambda () (flymake-mode t)))
(add-hook 'php-mode-hook
		  (lambda () (flymake-mode t)))


(provide 'flymake-my-setting)

;; end
