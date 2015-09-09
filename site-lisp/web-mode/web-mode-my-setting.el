;; web-mode-my-setting.el

(setq web-mode-engines-alist
      '(
	("html"        . "\\.html?\\'")
	("css"         . "\\.css\\'")
	("javascript"  . "\\.js\\'")
	("php"         . "\\(.phtml?\\|.php\\)\\'")
	("blade"       . "\\.blade\\'")
	("abbrev"      . "\\abbrev_defs\\'")
	)
)

(add-hook 'web-mode-hook 'my-web-mode-hook)

(defun my-web-mode-hook ()
  "Hooks for Web mode."

  ;; @auto-complete のための設定
  ;;;;  html
  ;; (require 'ac-html)
  ;; (add-hook 'web-mode-hook 'ac-html-enable)
  
  ;; <> </> で囲んでいるところを光らせる
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)

  (setq web-mode-enable-auto-closing   t) ; </ と入力したら 補完してくれる
  (setq web-mode-enable-auto-opening   t)
  (setq web-mode-enable-auto-expanding t) ; d/ と入力したら<div></div>など
  (setq web-mode-enable-auto-indentation t) ; 閉じかっこ入力で自動的にインデント直す
  
  ;; CSS 色付け
  (setq web-mode-enable-css-colorization t)

  ;; Tabのサイズの設定
  (setq tab-width 2)
  (setq web-mode-markup-indent-offset 2)  ;; HTML indent  2x2/2= 2文字インデント
  (setq web-mode-css-indent-offset    4)  ;; CSS  indent  2x4/2= 4文字インデント
  (setq web-mode-code-indent-offset   4)  ;; script indent(js,php,etc..)
  
  ;; 変更日時の自動修正
  ;; (setq time-stamp-line-limit -200)
  ;; (if (not (memq 'time-stamp write-file-hooks))
  ;;     (setq write-file-hooks
  ;;           (cons 'time-stamp write-file-hooks)))
  ;; (setq time-stamp-format " %3a %3b %02d %02H:%02M:%02S %:y %Z")
  ;; (setq time-stamp-start "Last modified:")
  ;; (setq time-stamp-end "$")
  ;; htmlの内容をインデント
  ;; TEXTAREA等の中身をインデントすると副作用が起こったりするので
  ;; デフォルトではインデントしない
  ;;(setq web-mode-indent-style 2)
  ;; コメントのスタイル
  ;;   1:htmlのコメントスタイル(default)
  ;;   2:テンプレートエンジンのコメントスタイル
  ;;      (Ex. {# django comment #},{* smarty comment *},{{-- blade comment --}})
  (setq web-mode-comment-style 2)
  css,js,php,etc..の範囲をbg色で表示
  (setq web-mode-enable-block-faces t)
  (custom-set-faces
   '(web-mode-server-face
     ((t (:background "grey"))))                  ; template Blockの背景色
   '(web-mode-css-face
     ((t (:background "grey18"))))                ; CSS Blockの背景色
   '(web-mode-javascript-face
     ((t (:background "grey36"))))                ; javascript Blockの背景色
   )
  (setq web-mode-enable-heredoc-fontification t)
  )

;; 色の設定
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
 '(web-mode-doctype-face ((t (:foreground "#bbff22"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#ed7dd7"))))
 '(web-mode-html-attr-value-face ((t (:foreground "light salmon"))))
 '(web-mode-html-tag-face ((t (:foreground "light sea green" :weight bold)))))


(provide 'web-mode-my-setting)

;; end
