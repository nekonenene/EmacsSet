;; ac-user-dict-import.el

;;;; ac-user-dict のインポート                   ;;;;
;;;; http://fukuyama.co/emacs-auto-complete      ;;;;


;; ユーザ辞書の場所を指定
(defvar ac-user-dict-dir (expand-file-name "~/.emacs.d/ac-user-dict/"))

;; コンプリート時の動作 - 候補の末尾に()があればその内にカーソルを置く
(defun ac-go-into-braces-action ()
  (save-restriction
    (narrow-to-region (point) (- (point) 2))
    (if (re-search-backward "()" nil t)
        (forward-char))))

;; 直前の文字を区別して辞書を使用する - 何か1文字+ドットの後の場合に補完する
(defun ac-js-dot-prefix ()
  "`x.' prefix."
  (if (re-search-backward ".\\.\\(.*\\)" nil t)
      ;; ".\\.\\(.*\\)" 何か一文字[.] ドット[\\.] 補完開始点[\\(.*\\)]
      (match-beginning 1)))

;; メニューで選択中の候補の色設定
(defface ac-my-selection-face
  '((t (:background "#000080" :foreground "#ffffff")))
  "Face for selectied candidates."
  :group 'auto-complete)

;;; 辞書1 (underscore.js)
;; 色設定
(defface ac-underscore-js-candidate-face
  '((t (:background "#730CE8" :foreground "#eeeeee")))
  "Face for underscore.js candidates."
  :group 'auto-complete)
;; 情報源に辞書ファイルを指定
(defvar ac-underscore-js-cache
  (ac-file-dictionary (concat ac-user-dict-dir "underscore-js")))
;; 辞書1の設定
(defvar ac-source-underscore-js-dict
  '((candidates . ac-underscore-js-cache) ;; 候補の情報源 これ以下はオプション
    (candidate-face . ac-underscore-js-candidate-face) ;; 候補の色設定
    (selection-face . ac-my-selection-face) ;; 選択中の色設定
    (prefix . ac-js-dot-prefix) ;; 直前の文字の条件
    (action . ac-go-into-braces-action) ;; 補完後の動作
    (symbol . "underscore.js") ;; ライブラリ名 (無理矢理。本来の意図とは違うはず)
    ;; (requires . 2) ;; 補完が開始される最低入力文字数を上書き
    ;; (limit . 30) ;; 候補を一度に表示する上限数を上書き
    ))

;;; 辞書2 (jquery)
;; 色設定
(defface ac-jquery-candidate-face
  '((t (:background "#1f679a" :foreground "#eeeeee")))
  "Face for jquery candidates."
  :group 'auto-complete)
;; 情報源に辞書ファイルを指定
(defvar ac-jquery-method1-cache
  (ac-file-dictionary (concat ac-user-dict-dir "jquery-method1")))
;; 辞書2の設定
(defvar ac-source-jquery-method-dict1
  '((candidates . ac-jquery-method1-cache)
    (candidate-face . ac-jquery-candidate-face)
    (selection-face . ac-my-selection-face)
    (prefix . ac-js-dot-prefix)
    (action . ac-go-into-braces-action)
    (symbol . "jquery method1")
    ))

;;; 辞書3 (jquery)
;; 直前の文字の条件 (`jQuery.'または`$.')
(defun ac-jquery-method2-prefix ()
  "`$' or `jQuery' prefix."
  (if (re-search-backward "\\(jQuery\\|\\$\\)\\.\\(.*\\)" nil t)
      (match-beginning 2)))
;; 情報源に辞書ファイルを指定
(defvar ac-jquery-method2-cache
  (ac-file-dictionary (concat ac-user-dict-dir "jquery-method2")))
;; 辞書3の設定
(defvar ac-source-jquery-method-dict2
  '((candidates . ac-jquery-method2-cache)
    (candidate-face . ac-jquery-candidate-face)
    (selection-face . ac-my-selection-face)
    (prefix . ac-jquery-method2-prefix)
    (action . ac-go-into-braces-action)
    (symbol . "jquery method2")
    ))

;;; 辞書4 (jqueryセレクタ)
;; 直前の文字の条件 (`x:')
(defun ac-jquery-selector-prefix ()
  "`x:' prefix."
  (if (re-search-backward ".\\:\\(.*\\)" nil t)
      (match-beginning 1)))
;; 色設定
(defface ac-jquery-selector-candidate-face
  '((t (:background "#1B919A" :foreground "#eeeeee")))
  "Face for jquery selector candidates."
  :group 'auto-complete)
;; 情報源に辞書ファイルを指定
(defvar ac-jquery-selector-cache
  (ac-file-dictionary (concat ac-user-dict-dir "jquery-selector")))
;; 辞書4の設定
(defvar ac-source-jquery-selector-dict
  '((candidates . ac-jquery-selector-cache)
    (candidate-face . ac-jquery-selector-candidate-face)
    (selection-face . ac-my-selection-face)
    (prefix . ac-jquery-selector-prefix)
    (action . ac-go-into-braces-action)
    (symbol . "jQuery selector")
    ))

;; 使用する辞書・情報源を選択
(defun ac-js-mode-setup ()
  (setq ac-sources
        '(
          ac-source-abbrev
          ac-source-words-in-same-mode-buffers
          ;; ac-source-yasnippet
          ac-source-filename
          ;; 優先順位で並べる (prefixを指定すると排他的になる; x.に$.が含まれる)

	  ;; とりあえず jQuery 今は使わないのでコメントアウト
	  ;; ac-source-jquery-method-dict2  ; prefix `$.'
          ;; ac-source-jquery-method-dict1  ; prefix `x.'
          ;; ac-source-underscore-js-dict   ; prefix `x.'
          ;; ac-source-jquery-selector-dict ; prefix `x:'
          )))
;; メジャーモードに反映させる
(add-hook 'js-mode-hook 'ac-js-mode-setup)
(add-hook 'js2-mode-hook 'ac-js-mode-setup)

;; 辞書間で重複するものが削除されるので
;; ~/.emacs.d/elisp/auto-complete/auto-complete.el L1052 (delete-dups candidates) をコメントアウト

;; ac-disable-facesの初期値は
;; (font-lock-comment-face font-lock-string-face font-lock-doc-face)
;; font-lock-string-faceがあるとクオートで囲まれた部分"..."で
;; auto-completeが反応しなくなり、セレクタを補完できないので次のように
(setq ac-disable-faces '(font-lock-comment-face font-lock-doc-face))


;;;;  HTML, CSS, PHP のための auto-complete ;;;;

;;; 辞書1 (css-include3)
;; 色設定
(defface ac-css-include3-candidate-face
  '((t (:background "#D12EA6" :foreground "#260D0A")))
  "Face for css-include3 candidates."
  :group 'auto-complete)
;; 情報源に辞書ファイルを指定
(defvar ac-css-include3-cache
  (ac-file-dictionary (concat ac-user-dict-dir "css-include3")))
;; 辞書1の設定
(defvar ac-source-css-include3-dict
  '((candidates . ac-css-include3-cache) ;; 候補の情報源 これ以下はオプション
    (candidate-face . ac-css-include3-candidate-face) ;; 候補の色設定
    (selection-face . ac-my-selection-face) ;; 選択中の色設定
    (action . ac-go-into-braces-action) ;; 補完後の動作
    (symbol . "CSS") ;; ライブラリ名 (無理矢理。本来の意図とは違うはず)
    ))

;;; 辞書2 (css-webkit)
;; 色設定
(defface ac-css-webkit-candidate-face
  '((t (:background "#FF5445" :foreground "#260D0A")))
  "Face for css-webkit candidates."
  :group 'auto-complete)
;; 情報源に辞書ファイルを指定
(defvar ac-css-webkit-cache
  (ac-file-dictionary (concat ac-user-dict-dir "css-webkit")))
;; 辞書1の設定
(defvar ac-source-css-webkit-dict
  '((candidates . ac-css-webkit-cache) ;; 候補の情報源 これ以下はオプション
    (candidate-face . ac-css-webkit-candidate-face) ;; 候補の色設定
    (selection-face . ac-my-selection-face) ;; 選択中の色設定
    (action . ac-go-into-braces-action) ;; 補完後の動作
    (symbol . "CSS-webkit") ;; ライブラリ名 (無理矢理。本来の意図とは違うはず)
    ))

;;; 辞書3 (css-mozilla)
;; 色設定
(defface ac-css-mozilla-candidate-face
  '((t (:background "#FF682C" :foreground "#260D0A")))
  "Face for css-mozilla candidates."
  :group 'auto-complete)
;; 情報源に辞書ファイルを指定
(defvar ac-css-mozilla-cache
  (ac-file-dictionary (concat ac-user-dict-dir "css-mozilla")))
;; 辞書1の設定
(defvar ac-source-css-mozilla-dict
  '((candidates . ac-css-mozilla-cache) ;; 候補の情報源 これ以下はオプション
    (candidate-face . ac-css-mozilla-candidate-face) ;; 候補の色設定
    (selection-face . ac-my-selection-face) ;; 選択中の色設定
    (action . ac-go-into-braces-action) ;; 補完後の動作
    (symbol . "CSS-moz") ;; ライブラリ名 (無理矢理。本来の意図とは違うはず)
    ))


;;; 辞書4 (HTML)
;; 色設定
(defface ac-html-sakura-candidate-face
  '((t (:background "#deaa9e" :foreground "#220")))
  "Face for html-sakura candidates."
  :group 'auto-complete)
;; 情報源に辞書ファイルを指定
(defvar ac-html-sakura-cache
  (ac-file-dictionary (concat ac-user-dict-dir "html-sakura")))
;; 辞書1の設定
(defvar ac-source-html-sakura-dict
  '((candidates . ac-html-sakura-cache) ;; 候補の情報源 これ以下はオプション
    (candidate-face . ac-html-sakura-candidate-face) ;; 候補の色設定
    (selection-face . ac-my-selection-face) ;; 選択中の色設定
    (action . ac-go-into-braces-action) ;; 補完後の動作
    (symbol . "HTML") ;; ライブラリ名 (無理矢理。本来の意図とは違うはず)
    ))


;;; 辞書5 (PHP)
;; 色設定
(defface ac-php-sakura-candidate-face
  '((t (:background "#736eb3" :foreground "#220")))
  "Face for php-sakura candidates."
  :group 'auto-complete)
;; 情報源に辞書ファイルを指定
(defvar ac-php-sakura-cache
  (ac-file-dictionary (concat ac-user-dict-dir "php-sakura")))
;; 辞書1の設定
(defvar ac-source-php-sakura-dict
  '((candidates . ac-php-sakura-cache) ;; 候補の情報源 これ以下はオプション
    (candidate-face . ac-php-sakura-candidate-face) ;; 候補の色設定
    (selection-face . ac-my-selection-face) ;; 選択中の色設定
    (action . ac-go-into-braces-action) ;; 補完後の動作
    (symbol . "PHP") ;; ライブラリ名 (無理矢理。本来の意図とは違うはず)
    ))



(provide 'ac-user-dict-import)

;; end
