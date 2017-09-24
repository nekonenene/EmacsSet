;;; .emacs --- dot emacs file

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this file; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ site-lisp                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(let
  (
    (default-directory
      (file-name-as-directory (concat user-emacs-directory "elpa"))
    )
  )
  (add-to-list 'load-path default-directory)
  ;; (add-to-list 'load-path "~/.emacs.d/elpa/")
  (normal-top-level-add-subdirs-to-load-path)
)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 言語指定
(set-language-environment "Japanese")

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)

;; テキストファイル／新規バッファの文字コード
(prefer-coding-system 'utf-8-unix)

;; ファイル名の文字コード
(set-file-name-coding-system 'utf-8-unix)

;; キーボード入力の文字コード
(set-keyboard-coding-system 'utf-8-unix)

;; サブプロセスのデフォルト文字コード
(setq default-process-coding-system '(utf-8-unix))

(set-default 'buffer-file-coding-system 'utf-8-unix)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - input method                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; デフォルトIME
(setq default-input-method "MacOSX")
;(setq ns-command-modifier 'super)

;; IME変更
(global-set-key (kbd "C-\\") 'toggle-input-method)

; Command キー、Option キーの定義
(setq mac-command-modifier 'super )
(setq mac-option-modifier  'meta  )


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - fontset                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; デフォルト フォント
(set-face-attribute 'default nil :family "Migu 1M" :height 132)

;; プロポーショナル フォント
(set-face-attribute 'variable-pitch nil :family "Migu 1M")

;; 等幅フォント
(set-face-attribute 'fixed-pitch nil :family "Migu 1M")

;; ツールチップ表示フォント
(set-face-attribute 'tooltip nil :family "Migu 1M" :height 120)

;;; fontset

;; フォントサイズ調整
(global-set-key (kbd "C-<wheel-up>")   '(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-=")            '(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-<wheel-down>") '(lambda() (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C--")            '(lambda() (interactive) (text-scale-decrease 1)))

;; フォントサイズ リセット
(global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - frame                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(setq default-frame-alist
  (append '(
    ;; (width                . 77 ) ; フレーム幅
    ;; (height               . 44 ) ; フレーム高
    ;; (left                 . 81 ) ; 配置左位置
    ;; (top                  .  2 ) ; 配置上位置
    (line-spacing         . 0  ) ; 文字間隔
    (left-fringe          . 10 ) ; 左フリンジ幅
    (right-fringe         . 11 ) ; 右フリンジ幅
    (menu-bar-lines       . 1  ) ; メニューバー
    (tool-bar-lines       . 1  ) ; ツールバー
    (vertical-scroll-bars . 1  ) ; スクロールバー
    (scroll-bar-width     . 14 ) ; スクロールバー幅
    (cursor-type          . box) ; カーソル種別
    (alpha                . 94) ; 透明度
  ) default-frame-alist)
)
(setq initial-frame-alist default-frame-alist)

;; フレーム タイトル
(setq frame-title-format
  '("emacs " emacs-version (buffer-file-name " - %f"))
)

;; 初期画面の非表示
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen nil)

;; フルスクリーン化
(global-set-key (kbd "<A-return>") 'toggle-frame-fullscreen)

;; ウィンドウ１つで起動
;; http://ameblo.jp/slont-code/entry-11718518554.html
(setq inhibit-startup-message t)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - 前回の表示サイズを記憶                                   ;;;
;;; http://www.bookshelf.jp/soft/meadow_30.html#SEC419              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'windowsize-save)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - mode line                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 行番号の表示
(line-number-mode t)

;; 列番号の表示
(column-number-mode t)

;; モードライン カスタマイズ
(setq-default
 mode-line-format
 `(
   ""
   w32-ime-mode-line-state-indicator
   " "
   mode-line-mule-info
   mode-line-modified
   mode-line-frame-identification
   mode-line-buffer-identification
   " "
   global-mode-string
   " %[("
   mode-name
   mode-line-process
   "%n"
   ")%] "
   (which-func-mode ("" which-func-format " "))
   (line-number-mode
    (:eval
     (format "L%%l/L%d " (count-lines (point-max) 1) )))
   (column-number-mode " C%c ")
   (-3 . "%p")
   )
 )
(setq mode-line-frame-identification " ")

;; cp932エンコードの表記変更
(coding-system-put 'cp932      :mnemonic ?P)
(coding-system-put 'cp932-dos  :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac  :mnemonic ?P)

;; UTF-8エンコードの表記変更
(coding-system-put 'utf-8 :mnemonic ?U)
(coding-system-put 'utf-8-with-signature :mnemonic ?u)

;; 改行コードの表記追加
(setq eol-mnemonic-dos       ":Win ") ; CR/LF
(setq eol-mnemonic-mac       ":CR  ")
(setq eol-mnemonic-unix      ":Unx ")
(setq eol-mnemonic-undecided ":??? ")


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines t)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - cursor                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; カーソルの点滅
(blink-cursor-mode 0)

;; 非アクティブウィンドウのカーソル表示
(setq-default cursor-in-non-selected-windows t)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - linum                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'linum)

;; バッファ中の行番号表示の遅延設定
;; (defvar linum-delay nil)
;; (setq linum-delay t)
;; (defadvice linum-schedule (around linum-schedule-around () activate)
;;   (run-with-idle-timer 1.0 nil #'linum-update-current))

;; 行番号の書式
(defvar linum-format nil)
(setq linum-format "%5d")

;; バッファ中の行番号表示
(global-linum-mode t)

;; 文字サイズ
(set-face-attribute 'linum nil :height 0.75)

;;;;;; 以降、個人設定
;; 文字を折り返さない t =yes, nil =no
;; (setq-default truncate-lines t)

;; 折り返す
(setq-default word-wrap t)

;; 改行時にインデント箇所から始める
(setq-default newline-and-indent t)

;; auto-indentation 関連の初期設定
(defun insert-tab-char ()
  "insert a tab char. (ASCII 9, \t)"
  (interactive)
  ;; (insert "\t")
  (insert " ")
  )
(setq-default indent-tabs-mode t)  ;; ソフトタブなら nil, ハードタブなら t
(setq-default tab-width 4)

;; カッコの対応関係を光らせる
(show-paren-mode t)
(setq-default show-paren-delay 0.8)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - hiwin                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 非アクティブなウィンドウの色を薄くする
;; http://d.hatena.ne.jp/ksugita0510/20111223/p1

(require 'hiwin)

;; hiwin-modeを有効化
(hiwin-activate)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 大文字・小文字を区別しないでサーチ
(setq-default case-fold-search nil)

;; インクリメント検索時に縦スクロールを有効化
(setq isearch-allow-scroll nil)

;; C-dで検索文字列を一文字削除
(define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)

;; C-yで検索文字列にヤンク貼り付け
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)

;; C-eで検索文字列を編集
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)

;; Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;; C-gで検索を終了
(define-key isearch-mode-map (kbd "C-g")
  '(lambda() (interactive) (isearch-done))
)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - hi-lock（ハイライト）を有効に                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy t)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - backup                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ファイルオープン時のバックアップ（~）
(setq make-backup-files   t)  ;; 自動バックアップの実行有無
(setq version-control     t)  ;; バックアップファイルへの番号付与
(setq kept-new-versions   3)  ;; 最新バックアップファイルの保持数
(setq kept-old-versions   0)  ;; 最古バックアップファイルの保持数
(setq delete-old-versions t)  ;; バックアップファイル削除の実行有無

;; ファイルオープン時のバックアップ（~）の格納ディレクトリ
(setq backup-directory-alist
  (cons
    (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
  backup-directory-alist)
)

;; 編集中ファイルの自動バックアップ
(setq backup-inhibited nil)

;; 終了時に自動バックアップファイルを削除
(setq delete-auto-save-files nil)

;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 3)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 100)

;; 編集中ファイル（##）の格納ディレクトリ
(setq auto-save-file-name-transforms
  `((".*" ,(expand-file-name "/tmp/emacsbk") t))
)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - lockfile                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ロックファイルの生成を抑止
(setq create-lockfiles nil)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ scroll                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; スクロール時のカーソル位置を維持
(setq scroll-preserve-screen-position t)

;; スクロール開始の残り行数
(setq scroll-margin 0)

;; スクロール時の行数
(setq scroll-conservatively 10000)

;; スクロール時の行数（scroll-marginに影響せず）
(setq scroll-step 0)

;; 画面スクロール時の重複表示する行数
(setq next-screen-context-lines 1)

;; キー入力中の画面更新を抑止
(setq redisplay-dont-pause t)

;; recenter-top-bottomのポジション
(setq recenter-positions '(top bottom))

;; 横スクロール開始の残り列数
(setq hscroll-margin 1)

;; 横スクロール時の列数
(setq hscroll-step 1)

;; スクロールダウン
;;(global-set-key (kbd "C-z") 'scroll-down)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; 右クリックの挙動を変更                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; マウスの右クリックの割り当て(押しながらの操作)をはずす
(if window-system (progn
  (global-unset-key [down-mouse-3])
  ;; マウスの右クリックメニューを使えるようにする
  (defun bingalls-edit-menu (event)  (interactive "e")
    (popup-menu menu-bar-edit-menu))
  (global-set-key [mouse-3] 'bingalls-edit-menu)
))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ shell                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'shell)
(setq explicit-shell-file-name "/bin/zsh")
(setq shell-command-switch "-c")

;; (M-! and M-| and compile.el)
(setq shell-file-name "/bin/zsh")
(modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ flycheck                         追加コンテンツ               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; flymake より 導入が手軽な flycheck を使う
;; 注意： jshint はインストールが必要。だが、Cygwin版 Emacsでは PATH の関係で動かない
(require 'flycheck)
; (add-hook 'after-init-hook #'global-flycheck-mode)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - tabbar                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'tabbar)

;; tabbar有効化
(call-interactively 'tabbar-mode t)

;; ボタン非表示
(dolist
  (btn '(
    tabbar-buffer-home-button
    tabbar-scroll-left-button
    tabbar-scroll-right-button
  ))
  (set btn (cons (cons "" nil) (cons "" nil)))
)

;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
;; (call-interactively 'tabbar-mwheel-mode -1)
;; (remove-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
;; (remove-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)

;; タブグループを使用（t：有効，nil：無効）
(defvar tabbar-buffer-groups-function nil)
(setq tabbar-buffer-groups-function nil)

;; タブの表示間隔
(defvar tabbar-separator nil)
(setq tabbar-separator '(1.0))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ whitespace - 全角スペース、Tabを表示        追加コンテンツ    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'whitespace)

;; 全角スペースの色		theme.el - whitespace-space 内でも指定されている
(set-face-foreground 'whitespace-space "#bf4646")
(set-face-background 'whitespace-space nil)
(set-face-bold-p 'whitespace-space t)
;; Tabの色		theme.el - whitespace-tab 内でも指定されている
;(set-face-foreground 'whitespace-tab "DarkOliveGreen1")
;(set-face-background 'whitespace-tab nil)
;(set-face-underline  'whitespace-tab t)

;; 全角スペースとTabのみ強調（デフォだと半角スペースや改行を強調したりする）
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings '(
  (space-mark ?\x3000 [?\□])
  (tab-mark   ?\t   [?\xBB ?\t])
))

(global-whitespace-mode 1) ; 全角スペースを常に表示


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ saveplace - カーソル位置を記憶                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'saveplace)
(save-place-mode 1)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ daddrev-ex                              追加コンテンツ        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; daddrev : M - / で実行。バッファ内の単語を補完
;; auto-complete の簡易版みたいな、Emacs標準機能

(load "dabbrev-ja")  ; 日本語の補完を拾いすぎないように改善
(require 'dabbrev-highlight)  ; どこから補完してきたのか、単語をハイライト


;;;;;;;;;;;;;;;;;;;;;;;;
;;;; addrev の設定  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
; (setq addrev-file-name "~/.emacs.d/abbrev_defs") ;; ここ以外指定できないっぽい
(setq save-abbrevs t)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ recentf-ext - 最近開いたファイルを表示       追加コンテンツ   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'recentf-ext)
(setq recentf-max-saved-items 100) ; 100個まで履歴として保存


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rainbow-mode - #ffffff 等を色づけ            追加コンテンツ   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'rainbow-mode)

(add-hook 'text-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ package manager                 追加コンテンツ                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ visual-regexp-steroids              追加コンテンツ            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'visual-regexp-steroids)

(setq vr/engine 'python) ; 'python, 'pcre2el, or 'emacs
;; python が インストールされてない環境では、上１行をコメントアウト、下２行をコメント解除
;; (setq vr/engine 'pcre2el)
;; (require 'pcre2el)

;; multiple-cursors ( https://github.com/magnars/multiple-cursors.el ) を使っている場合は下１行をコメント解除
; (global-set-key (kbd "C-c m") 'vr/mc-mark)

;; 普段の 'query-replace-regexp を visual-regexp に
(global-set-key (kbd "C-x C-r") 'vr/query-replace)
(global-set-key (kbd "C-S-x C-S-r") 'vr/replace)
(global-set-key (kbd "C-S-s") 'vr/isearch-forward)
(global-set-key (kbd "C-S-r") 'vr/isearch-backward)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; ★ 俺キーバインド     key-bind  keybind                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Command キーらしい動きをするように設定
(global-set-key (kbd "s-r")   'query-replace)
(global-set-key (kbd "s-o")   'recentf-open-files)
(global-set-key (kbd "s-;")   'comment-dwim)
(global-set-key (kbd "s-w")   'kill-buffer)
(global-set-key (kbd "s-f")   'find-file)
(global-set-key (kbd "s-s")   'save-buffer)
(global-set-key (kbd "s-S-s") 'write-file)
(global-set-key (kbd "s-w")   'kill-buffer)
(global-set-key (kbd "s-z")   'advertised-undo)
(global-set-key (kbd "s-x")   'kill-region)
(global-set-key (kbd "s-c")   'copy-region-as-kill)
(global-set-key (kbd "s-v")   'yank)
(global-set-key (kbd "s-S-v") 'yank-pop)
(global-set-key (kbd "s-a")   'mark-whole-buffer)
(global-set-key (kbd "s-p")   'print-buffer)
(global-set-key (kbd "s-q")   'save-buffers-kill-emacs)
(global-set-key (kbd "s-S-q") 'kill-emacs)

(global-set-key (kbd "<s-right>") 'end-of-line)
(global-set-key (kbd "<s-left>")  'beginning-of-line)
(global-set-key (kbd "<s-down>")  'scroll-up)
(global-set-key (kbd "<s-up>")    'scroll-down)


;; 元に戻すを横着する
(define-key global-map (kbd "C-z") 'undo)

;; コンパイルをF12で
(global-set-key [f12] 'compile)

;; タブ切り替え @tabber
(global-set-key (kbd "<C-tab>")   'tabbar-forward-tab)
(global-set-key [f6]              'tabbar-forward-tab)
;; 前のタブへ
(global-set-key (kbd "C-q")       'tabbar-backward-tab)
(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
(global-set-key [f7]              'tabbar-backward-tab)

;; バッファを閉じる Close Buffer
(global-set-key (kbd "C-c b") 'kill-buffer)
(global-set-key (kbd "M-RET") 'kill-buffer) ; Alt + Enter
(global-set-key [f8]          'kill-buffer)

;; コピー (デフォの M-w も可能なまま)
(define-key global-map (kbd "C-S-w") 'copy-region-as-kill)
(define-key global-map (kbd "C-S-c") 'copy-region-as-kill)

;; 置換
(global-set-key (kbd "C-x r") 'query-replace)

;; 正規表現で置換
;; (global-set-key (kbd "C-x C-r") 'query-replace-regexp)
;; visual-regexp を使うのでコメントアウトした

;; ハイライトする @hiwin
(global-set-key (kbd "M-s s")   'hi-lock-find-patterns)
(global-set-key (kbd "M-s l")   'highlight-lines-matching-regexp)
(global-set-key (kbd "M-s M-s") 'highlight-regexp)
(global-set-key (kbd "M-s d")   'unhighlight-regexp)  ;;ハイライト解除
(global-set-key (kbd "M-s M-d") 'unhighlight-regexp)  ;;ハイライト解除

;; 全角スペース表示の切替    @whitespace
(global-set-key (kbd "C-x w") 'global-whitespace-mode)

;; Alt + Space で候補を表示  @auto-complete
(global-set-key (kbd "M-SPC") 'auto-complete)

;; 最近開いたファイルを表示  @recentf-ext
(global-set-key (kbd "C-x C-o") 'recentf-open-files)
(global-set-key (kbd "C-x o")   'recentf-open-files)

;; コメント化
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-/") 'comment-dwim)

;; 横（水平）に2画面
(global-set-key (kbd "C-x C-2") 'split-window-right)

;; バッファーを読み込みなおす
(global-set-key [f5] 'revert-buffer)

;; shell を開く
(global-set-key (kbd "M-s h") 'shell)

;; diff を開始  C-d のデフォルトは一文字削除
(global-set-key (kbd "C-d") 'ediff-buffers)

;; Ctrl + Alt + i で More Indent （本来は半自動インデントキーバインド）
(global-set-key (kbd "C-M-i") 'increase-left-margin)
(global-set-key (kbd "C-S-i") 'decrease-left-margin)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto-complete                追加コンテンツ                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")

;; 0秒後に自動で表示
(setq ac-delay 0.01)
(setq ac-auto-show-menu 0.01)
;; クイックヘルプ表示
(setq ac-quick-help-delay 0.1)

;; 1文字目を入力したら補完開始
(setq ac-auto-start 1)

;; default color は theme 内に記述されている

;; デフォルトの補完内容
(setq-default ac-sources '(
  ac-source-words-in-buffer ;; 現在のバッファの内容
  ac-source-abbrev
  ac-source-words-in-same-mode-buffers
))

;;;; ac-user-dict のインポート              ;;;;
;;;; http://fukuyama.co/emacs-auto-complete ;;;;

(require 'ac-user-dict-import)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  modeの設定  ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;:;;;;;;;
;; 拡張子の末尾は "$" or "\\"  \\ がベター
;; 参考 : http://syohex.hatenablog.com/entry/20140910/1410356727

;; .h は c-mode でなく c++-mode で
(add-to-list 'auto-mode-alist '("\\.h\\'"   . c++-mode))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"        . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"     . web-mode))

(add-to-list 'auto-mode-alist '("\\(.html?\\|.css\\|.php\\)\\'"
                                . web-mode))

(require 'web-mode-my-setting)

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\(.js\\|.es6\\)\\'" . js2-mode))

;; Windows Power Shell mode
(require 'powershell)
(add-hook 'powershell-mode-hook '(lambda()
  (auto-complete-mode t)
))

;; lua
(add-to-list 'auto-mode-alist '("\\(.anm\\|.scn\\|.tra\\|.cam\\|.obj\\)\\'" . lua-mode))

;; sh - bash, zsh
;; (require 'sh-mode)
(add-to-list 'auto-mode-alist '("\\(.?(bash|zsh)rc\\|.zprofile\\)\\'" . sh-mode))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ theme                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; テーマ格納ディレクトリのパス追加
(add-to-list 'custom-theme-load-path
  (file-name-as-directory (concat user-emacs-directory "theme"))
)

;; テーマ選択
;; (load-theme 'solarized-light t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'gnupack-dark t)
;; (load-theme 'tsdh-dark t)
(load-theme 'tsdh-dark-kai t)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ server                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; emacs-server起動
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)
(setq server-socket-dir "~/.emacs.d")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(ediff-window-setup-function (quote ediff-setup-windows-plain))
  '(package-selected-packages
    (quote
      (kotlin-mode editorconfig flycheck lua-mode pcre2el csv-mode gitconfig-mode gitignore-mode hiwin json-mode ac-html ac-html-csswatcher ac-js2 tabbar rainbow-mode recentf-ext popup popup-complete powershell web-completion-data web-mode visual-regexp visual-regexp-steroids)
    )
  )
  '(tabbar-mode t nil (tabbar))
)
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
  '(web-mode-html-tag-face ((t (:foreground "light sea green" :weight bold))))
)
;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ends here
