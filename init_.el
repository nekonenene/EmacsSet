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

(let ( (default-directory
         (file-name-as-directory (concat user-emacs-directory "elpa")))
       )
  (add-to-list 'load-path default-directory)
;;  (add-to-list 'load-path "~/.emacs.d/elpa/")
  (normal-top-level-add-subdirs-to-load-path)
  )


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)

;; テキストファイル／新規バッファの文字コード
(prefer-coding-system 'utf-8-unix)

;; ファイル名の文字コード
(set-file-name-coding-system 'utf-8-unix)

;; キーボード入力の文字コード
(set-keyboard-coding-system 'utf-8-unix)

;; サブプロセスのデフォルト文字コード
(setq default-process-coding-system '(undecided-dos . utf-8-unix))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ key binding - keyboard                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Altキーを使用せずにMetaキーを使用
;; (setq w32-alt-is-meta nil)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - input method                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; モードラインの表示文字列
(setq-default w32-ime-mode-line-state-indicator "[Aa] ")
(setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

;; IME初期化
(w32-ime-initialize)

;; デフォルトIME
(setq default-input-method "W32-IME")

;; IME変更
(global-set-key (kbd "C-\\") 'toggle-input-method)

;; 漢字/変換キー入力時のエラーメッセージ抑止
(global-set-key (kbd "<A-kanji>") 'ignore)
(global-set-key (kbd "<M-kanji>") 'ignore)
(global-set-key (kbd "<kanji>") 'ignore)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - fontset                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; デフォルト フォント
(set-face-attribute 'default nil :family "Migu 1M" :height 110)

;; プロポーショナル フォント
(set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 110)

;; 等幅フォント
(set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 110)

;; ツールチップ表示フォント
(set-face-attribute 'tooltip nil :family "Migu 1M" :height 90)

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
             ;; (width                . 85)  ; フレーム幅
             ;; (height               . 38 ) ; フレーム高
             ;; (left                 . 70 ) ; 配置左位置
             ;; (top                  . 28 ) ; 配置上位置
                (line-spacing         . 0  ) ; 文字間隔
                (left-fringe          . 10 ) ; 左フリンジ幅
                (right-fringe         . 11 ) ; 右フリンジ幅
                (menu-bar-lines       . 1  ) ; メニューバー
                (tool-bar-lines       . 1  ) ; ツールバー
                (vertical-scroll-bars . 1  ) ; スクロールバー
                (scroll-bar-width     . 14 ) ; スクロールバー幅
                (cursor-type          . box) ; カーソル種別
                (alpha                . 95) ; 透明度 
               ) default-frame-alist) )
(setq initial-frame-alist default-frame-alist)

;; フレーム タイトル
(setq frame-title-format
      '("emacs " emacs-version (buffer-file-name " - %f")))

;; 初期画面の非表示
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen nil)

;; フルスクリーン化
(global-set-key (kbd "<A-return>") 'toggle-frame-fullscreen)

;; ウィンドウ１つで起動
;; http://ameblo.jp/slont-code/entry-11718518554.html
(setq inhibit-startup-message t)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - 前回の表示サイズを記憶                               ;;;
;;; http://www.bookshelf.jp/soft/meadow_30.html#SEC419              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(defun my-window-size-save ()
  (let* ((rlist (frame-parameters (selected-frame)))
         (ilist initial-frame-alist)
         (nCHeight (frame-height))
         (nCWidth (frame-width))
         (tMargin (if (integerp (cdr (assoc 'top rlist)))
                      (cdr (assoc 'top rlist)) 0))
         (lMargin (if (integerp (cdr (assoc 'left rlist)))
                      (cdr (assoc 'left rlist)) 0))
         buf
         (file "~/.framesize.el"))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert (concat
             ;; 初期値をいじるよりも modify-frame-parameters
             ;; で変えるだけの方がいい?
             "(delete 'width initial-frame-alist)\n"
             "(delete 'height initial-frame-alist)\n"
             "(delete 'top initial-frame-alist)\n"
             "(delete 'left initial-frame-alist)\n"
             "(setq initial-frame-alist (append (list\n"
             "'(width . " (int-to-string nCWidth) ")\n"
             "'(height . " (int-to-string nCHeight) ")\n"
             "'(top . " (int-to-string tMargin) ")\n"
             "'(left . " (int-to-string lMargin) "))\n"
             "initial-frame-alist))\n"
             ;;"(setq default-frame-alist initial-frame-alist)"
             ))
    (save-buffer)
    ))

(defun my-window-size-load ()
  (let* ((file "~/.framesize.el"))
    (if (file-exists-p file)
        (load file))))

(my-window-size-load)

;; Call the function above at C-x C-c.
(defadvice save-buffers-kill-emacs
  (before save-frame-size activate)
  (my-window-size-save))


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
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; UTF-8エンコードの表記変更
(coding-system-put 'utf-8 :mnemonic ?U)
(coding-system-put 'utf-8-with-signature :mnemonic ?u)

;; 改行コードの表記追加
(setq eol-mnemonic-dos       ":Dos ")
(setq eol-mnemonic-mac       ":Mac ")
(setq eol-mnemonic-unix      ":Unx ")
(setq eol-mnemonic-undecided ":??? ") 


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

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

;; IME無効／有効時のカーソルカラー定義
(unless (facep 'cursor-ime-off)
  (make-face 'cursor-ime-off)
  (set-face-attribute 'cursor-ime-off nil
                      :background "DarkRed" :foreground "White")
  )
(unless (facep 'cursor-ime-on)
  (make-face 'cursor-ime-on)
  (set-face-attribute 'cursor-ime-on nil
                      :background "DarkGreen" :foreground "White")
  )

;; IME無効／有効時のカーソルカラー設定
(add-hook
 'input-method-inactivate-hook
 '(lambda()
    (if (facep 'cursor-ime-off)
        (let ( (fg (face-attribute 'cursor-ime-off :foreground))
               (bg (face-attribute 'cursor-ime-off :background)) )
          (set-face-attribute 'cursor nil :foreground fg :background bg)
          )
      )
    )
 )
(add-hook
 'input-method-activate-hook
 '(lambda()
    (if (facep 'cursor-ime-on)
        (let ( (fg (face-attribute 'cursor-ime-on :foreground))
               (bg (face-attribute 'cursor-ime-on :background)) )
          (set-face-attribute 'cursor nil :foreground fg :background bg)
          )
      )
    )
 )

;; バッファ切り替え時の状態引継ぎ設定
(setq w32-ime-buffer-switch-p nil)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - linum                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'linum)

;; 行移動を契機に描画
(defvar linum-line-number 0)
(declare-function linum-update-current "linum" ())
(defadvice linum-update-current
    (around linum-update-current-around activate compile)
  (unless (= linum-line-number (line-number-at-pos))
    (setq linum-line-number (line-number-at-pos))
    ad-do-it
    ))

;; バッファ中の行番号表示の遅延設定
(defvar linum-delay nil)
(setq linum-delay t)
(defadvice linum-schedule (around linum-schedule-around () activate)
  (run-with-idle-timer 1.0 nil #'linum-update-current))

;; 行番号の書式
(defvar linum-format nil)
(setq linum-format "%5d")

;; バッファ中の行番号表示
(global-linum-mode t)

;; 文字サイズ
(set-face-attribute 'linum nil :height 0.75)


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
  '(lambda() (interactive) (isearch-done)))

;; 日本語の検索文字列をミニバッファに表示
(define-key isearch-mode-map (kbd "<compend>")
  '(lambda() (interactive) (isearch-update)))
(define-key isearch-mode-map (kbd "<kanji>")
  'isearch-toggle-input-method)
(add-hook
 'isearch-mode-hook
 '(lambda() (setq w32-ime-composition-window (minibuffer-window)))
 )
(add-hook
 'isearch-mode-end-hook
 '(lambda() (setq w32-ime-composition-window nil))
 )


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - migemo                                               ;;;
;;;   https://github.com/emacs-jp/migemo                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'migemo)

(defvar migemo-command nil)
(setq migemo-command "cmigemo")

(defvar migemo-options nil)
(setq migemo-options '("-q" "--emacs"))

(defvar migemo-dictionary nil)
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

(defvar migemo-user-dictionary nil)

(defvar migemo-regex-dictionary nil)

(defvar migemo-coding-system nil)
(setq migemo-coding-system 'utf-8-unix)

(load-library "migemo")


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
      (cons (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
            backup-directory-alist))

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
      `((".*" ,(expand-file-name "/tmp/emacsbk") t)))


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

;; バッファの最後までスクロールダウン
(defadvice scroll-down (around scroll-down activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        )
    (if (< bgn-num (window-height))
        (goto-char (point-min))
      ad-do-it) ))

;; バッファの先頭までスクロールアップ
(defadvice scroll-up (around scroll-up activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        (end-num nil)
        )
    (save-excursion
      (goto-char (point-max))
      (setq end-num (+ 1 (count-lines (point-min) (point))))
      )
    (if (< (- (- end-num bgn-num) (window-height)) 0)
        (goto-char (point-max))
      ad-do-it) ))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ shell                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'shell)
(setq explicit-shell-file-name "bash.exe")
(setq shell-command-switch "-c")
(setq shell-file-name "bash.exe")

;; (M-! and M-| and compile.el)
(setq shell-file-name "bash.exe")
(modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - tabbar                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'tabbar)

;; tabbar有効化
(call-interactively 'tabbar-mode t)

;; ボタン非表示
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil) (cons "" nil)))
  )

;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
(call-interactively 'tabbar-mwheel-mode -1)
(remove-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
(remove-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)

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
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))

(global-whitespace-mode 1) ; 全角スペースを常に表示


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ saveplace - カーソル位置を記憶          追加コンテンツ        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(when (require 'saveplace nil t)
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))


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
;(setq addrev-file-name "~/.emacs.d/abbrev_defs") ;; ここ以外指定できないっぽい
;(setq save-abbrevs t)



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


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ package manager              追加コンテンツ                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; 右クリックの挙動を変更                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; マウスの右クリックの割り当て(押しながらの操作)をはずす
(if window-system (progn
		    (global-unset-key [down-mouse-3])
		    ;; マウスの右クリックメニューを使えるようにする
		    (defun bingalls-edit-menu (event)  (interactive "e")
			   (popup-menu menu-bar-edit-menu))
		    (global-set-key [mouse-3] 'bingalls-edit-menu)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; ★ 俺キーバインド     key-bind  keybind                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 元に戻すを横着する
(define-key global-map (kbd "C-z") 'undo)

;; コンパイルをF12で
(global-set-key [f12] 'compile)

;; バッファを閉じる Close Buffer
(global-set-key (kbd "C-c b") 'kill-buffer)
(global-set-key (kbd "M-RET") 'kill-buffer) ; Alt + Enter
(global-set-key [f5]          'kill-buffer)

;; タブ切り替え @tabber
(global-set-key (kbd "<C-tab>")   'tabbar-forward-tab)
(global-set-key [f6]              'tabbar-forward-tab)
;; 前のタブへ
(global-set-key (kbd "C-q")       'tabbar-backward-tab)
(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
(global-set-key [f7]              'tabbar-backward-tab)

;; F8キーで shell を開く
(global-set-key [f8] 'shell)

;; コピー (デフォの M-w も可能なまま)
(define-key global-map (kbd "C-S-w") 'copy-region-as-kill)
(define-key global-map (kbd "C-S-c") 'copy-region-as-kill)

;; 置換
(global-set-key (kbd "C-x r") 'query-replace)

;; 正規表現で置換
(global-set-key (kbd "C-x C-r") 'query-replace-regexp)

;; ハイライトする @hiwin
(global-set-key (kbd "M-s s") 'hi-lock-find-patterns)
(global-set-key (kbd "M-s l") 'highlight-lines-matching-regexp)
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

;; Alt + ; だけでなく Ctrl + ; , Ctrl + / でもコメント化
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-/") 'comment-dwim)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto-complete                追加コンテンツ                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")

;; 0.1秒後に自動で表示
(setq ac-auto-show-menu 0.1)

;; デフォルトの補完内容
(setq-default ac-sources '(
ac-source-words-in-all-buffer
ac-source-abbrev
))

;;;; ac-user-dict のインポート                   ;;;;
;;;; http://fukuyama.co/emacs-auto-complete      ;;;;

;; ユーザ辞書の場所
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  modeの設定  ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;:;;;;;;

;; lua
(add-to-list 'auto-mode-alist '("\\.lua\\'"      . lua-mode))
(add-to-list 'auto-mode-alist '("\\(.anm\\|.scn\\|.tra\\|.cam\\|.obj\\)\\'"      . lua-mode))

;; emacs-lisp
(add-to-list 'auto-mode-alist '(".el$'"      . emacs-lisp-mode))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"        . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"     . web-mode))

(add-to-list 'auto-mode-alist '("\\(.html?\\|.css\\|.php\\)\\'"
				. web-mode))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'"         . js2-mode))

;; json-mode
(add-to-list 'auto-mode-alist '("\\.json\\'"         . json-mode))

;; gitignore
(add-to-list 'auto-mode-alist '("\\.gitignore\\'"     . gitignore-mode))

;; gitconfig
(add-to-list 'auto-mode-alist '("\\.gitconfig\\'"     . gitconfig-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  @web mode   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;:;;;;;;

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
  
  (setq web-mode-ac-sources-alist
  	'(
  	  ("css"  . (;ac-source-words-in-buffer
		     ac-source-abbrev
		     ac-source-files-in-current-dir
		     ac-source-css-include3-dict
		     ac-source-css-webkit-dict  
		     ac-source-css-mozilla-dict ; prefix `x.'
		     ac-source-css-property
		     ))
  	  ("html" . (;ac-source-words-in-buffer
		     ac-source-abbrev
		     ac-source-files-in-current-dir
		     ac-source-html-sakura-dict
		     ))
  	  ("php"  . (;ac-source-words-in-buffer
		     ac-source-abbrev
		     ac-source-files-in-current-dir
		     ac-source-php-sakura-dict
		     `$.'ac-source-html-sakura-dict ;; prefix `$.' これを付けることで、phpタグの外でしかhtmlの候補が出ない
		     ))
	  ))
  
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
(unless (server-running-p)
  (server-start)
)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(tabbar-mode t nil (tabbar)))
;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ends here