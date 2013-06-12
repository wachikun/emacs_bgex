;; bgex.el -- Emacs-BGEX patch convenience functions -*- coding: iso-2022-jp -*-

;; Copyright (C) 2003-2013 Tadashi Watanabe <wac@umiushi.org>

;; Author: Tadashi Watanabe <wac@umiushi.org>
;; Maintainer: Tadashi Watanabe <wac@umiushi.org>
;; Version: 
;; Keywords: tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; http://umiushi.org/~wac/bgex/

;;; Code:

;;
(defvar bgex-id-default nil)
(defvar bgex-initialize-add-hook-p nil)

;; 初期化時に 必要な hook をします。
(defun bgex-initialize-add-hook ()
  (when (not bgex-initialize-add-hook-p)
    (add-hook 'dired-mode-hook
	      (lambda ()
		(define-key dired-mode-map "s" 'bgex-dired-sort-wrapper)))
    (setq bgex-initialize-add-hook-p t)))


(bgex-initialize-add-hook)


;; デフォルト背景が登録されていれば Non-nil を返します。
(defun bgex-bgexid-check-default ()
  (let ((list (bgexid-get-bgexid-list)) (i 0) (id))
    (catch 'loop
      (while (setq id (nth i list))
	(let ((ilist (bgexid-get-identifier id)))
	  (when (assq 'bgex-identifier-type-default ilist)
	    (throw 'loop t))
	  (setq i (1+ i))))
      (throw 'loop nil))))

;; 指定した背景が登録されていれば Non-nil を返します。
(defun bgex-bgexid-check (identifier type)
  (let ((list (bgexid-get-bgexid-list)) (i 0) (id))
    (catch 'loop
      (while (setq id (nth i list))
	(let ((ilist (bgexid-get-identifier id)))
	  (when (and (assq type ilist)
		     (string= (cdr (assq type ilist)) identifier))
	    (throw 'loop id))
	  (setq i (1+ i))))
      (throw 'loop nil))))

;;
(defun bgex-strict ()
  "BGEX の描画を厳密モードにします。
CPU とネットワーク帯域を犠牲にしてでも描画の厳密さを優先します。
デフォルトではこのモードになります。"
  (interactive)

  (if (string= window-system "x")
      (progn
	(bgexi-set-draw-strict)
	(redraw-display))
    (message "Emacs is not running under a X window system.")))

(defun bgex-fast ()
  "BGEX の描画を高速モードにします。
画面が若干乱れることがあります。
CPU 性能が低い場合やネットワーク越しで扱う場合などに有効です。"
  (interactive)

  (if (string= window-system "x")
      (progn
	(bgexi-set-draw-fast)
	(redraw-display))
    (message "Emacs is not running under a X window system.")))

;;
(defun bgex-enable ()
  "BGEX を有効にします。"
  (interactive)

  (if (string= window-system "x")
      (progn
	(bgexi-enable)
	(redraw-display))
    (message "Emacs is not running under a X window system.")))

(defun bgex-disable ()
  "BGEX を無効化します。"
  (interactive)

  (if (string= window-system "x")
      (progn
	(bgexi-disable)
	(redraw-display)
	(sit-for 0.1)
	(redraw-display))
    (message "Emacs is not running under a X window system.")))

;;
(defun bgex-set-image (identifier type filename &optional dynamic-color-p)
  "ウィンドウごとの背景画像を指定します。
identifier はウィンドウを識別するための文字列で、メジャーモードやバッファ名を指定します。
type には
    'bgex-identifier-type-major-mode	(identifier をメジャーモード名として扱う)
    'bgex-identifier-type-buffer-name	(identifier をバッファ名として扱う)
が指定できます。
filename には画像ファイル名を指定します。
dynamic-color-p が Non-nil ならば動的色生成モードで起動します。"
  (interactive)

  (unless (file-readable-p filename)
    (error "file \"%s\" not found." filename))

  (if (string= window-system "x")
      (progn
	(let ((getid (bgex-bgexid-check identifier type)))
	  (if getid
	      (progn
		(let ((color (bgexi-get-color getid)))
		  (bgexi-restart getid t dynamic-color-p color (expand-file-name filename)))
		(redraw-display)
		getid)
	    (let ((id (bgexid-create identifier type)))
	      (if id
		  (if (bgexi-create id t dynamic-color-p "white" (expand-file-name filename))
		      (message "bgexi-create failed.")
		    (redraw-display)
		    id)
		(error "Internal error."))))))
    (message "Emacs is not running under a X window system.")))

(defun bgex-set-color (identifier type color &optional dynamic-color-p)
  "ウィンドウごとの背景色を指定します。
identifier はウィンドウを識別するための文字列で、メジャーモードやバッファ名を指定します。
type には
    'bgex-identifier-type-major-mode	(identifier をメジャーモード名として扱う)
    'bgex-identifier-type-buffer-name	(identifier をバッファ名として扱う)
が指定できる。
color には色を指定します。色名をあらわす文字列か '(r g b) のようなリストを指定します。
r,g,b は整数で 0 - 65535 の値をとります。
dynamic-color-p が Non-nil ならば動的色生成モードで起動します。"
  (interactive)

  (if (string= window-system "x")
      (progn
	(let ((getid (bgex-bgexid-check identifier type)))
	  (if getid
	      (progn
		(let ((filename (bgexi-get-image-filename getid)))
		  (bgexi-restart getid nil dynamic-color-p color filename))
		(redraw-display)
		getid)
	    (let ((id (bgexid-create identifier type)))
	      (if id
		  (if (bgexi-create id nil dynamic-color-p color nil)
		      (message "bgexi-create failed.")
		    (redraw-display)
		    id)
		(error "Internal error."))))))
    (message "Emacs is not running under a X window system.")))

(defun bgex-destroy (identifier type)
  "指定した背景を破棄します。"
  (interactive)

  (if (string= window-system "x")
      (progn
	(let ((getid (bgex-bgexid-check identifier type)))
	  (when getid
	    (bgexid-destroy getid)
	    (redraw-display)
	    getid)))
    (message "Emacs is not running under a X window system.")))

(defun bgex-set-dynamic-color-flag (bgexid flag)
  "指定した背景の動的色生成モードフラグを設定します。
Non-nil ならば動的色生成モードになります。"
  (interactive)

  (redraw-display)
  (bgexi-set-dynamic-color-flag bgexid flag)
  (redraw-display))

(defun bgex-get-dynamic-color-flag (bgexid)
  "指定した背景の動的色生成モードフラグを得ます。"
  (interactive)

  (bgexi-get-dynamic-color-flag bgexid))

;;
(defun bgex-set-image-default (filename &optional dynamic-color-p)
  "デフォルトの背景画像を指定します。"
  (interactive)

  (unless (file-readable-p filename)
    (error "file \"%s\" not found." filename))

  (if (string= window-system "x")
      (progn
	(when (and (not (car (nth 0 (bgexid-get-identifier 0))))
		   (or (> (length (bgexid-get-bgexid-list)) 0)
		       (> (length (bgexi-get-bgexid-list)) 0)))
	  (error "Cant set default image."))
	(if (bgex-bgexid-check-default)
	    (progn
	      (let ((color (bgexi-get-color bgex-id-default)))
		(bgexi-restart bgex-id-default t dynamic-color-p color (expand-file-name filename)))
	      (redraw-display)
	      0)
	  (let ((id (bgexid-create nil 'bgex-identifier-type-default)))
	    (if id
		(if (bgexi-create id t dynamic-color-p "white" (expand-file-name filename))
		    (message "bgexi-create failed.")
		  (setq bgex-id-default id)
		  (redraw-display)
		  0)
	      (error "Internal error.")))))
    (message "Emacs is not running under a X window system.")))

(defun bgex-set-color-default (color &optional dynamic-color-p)
  "デフォルトの背景色を指定します。"
  (interactive)

  (if (string= window-system "x")
      (progn
	(when (and (not (car (nth 0 (bgexid-get-identifier 0))))
		   (or (> (length (bgexid-get-bgexid-list)) 0)
		       (> (length (bgexi-get-bgexid-list)) 0)))
	  (error "Cant set default color."))
	(if (bgex-bgexid-check-default)
	    (progn
	      (let ((filename (bgexi-get-image-filename bgex-id-default)))
		(bgexi-restart bgex-id-default nil dynamic-color-p color filename))
	      (redraw-display)
	      0)
	  (let ((id (bgexid-create nil 'bgex-identifier-type-default)))
	    (if id
		(if (bgexi-create id nil dynamic-color-p color nil)
		    (message "bgexi-create failed.")
		  (setq bgex-id-default id)
		  (redraw-display)
		  0)
	      (error "Internal error.")))))
    (message "Emacs is not running under a X window system.")))

(defun bgex-destroy-default ()
  "デフォルトの背景を破棄します。"
  (interactive)

  (if (string= window-system "x")
      (progn
	(if bgex-id-default
	    (progn
	      (bgexid-destroy bgex-id-default)
	      (setq bgex-id-default nil)
	      (redraw-display)
	      0)
	  (message "BGEX is not used.")))
    (message "Emacs is not running under a X window system.")))

(defun bgex-set-dynamic-color-flag-default (flag)
  "デフォルトの背景の動的色生成モードフラグを設定します。"
  (interactive)

  (if (string= window-system "x")
      (progn
	(redraw-display)
	(bgexi-set-dynamic-color-flag bgex-id-default flag)
	(redraw-display))
    (message "Emacs is not running under a X window system.")))

;;
(defun bgex-destroy-all ()
  "BGEX を破棄します。"
  (interactive)

  (if (string= window-system "x")
      (progn
	(let ((list (bgexid-get-bgexid-list)) (i 0) (id))
	  (while (setq id (nth i list))
	    (bgexid-destroy id)
	    (setq i (1+ i)))
	  (redraw-display)
	  i))
    (message "Emacs is not running under a X window system.")))

(defun bgex-dired-sort-wrapper (&optional arg)
  (interactive "P")
  (dired-sort-toggle-or-edit arg)
  (bgexi-redraw-window (selected-window)))

;;
(provide 'bgex)
