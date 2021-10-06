;;; bgex.el --- Emacs-BGEX patch convenience functions -*- coding: iso-2022-jp -*-

;; Copyright (C) 2003-2021 Tadashi Watanabe <twacc2020@gmail.com>
;;
;; Author: Tadashi Watanabe <twacc2020@gmail.com>
;; Maintainer: Tadashi Watanabe <twacc2020@gmail.com>
;; URL: https://github.com/wachikun/emacs_bgex
;; Homepage: https://github.com/wachikun/emacs_bgex
;; Version: 0.1.0
;; Keywords: tools
;;
;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;;; Code:

;;
(defvar bgex-id-default nil)
(defvar bgex-initialize-add-hook-p nil)

;; $B=i4|2=;~$K(B $BI,MW$J(B hook $B$r$7$^$9!#(B
(defun bgex-initialize-add-hook ()
  (when (not bgex-initialize-add-hook-p)
    (add-hook 'dired-mode-hook
	      (lambda ()
		(define-key dired-mode-map "s" 'bgex-dired-sort-wrapper)))
    (setq bgex-initialize-add-hook-p t)))


(bgex-initialize-add-hook)


;; $B%G%U%)%k%HGX7J$,EPO?$5$l$F$$$l$P(B Non-nil $B$rJV$7$^$9!#(B
(defun bgex-bgexid-check-default ()
  (let ((list (bgexid-get-bgexid-list)) (i 0) (id))
    (catch 'loop
      (while (setq id (nth i list))
	(let ((ilist (bgexid-get-identifier id)))
	  (when (assq 'bgex-identifier-type-default ilist)
	    (throw 'loop t))
	  (setq i (1+ i))))
      (throw 'loop nil))))

;; $B;XDj$7$?GX7J$,EPO?$5$l$F$$$l$P(B Non-nil $B$rJV$7$^$9!#(B
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
  "BGEX $B$NIA2h$r87L)%b!<%I$K$7$^$9!#(B
CPU $B$H%M%C%H%o!<%/BS0h$r5>@7$K$7$F$G$bIA2h$N87L)$5$rM%@h$7$^$9!#(B
$B%G%U%)%k%H$G$O$3$N%b!<%I$K$J$j$^$9!#(B"
  (interactive)

  (if (string= window-system "x")
      (progn
	(bgexi-set-draw-strict)
	(redraw-display))
    (message "Emacs is not running under a X window system.")))

(defun bgex-fast ()
  "BGEX $B$NIA2h$r9bB.%b!<%I$K$7$^$9!#(B
$B2hLL$,<c43Mp$l$k$3$H$,$"$j$^$9!#(B
CPU $B@-G=$,Dc$$>l9g$d%M%C%H%o!<%/1[$7$G07$&>l9g$J$I$KM-8z$G$9!#(B"
  (interactive)

  (if (string= window-system "x")
      (progn
	(bgexi-set-draw-fast)
	(redraw-display))
    (message "Emacs is not running under a X window system.")))

;;
(defun bgex-enable ()
  "BGEX $B$rM-8z$K$7$^$9!#(B"
  (interactive)

  (if (string= window-system "x")
      (progn
	(bgexi-enable)
	(redraw-display))
    (message "Emacs is not running under a X window system.")))

(defun bgex-disable ()
  "BGEX $B$rL58z2=$7$^$9!#(B"
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
  "$B%&%#%s%I%&$4$H$NGX7J2hA|$r;XDj$7$^$9!#(B
identifier $B$O%&%#%s%I%&$r<1JL$9$k$?$a$NJ8;zNs$G!"%a%8%c!<%b!<%I$d%P%C%U%!L>$r;XDj$7$^$9!#(B
type $B$K$O(B
    'bgex-identifier-type-major-mode	(identifier $B$r%a%8%c!<%b!<%IL>$H$7$F07$&(B)
    'bgex-identifier-type-buffer-name	(identifier $B$r%P%C%U%!L>$H$7$F07$&(B)
$B$,;XDj$G$-$^$9!#(B
filename $B$K$O2hA|%U%!%$%kL>$r;XDj$7$^$9!#(B
dynamic-color-p $B$,(B Non-nil $B$J$i$PF0E*?'@8@.%b!<%I$G5/F0$7$^$9!#(B"
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
  "$B%&%#%s%I%&$4$H$NGX7J?'$r;XDj$7$^$9!#(B
identifier $B$O%&%#%s%I%&$r<1JL$9$k$?$a$NJ8;zNs$G!"%a%8%c!<%b!<%I$d%P%C%U%!L>$r;XDj$7$^$9!#(B
type $B$K$O(B
    'bgex-identifier-type-major-mode	(identifier $B$r%a%8%c!<%b!<%IL>$H$7$F07$&(B)
    'bgex-identifier-type-buffer-name	(identifier $B$r%P%C%U%!L>$H$7$F07$&(B)
$B$,;XDj$G$-$k!#(B
color $B$K$O?'$r;XDj$7$^$9!#?'L>$r$"$i$o$9J8;zNs$+(B '(r g b) $B$N$h$&$J%j%9%H$r;XDj$7$^$9!#(B
r,g,b $B$O@0?t$G(B 0 - 65535 $B$NCM$r$H$j$^$9!#(B
dynamic-color-p $B$,(B Non-nil $B$J$i$PF0E*?'@8@.%b!<%I$G5/F0$7$^$9!#(B"
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

(defun bgex-set-xpm-string (identifier type xpm-string &optional dynamic-color-p)
  "$B%&%#%s%I%&$4$H$NGX7J2hA|$r;XDj$7$^$9!#(B
identifier $B$O%&%#%s%I%&$r<1JL$9$k$?$a$NJ8;zNs$G!"%a%8%c!<%b!<%I$d%P%C%U%!L>$r;XDj$7$^$9!#(B
type $B$K$O(B
    'bgex-identifier-type-major-mode	(identifier $B$r%a%8%c!<%b!<%IL>$H$7$F07$&(B)
    'bgex-identifier-type-buffer-name	(identifier $B$r%P%C%U%!L>$H$7$F07$&(B)
$B$,;XDj$G$-$^$9!#(B
xpm-string $B$K$O(B XPM $B%$%a!<%8$r;XDj$7$^$9!#(B
dynamic-color-p $B$,(B Non-nil $B$J$i$PF0E*?'@8@.%b!<%I$G5/F0$7$^$9!#(B"
  (interactive)

  (if (string= window-system "x")
      (progn
	(let ((getid (bgex-bgexid-check identifier type)))
	  (if getid
	      (progn
		(let ((color (bgexi-get-color getid)))
		  (bgexi-restart-for-xpm-string getid t dynamic-color-p color xpm-string))
		(redraw-display)
		getid)
	    (let ((id (bgexid-create identifier type)))
	      (if id
		  (if (bgexi-create-for-xpm-string id t dynamic-color-p "white" xpm-string)
		      (message "bgexi-create failed.")
		    (redraw-display)
		    id)
		(error "Internal error."))))))
    (message "Emacs is not running under a X window system.")))

(defun bgex-destroy (identifier type)
  "$B;XDj$7$?GX7J$rGK4~$7$^$9!#(B"
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
  "$B;XDj$7$?GX7J$NF0E*?'@8@.%b!<%I%U%i%0$r@_Dj$7$^$9!#(B
Non-nil $B$J$i$PF0E*?'@8@.%b!<%I$K$J$j$^$9!#(B"
  (interactive)

  (redraw-display)
  (bgexi-set-dynamic-color-flag bgexid flag)
  (redraw-display))

(defun bgex-get-dynamic-color-flag (bgexid)
  "$B;XDj$7$?GX7J$NF0E*?'@8@.%b!<%I%U%i%0$rF@$^$9!#(B"
  (interactive)

  (bgexi-get-dynamic-color-flag bgexid))

;;
(defun bgex-set-image-default (filename &optional dynamic-color-p)
  "$B%G%U%)%k%H$NGX7J2hA|$r;XDj$7$^$9!#(B"
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
  "$B%G%U%)%k%H$NGX7J?'$r;XDj$7$^$9!#(B"
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

(defun bgex-set-xpm-string-default (xpm-string &optional dynamic-color-p)
  "$B%G%U%)%k%H$NGX7J2hA|$r;XDj$7$^$9!#(B"
  (interactive)

  (if (string= window-system "x")
      (progn
	(when (and (not (car (nth 0 (bgexid-get-identifier 0))))
		   (or (> (length (bgexid-get-bgexid-list)) 0)
		       (> (length (bgexi-get-bgexid-list)) 0)))
	  (error "Cant set default image."))
	(if (bgex-bgexid-check-default)
	    (progn
	      (let ((color (bgexi-get-color bgex-id-default)))
		(bgexi-restart-for-xpm-string bgex-id-default t dynamic-color-p color xpm-string))
	      (redraw-display)
	      0)
	  (let ((id (bgexid-create nil 'bgex-identifier-type-default)))
	    (if id
		(if (bgexi-create-for-xpm-string id t dynamic-color-p "white" xpm-string)
		    (message "bgexi-create failed.")
		  (setq bgex-id-default id)
		  (redraw-display)
		  0)
	      (error "Internal error.")))))
    (message "Emacs is not running under a X window system.")))

(defun bgex-destroy-default ()
  "$B%G%U%)%k%H$NGX7J$rGK4~$7$^$9!#(B"
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
  "$B%G%U%)%k%H$NGX7J$NF0E*?'@8@.%b!<%I%U%i%0$r@_Dj$7$^$9!#(B"
  (interactive)

  (if (string= window-system "x")
      (progn
	(redraw-display)
	(bgexi-set-dynamic-color-flag bgex-id-default flag)
	(redraw-display))
    (message "Emacs is not running under a X window system.")))

;;
(defun bgex-destroy-all ()
  "BGEX $B$rGK4~$7$^$9!#(B"
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

;;; bgex.el ends here
