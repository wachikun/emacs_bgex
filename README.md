# Emacs-BGEX patch

The `Emacs-BGEX patch' is an unofficial patch of the FSF Emacs background extension for X.

If you apply the Emacs-BGEX patch, you can specify an image or color for the background.

You can also specify a different background for each buffer instead of per frame.

However, the background rendering is not complete because it is implemented forcibly.

I've been maintaining it for over 10 years now.




# Install

```
get patch-bgex_VERSION.tar.xz
$ cd /tmp
$ tar xf patch-bgex_VERSION.tar.xz

$ cp -av /tmp/patch-bgex_VERSION/bgex.el YOUR-ELISP-PATH/

$ cd YOUR-EMACS-SOURCE-PATH
$ cd src
$ patch < /tmp/patch-bgex_VERSION/patch-bgex_VERSION
$ cd ..
$ ./configure --prefix=YOUR-INSTALL-PATH && make -j15
```


## Settings

```
;; Emacs-BGEX patch
(require 'bgex)


;; Image on frame
(when (boundp 'bgex-exist-p)
  (bgex-set-image-default "~/.emacs.d/images/background.xpm"))

;; Image on frame (dynamic color mode (SRC * DST / factor))
(bgex-set-image-default "~/.emacs.d/images/background.xpm" t)

;; Color for HTML-mode (dynamic color mode)
(bgex-set-color "HTML" 'bgex-identifier-type-major-mode '(60000 40000 40000) t)

;; Color for buffer-name (*scratch*)
(bgex-set-color "*scratch*" 'bgex-identifier-type-buffer-name "skyblue")

;; XPM string
(bgex-set-xpm-string "*scratch*" 'bgex-identifier-type-buffer-name "XPM string" t)
(bgex-set-xpm-string-default "XPM string" t)
```




# Patches


## HEAD

- [patch-bgex_20211121_a-git-emacs-master](patch-bgex_20211121_a-git-emacs-master)


## for macOS (simple version)

- [ns_bgex.20211009_a.patch](ns_bgex.20211009_a.patch)

```elisp
(set-background-color "#d0b8a0")
(set-frame-parameter nil 'bg-image-enable-flag t)
(set-frame-parameter nil 'bg-fill-alpha 0.6)
(set-frame-parameter nil 'bg-image-filename "IMAGE_PATH")
```

## Emacs-27.1 / Emacs-27.2

- [patch-bgex_20200403_0-git-emacs-master](patch-bgex_20200403_0-git-emacs-master)


## Emacs-26.1 / Emacs-26.2 / Emacs-26.3

- [patch-bgex_20171115_0-git-emacs-master](patch-bgex_20171115_0-git-emacs-master)


## Emacs-25.1 / 25.2 / 25.3

- [patch-bgex_20160201_0-git-emacs-master](patch-bgex_20160201_0-git-emacs-master)


## Emacs-24.5

- [patch-bgex_20150515_0-emacs-24.5](patch-bgex_20150515_0-emacs-24.5)


## Emacs-24.4

- [patch-bgex_20140112_0-bzr-emacs-trunk](patch-bgex_20140112_0-bzr-emacs-trunk)


## Emacs-24.3

- [patch-bgex_20121216_0-bzr-emacs-trunk](patch-bgex_20121216_0-bzr-emacs-trunk)


## Emacs-24.2

- [patch-bgex_20120916_0-emacs-24.2](patch-bgex_20120916_0-emacs-24.2)


## Emacs-24.1

- [patch-bgex_20111215_0-bzr-emacs-trunk](patch-bgex_20111215_0-bzr-emacs-trunk)


## Emacs-23.3a

- [patch-bgex_20110822_0-emacs-23.3a](patch-bgex_20110822_0-emacs-23.3a)


## Emacs-22.3

- [patch-bgex_20060712_0-emacs-cvshead](patch-bgex_20060712_0-emacs-cvshead)


## Emacs-21.4 / Emacs-21.3

- [patch-bgex_20050921_0-emacs-21.4](patch-bgex_20050921_0-emacs-21.4)




# Specification

Please refer to bgex_memo_en.txt.
