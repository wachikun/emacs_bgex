# Emacs-BGEX patch

X 用 (X なら Cygwin でも MacOSX でも動きます!) FSF Emacs の背景拡張非公式パッチです。

Emacs-BGEX patch を適用すると背景に画像や色を指定できるようになります。

フレーム単位ではなく、バッファごとに異なる背景を指定することもできます。

かれこれ 10 年以上メンテナンスしています。




# Example

## YouTube

[![example](http://img.youtube.com/vi/5P89fA-2ZfU/0.jpg)](http://www.youtube.com/watch?v=5P89fA-2ZfU)




# Install

## Patch and Build

```
$ cd /tmp
$ wget http://umiushi.org/~wac/bgex/patch-bgex_VERSION.tar.xz
$ tar xf patch-bgex_VERSION.tar.xz
$ cp -av /tmp/patch-bgex_VERSION/bgex.el YOUR-ELISP-PATH/

$ cd YOUR-EMACS-SOURCE-PATH
$ cd src
$ patch > /tmp/patch-bgex_VERSION/patch-bgex_VERSION
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

- [patch-bgex_20180812_0-git-emacs-master](patch-bgex_20180812_0-git-emacs-master)


## Emacs-26.1

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

Please see bgex_memo.txt (in Japanese).
