

.SUFFIXES		:
.PHONY			: all trunk 21.4 23.3a 24.2


BASE_MASTER		= patch-bgex_20190523_0-git-emacs-master
BASE_21_4		= patch-bgex_20050921_0-emacs-21.4
BASE_23_3A		= patch-bgex_20110822_0-emacs-23.3a
BASE_24_2		= patch-bgex_20120916_0-emacs-24.2
BASE_24_5		= patch-bgex_20150515_0-emacs-24.5
COMMON_FILES		= background.xpm bgex.el bgex_memo.txt dot_emacs.sample


all			: trunk 21.4 23.3a 24.2 24.5
	sha1sum $(BASE_21_4).tar.xz
	sha1sum $(BASE_23_3A).tar.xz
	sha1sum $(BASE_24_2).tar.xz
	sha1sum $(BASE_24_5).tar.xz
	sha1sum $(BASE_MASTER).tar.xz

trunk			: $(BASE_MASTER).tar.xz
21.4			: $(BASE_21_4).tar.xz
23.3a			: $(BASE_23_3A).tar.xz
24.2			: $(BASE_24_2).tar.xz
24.5			: $(BASE_24_5).tar.xz


$(BASE_MASTER).tar.xz	: $(BASE_MASTER) $(COMMON_FILES)
	-mkdir ./tmp
	rm -rf ./tmp/$(BASE_MASTER)
	mkdir ./tmp/$(BASE_MASTER)
	cp -av $(BASE_MASTER) $(COMMON_FILES) ./tmp/$(BASE_MASTER)/
	tar -C ./tmp/ -cJf $(BASE_MASTER).tar.xz $(BASE_MASTER)


$(BASE_21_4).tar.xz	: $(BASE_21_4) $(COMMON_FILES)
	-mkdir ./tmp
	rm -rf ./tmp/$(BASE_21_4)
	mkdir ./tmp/$(BASE_21_4)
	cp -av $(BASE_21_4) $(COMMON_FILES) ./tmp/$(BASE_21_4)/
	tar -C ./tmp/ -cJf $(BASE_21_4).tar.xz $(BASE_21_4)


$(BASE_23_3A).tar.xz	: $(BASE_23_3A) $(COMMON_FILES)
	-mkdir ./tmp
	rm -rf ./tmp/$(BASE_23_3A)
	mkdir ./tmp/$(BASE_23_3A)
	cp -av $(BASE_23_3A) $(COMMON_FILES) ./tmp/$(BASE_23_3A)/
	tar -C ./tmp/ -cJf $(BASE_23_3A).tar.xz $(BASE_23_3A)


$(BASE_24_2).tar.xz	: $(BASE_24_2) $(COMMON_FILES)
	-mkdir ./tmp
	rm -rf ./tmp/$(BASE_24_2)
	mkdir ./tmp/$(BASE_24_2)
	cp -av $(BASE_24_2) $(COMMON_FILES) ./tmp/$(BASE_24_2)/
	tar -C ./tmp/ -cJf $(BASE_24_2).tar.xz $(BASE_24_2)


$(BASE_24_5).tar.xz	: $(BASE_24_5) $(COMMON_FILES)
	-mkdir ./tmp
	rm -rf ./tmp/$(BASE_24_5)
	mkdir ./tmp/$(BASE_24_5)
	cp -av $(BASE_24_5) $(COMMON_FILES) ./tmp/$(BASE_24_5)/
	tar -C ./tmp/ -cJf $(BASE_24_5).tar.xz $(BASE_24_5)
