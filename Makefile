

.PHONY			: all trunk 21.4 23.3a 24.2


BASE_TRUNK		= patch-bgex_20130329_0-bzr-emacs-trunk
BASE_21_4		= patch-bgex_20050921_0-emacs-21.4
BASE_23_3A		= patch-bgex_20110822_0-emacs-23.3a
BASE_24_2		= patch-bgex_20120916_0-emacs-24.2
COMMON_FILES		= background.xpm bgex.el bgex_memo.txt dot_emacs.sample


all			: trunk 21.4 23.3a 24.2
	sha1sum $(BASE_TRUNK).tar.bz2
	sha1sum $(BASE_21_4).tar.bz2
	sha1sum $(BASE_23_3A).tar.bz2
	sha1sum $(BASE_24_2).tar.bz2

trunk			: $(BASE_TRUNK).tar.bz2
21.4			: $(BASE_21_4).tar.bz2
23.3a			: $(BASE_23_3A).tar.bz2
23.3a			: $(BASE_24_2).tar.bz2


$(BASE_TRUNK).tar.bz2	: $(BASE_TRUNK) $(COMMON_FILES)
	-mkdir ./tmp
	rm -rf ./tmp/$(BASE_TRUNK)
	mkdir ./tmp/$(BASE_TRUNK)
	cp -av $(BASE_TRUNK) $(COMMON_FILES) ./tmp/$(BASE_TRUNK)/
	tar -C ./tmp/ -cjf $(BASE_TRUNK).tar.bz2 $(BASE_TRUNK)


$(BASE_21_4).tar.bz2	: $(BASE_21_4) $(COMMON_FILES)
	-mkdir ./tmp
	rm -rf ./tmp/$(BASE_21_4)
	mkdir ./tmp/$(BASE_21_4)
	cp -av $(BASE_21_4) $(COMMON_FILES) ./tmp/$(BASE_21_4)/
	tar -C ./tmp/ -cjf $(BASE_21_4).tar.bz2 $(BASE_21_4)


$(BASE_23_3A).tar.bz2	: $(BASE_23_3A) $(COMMON_FILES)
	-mkdir ./tmp
	rm -rf ./tmp/$(BASE_23_3A)
	mkdir ./tmp/$(BASE_23_3A)
	cp -av $(BASE_23_3A) $(COMMON_FILES) ./tmp/$(BASE_23_3A)/
	tar -C ./tmp/ -cjf $(BASE_23_3A).tar.bz2 $(BASE_23_3A)


$(BASE_24_2).tar.bz2	: $(BASE_24_2) $(COMMON_FILES)
	-mkdir ./tmp
	rm -rf ./tmp/$(BASE_24_2)
	mkdir ./tmp/$(BASE_24_2)
	cp -av $(BASE_24_2) $(COMMON_FILES) ./tmp/$(BASE_24_2)/
	tar -C ./tmp/ -cjf $(BASE_24_2).tar.bz2 $(BASE_24_2)
