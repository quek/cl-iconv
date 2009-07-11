VER=0.4

dist:
	darcs dist
	mv iconv.tar.gz iconv_$(VER).tar.gz
	gpg -b -a iconv_$(VER).tar.gz