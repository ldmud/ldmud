SHELL=/bin/sh
MAKE=make

TAR=gtar
TAROPTS=cvzf
TARDEST=/usr/local/tar/games/lpmud/driver-docs-3.2.1@133-
#TARDATE=19-Sep-94
TARDATE=`date +%y-%m-%d`
TARSUFFIX=.tar.gz
TARFILES=doc/Makefile doc/README.driverdocs doc/TODO doc/man2tex.awk \
	doc/3.2vs3.2.1 doc/LPC doc/applied doc/concepts doc/driver \
	doc/efun doc/master

default:
	@echo make tar

tar:
	$(TAR) $(TAROPTS) $(TARDEST)$(TARDATE)$(TARSUFFIX) $(TARFILES)
