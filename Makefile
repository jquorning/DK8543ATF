##
##  The author disclaims copyright to this source code.  In place of
##  a legal notice, here is a blessing:
##
##    May you do good and not evil.
##    May you find forgiveness for yourself and forgive others.
##    May you share freely, not taking more than you give.
##

INSTALL_PREFIX=/Users/jquorning/opt/wedonu

all: setup build

build:
	gprbuild -k wedonu.gpr

clean:
	gprclean -q wedonu.gpr

setup:
	tools/create-setup-adb.sh

install:
	gprinstall -v -p --mode=usage wedonu.gpr --prefix=$(INSTALL_PREFIX)

uninstall:
	gprinstall -v -p --mode=usage wedonu.gpr --uninstall --prefix=$(INSTALL_PREFIX)
