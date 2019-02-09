
##
##  Copyright (C) 2019 Jesper Quorning
##

all:
	gprbuild -k -p wedo.gpr

clean:
	gprclean -q wedo.gpr

setup:
	tools/create-setup-adb.sh
