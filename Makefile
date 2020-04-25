# Willaschek Tomas
# FLP-LOG
SRC=src/flp20-log.pl
GOAL=start

default:
	swipl -q -g ${GOAL} -o flp20-log -c ${SRC}

