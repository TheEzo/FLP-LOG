# Willaschek Tomas
# FLP-LOG
SRC=flp20-log.pl
START=start


default:
	swipl -o flp20-log ${SRC} -g ${START}

