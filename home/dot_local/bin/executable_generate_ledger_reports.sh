#!/bin/zsh
# Reactively generate ledger reports when ledger file is changed

while true
do
	ledger bal Bienes > $HOME/ledger/_bienes.txt
	if [[ "$?" == "0" ]];
		then mv $HOME/ledger/_bienes.txt $HOME/ledger/bienes.txt
		else rm $HOME/ledger/_bienes.txt
	fi

	#ledger bal Fondos > $HOME/ledger/_fondos.txt
	#if [[ "$?" == "0" ]];
		#then mv $HOME/ledger/_fondos.txt $HOME/ledger/fondos.txt
		#else rm $HOME/ledger/_fondos.txt
	#fi

	ledger -b "this month" reg Gastos > $HOME/ledger/_gastos.txt
	if [[ "$?" == "0" ]];
		then mv $HOME/ledger/_gastos.txt $HOME/ledger/gastos.txt
		else rm $HOME/ledger/_gastos.txt
	fi

	inotifywait -qe modify $HOME/ledger/personal.ledger
done
