#!/bin/bash

rm pesquisa_eleitoral_2020.zip
wget http://agencia.tse.jus.br/estatistica/sead/odsele/pesquisa_eleitoral/pesquisa_eleitoral_2020.zip
unzip -o pesquisa_eleitoral_2020.zip -d data/tse/2020

rm data/tse/tse_2020.csv
awk '(NR == 1) || (FNR > 1)' data/tse/2020/*.csv > data/tse/tse_2020.csv
