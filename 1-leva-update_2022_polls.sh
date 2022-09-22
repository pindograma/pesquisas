#!/bin/bash

rm pesquisa_eleitoral_2022.zip

curl 'https://cdn.tse.jus.br/estatistica/sead/odsele/pesquisa_eleitoral/pesquisa_eleitoral_2022.zip' \
  -H 'authority: cdn.tse.jus.br' \
  -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
  -H 'accept-language: en-US,en;q=0.9,pt;q=0.8,de;q=0.7,fr;q=0.6' \
  -H 'cache-control: no-cache' \
  -H 'dnt: 1' \
  -H 'pragma: no-cache' \
  -H 'referer: https://dadosabertos.tse.jus.br/' \
  -H 'sec-ch-ua: " Not A;Brand";v="99", "Chromium";v="102", "Google Chrome";v="102"' \
  -H 'sec-ch-ua-mobile: ?0' \
  -H 'sec-ch-ua-platform: "macOS"' \
  -H 'sec-fetch-dest: document' \
  -H 'sec-fetch-mode: navigate' \
  -H 'sec-fetch-site: same-site' \
  -H 'sec-fetch-user: ?1' \
  -H 'upgrade-insecure-requests: 1' \
  -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36' \
  --compressed > pesquisa_eleitoral_2022.zip
unzip -o pesquisa_eleitoral_2022.zip -d data/tse/2022

rm data/tse/tse_2022.csv
awk '(NR == 1) || (FNR > 1)' data/tse/2022/*.csv > data/tse/tse_2022.csv
