# bing_search2.py
# (c) 2022 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

import json
import os 
import requests
import time
import csv
import traceback

with open('bing_key_2.txt', 'r') as f:
    with open('bing_input.txt', 'r') as inp:
        with open('bing_endpoint_2.txt', 'r') as ff:
            subscription_key = f.read().strip()
            endpoint = ff.read().strip()

            tse_ids = [x.strip() for x in inp.readlines()]
            oup_dicts = []

            for tse_id in tse_ids:
                print(tse_id)

                try:
                    time.sleep(0.4)

                    headers = {
                        'Ocp-Apim-Subscription-Key': subscription_key
                    }

                    params = {
                        'q': '{}'.format(tse_id),
                        'mkt': 'pt-BR',
                        'responseFilter': 'Webpages',
                        'count': 10,
                        'setLang': 'pt'
                    }

                    web_data = requests.get(endpoint, headers=headers, params=params).json()

                    if 'value' in web_data['webPages']:
                        for page in web_data['webPages']['value']:
                            oup_dicts.append({
                                'tse_id': tse_id,
                                'url': page['url'],
                                'name': page['name'],
                                'snippet': page['snippet']
                            })
                
                except Exception as e:
                    traceback.print_exc()
                    break
                        
            with open('bing_output.csv', 'w', newline='\n', encoding='utf-8') as oup:
                dict_writer = csv.DictWriter(oup, oup_dicts[0].keys(), lineterminator='\n')
                dict_writer.writeheader()
                dict_writer.writerows(oup_dicts)
