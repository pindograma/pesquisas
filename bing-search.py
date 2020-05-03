from azure.cognitiveservices.search.websearch import WebSearchClient
from msrest.authentication import CognitiveServicesCredentials
from pprint import pprint
import time
import csv

with open('gapi-key-oscar.txt', 'r') as f:
    with open('bing_input.txt', 'r') as inp:
        with open('gapi-endpoint.txt', 'r') as ff:
            subscription_key = f.read().strip()
            endpoint = ff.read().strip()

            client = WebSearchClient(
                endpoint=endpoint,
                credentials=CognitiveServicesCredentials(subscription_key))

            tse_ids = [x.strip() for x in inp.readlines()]
            oup_dicts = []

            for tse_id in tse_ids:
                print(tse_id)
                web_data = client.web.search(
                    query = '"{}"'.format(tse_id),
                    count = 10,
                    responseFilter = 'Webpages')

                if hasattr(web_data.web_pages, 'value'):
                    for page in web_data.web_pages.value:
                        oup_dicts.append({
                            'tse_id': tse_id,
                            'url': page.url,
                            'name': page.name,
                            'snippet': page.snippet
                        })
                        
                time.sleep(0.4)

            with open('bing_output.csv', 'w', newline='\n', encoding='utf-8') as oup:
                dict_writer = csv.DictWriter(oup, oup_dicts[0].keys(), lineterminator='\n')
                dict_writer.writeheader()
                dict_writer.writerows(oup_dicts)
