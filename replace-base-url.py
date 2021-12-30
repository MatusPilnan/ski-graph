import re
import argparse

arg_parser = argparse.ArgumentParser()
arg_parser.add_argument("url")

args = arg_parser.parse_args()

with open('public/index.html', 'r', encoding='utf-8') as f:
    content = f.read()

content = re.sub('baseUrl: ".+"', f'baseUrl: "{args.url.rstrip("/")}"', content)

with open('public/index.html', 'w', encoding='utf-8') as f:
    f.write(content)
