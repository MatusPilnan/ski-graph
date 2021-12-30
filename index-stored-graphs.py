import json
import os
import argparse
import uuid


def generate_graph_files_index(graphs_dir="public/graphs", graphs_base_url="/graphs", index_file="index.json"):
    index = []

    for file in os.listdir(graphs_dir):
        if file == index_file:
            continue

        with open(os.path.join(graphs_dir, file), "r", encoding="utf-8") as f:
            graph = json.load(f)

        entry = dict(title=file, path=f'{graphs_base_url}/{file}')
        try:
            entry['title'] = graph['title']
        except KeyError:
            print(f'File "{file}" has no graph title specified.')

        try:
            entry['id'] = graph['id']
        except KeyError:
            entry['id'] = str(uuid.uuid4())
        index.append(entry)

    with open(os.path.join(graphs_dir, index_file), "w", encoding='utf-8') as f:
        json.dump(index, f, indent=2)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--dir", "-d", help="Directory where graph files are stored.", dest="graphs_dir")
    parser.add_argument("--url", "-u", help="Directory from where graphs will be served", dest="graphs_base_url")
    parser.add_argument("--index", "-i", help="Name of the graph index file.", dest="index_file")

    args = parser.parse_args()

    print({k: v for k, v in vars(args).items() if v is not None})
    generate_graph_files_index(**{k: v for k, v in vars(args).items() if v is not None})
