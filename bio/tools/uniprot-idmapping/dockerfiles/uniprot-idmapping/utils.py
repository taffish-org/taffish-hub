import json
import numpy as np
import requests


def save_json(data: dict, name: str):
    with open(name, 'w') as f:
        json.dump(data, f, indent=4)


def update_json(key, value, name):
    with open(name, 'r+') as f:
        d = json.load(f)
        d.update({key: value})
        json.dump(d, f, indent=4)


def read_json(name: dict):
    with open(name) as f:
        d = json.load(f)
    return d


def check_response(response: requests.Response):
    try:
        response.raise_for_status()
    except requests.HTTPError:
        print(response.json())
        raise


def divide_batches(data: list, size=500):
    return [data[i : i + size] for i in range(0, len(data), size)]


def isin(data: list, in_data: list, invert=False):
    return np.isin(data, in_data, invert=invert).tolist()


def compress(condition: list | np.ndarray, data: list):
    return np.compress(condition, data).tolist()
