import json

def test(kw):
    with open("./energy.json", 'r') as fp:
        json_data = json.load(fp)
    print(json_data[kw])
    return None