def mapper(_, value):
    output = []
    for vendor, _ in value['software']:
        if not vendor in output:
            output.append(vendor)
    return [(v, 1) for v in output]

def reducer(key, values):
    return (key, sum(values))
