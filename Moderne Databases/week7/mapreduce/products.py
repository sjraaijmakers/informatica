def mapper(_, value):
    output = []
    for _, v in value['software']:
        product = v.split(':')[0]
        if not product in output:
            output.append(product)
    return [(v, 1) for v in output]

def reducer(key, values):
    return (key, sum(values))
