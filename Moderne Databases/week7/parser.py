import xmltodict

def parse(file_name):
    with open(file_name) as fd:
        docs = xmltodict.parse(fd.read())['nvd']['entry']

    store = []

    for doc in docs:
        entry = {}

        entry['time'] = doc['vuln:published-datetime']

        entry['software'] = []

        if not 'vuln:vulnerable-software-list' in doc:
            continue

        if isinstance(doc['vuln:vulnerable-software-list']['vuln:product'], str):
            spl = doc['vuln:vulnerable-software-list']['vuln:product'].split(':')
            entry['software'].append((spl[2], ':'.join(spl[3:])))
        else:
            for s in doc['vuln:vulnerable-software-list']['vuln:product']:
                spl = s.split(':')
                entry['software'].append((spl[2], ':'.join(spl[3:])))

        store.append(entry)

    return store
