import json

def load_json(path):
    with open(path, 'r') as f:
        return json.loads(f.read())

def save_json(path, data):
    with open(path, 'w') as f:
        f.write(json.dumps(data))

def query(prompt):
    return input('{} '.format(prompt))

def query_yes_no(prompt, default=True):
    answers = { 'y': True, 'yes': True, 'n': False, 'no': False,
        }
    defaults = { None: '[y/n]', True: '[Y/n]', False: '[y/N]',
        }

    while True:
        answer = query('{} {}'.format(prompt, defaults[default])).lower()

        if default is not None and answer == '':
            return default
        
        if answer in answers:
            return answers[answer]
        
        print('Please respond with \'yes\' or \'no\'.')

