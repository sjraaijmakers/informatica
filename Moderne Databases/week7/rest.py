# Curl command to use rest api:
# curl -H "Content-Type: application/json" -X POST -d '[{"vendor": "Microsoft"}, {"product": "Windows"}]' http://localhost:PORT/documents
# curl -X "DELETE" http://localhost:8084/document/2/

from flask import Flask, jsonify, request
from static_tree import tree
import json
from mapreduce import Script
from collections import Counter

app = Flask(__name__)

# Retrieval of all docs
@app.route('/documents/', methods=['GET'])
def get():
    tmp = []
    for key in tree:
        tmp.append({key: tree[key]})
    return jsonify(tmp)

# Add docs
@app.route('/documents/', methods=['POST'])
def post():
    obj = request.json

    # bulk loading
    if isinstance(obj, list):
        tot = 0
        for i in obj:
            key = max(tree, key=int)
            tree[key+1] = i
            tot += 1
        return "Succesfully added "  + str(tot) + " documents"
    # single dict
    elif isinstance(obj, dict):
        tmp = []
        for k, v in obj.items():
            tmp.append({k: v})
        return jsonify(tmp)

# Deletion of whole database
@app.route('/documents/', methods=['DELETE'])
def delete():
    for key in [reversed([key for key in tree])][1:]:
        del tree[key]
    return "Delete succesful"

@app.route('/mapreduce/', methods=['GET'])
def show_mapreduce():
    return "post a pythonfile to this url, plz"

# MAPREDUCE:
@app.route('/mapreduce/', methods=['POST'])
def run_mapreduce():
    try:
        obj = request.data
        wrapper = Script()
        wrapper.add_file(obj)
        return str(wrapper.run_mapreduce(tree))
    except:
        return "Something went wrong"

# SINGLE DOC RETRIEVAL
@app.route('/document/<_id>/', methods=['GET'])
def get_single(_id):
    id = int(_id)
    try:
        tmp = {id: tree[id]}
        return jsonify(tmp)
    except:
        return "No entry found for doc " + str(id)

@app.route('/document/<_id>/', methods=['PUT'])
def get_single(_id):
    id = int(_id)
    try:
        obj = request.json
        tree[id] = jsonify(obj)
        return "updated " + str(id) + " to: " + jsonify(obj)
    except:
        return "No entry found for doc " + str(id)

# SINGLE DOC RETRIEVAL
@app.route('/document/<_id>/', methods=['DELETE'])
def delete_single(_id):
    # cast to int, important!!!!!!!!!!!
    id = int(_id)
    try:
        del tree[id]
        return "Delete document " + str(id)
    except KeyError:
        return "No entry found for doc " + str(id)
