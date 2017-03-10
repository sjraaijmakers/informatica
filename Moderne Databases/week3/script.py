# Steven Raaijmakers, 10804242
# Script imports schema below in Neo4j

from py2neo import Graph, Node, Relationship

graph = Graph("http://localhost:7474/db/data")

# Classes (is sneller dan alles weghalen)
class Schema:
    def __init__(self, list):
        self.list = list

class Entity:
    def __init__(self, label, properties, relations=None):
        self.label = label
        self.properties = properties
        self.relations = relations

class IntegerProperty:
    def __init__(self, type, column):
        self.type = type
        self.column = column

class Property:
    def __init__(self, type, column):
        self.type = type
        self.column = column

class Column:
    def __init__(self, name, fk=None):
        self.name = name
        self.fk = fk

    def __repr__(self):
        if self.fk:

            return str(self.name)
        else:
            return str(self.name)

class ForeignColumn:
    def __init__(self, name, label, null=False):
        self.name = name
        self.label = label
        self.null = null

    def __repr__(self):
        return str(self.name)

class Relation:
    def __init__(self, name, start, end, properties):
        self.name = name
        self.start = start
        self.end = end
        self.properties = properties

class Reference:
    def __init__(self, entity, column):
        self.entity = entity
        self.column = column

# Schema from redapple:
mbschema = Schema([
    Entity('area_type', [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
        ],
    ),
    Entity('area', [
            IntegerProperty('pk', Column('id')),
            Property('mbid', Column('gid')),
            Property('name', Column('name')),
            Property('type', Column('type', ForeignColumn('area_type', 'name', null=True))),
        ],
    ),
    Entity('area_alias',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
            Property('type', Column('type', ForeignColumn('area_alias_type', 'name', null=True))),
            Property('locale', Column('locale')),
        ],
        [
            Relation(
                'HAS_ALIAS',
                start=Reference('area', Column('area')),
                end=Reference('area_alias', Column('id')),
                properties=[]
            ),
        ]
    ),
    Entity('place', [
            IntegerProperty('pk', Column('id')),
            Property('mbid', Column('gid')),
            Property('name', Column('name')),
            Property('type', Column('type', ForeignColumn('place_type', 'name', null=True))),
            IntegerProperty('latitude', Column('coordinates[0]')),
            IntegerProperty('longitude', Column('coordinates[1]')),
        ],
    ),
    Entity('place_alias',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
            Property('type', Column('type', ForeignColumn('place_alias_type', 'name', null=True))),
            Property('locale', Column('locale')),
        ],
        [
            Relation(
                'HAS_ALIAS',
                start=Reference('place', Column('place')),
                end=Reference('place_alias', Column('id')),
                properties=[]
            ),
        ]
    ),
    Entity('artist',
        [
            IntegerProperty('pk', Column('id')),
            Property('mbid', Column('gid')),
            Property('disambiguation', Column('comment')),
            Property('name', Column('name')),
            Property('type', Column('type', ForeignColumn('artist_type', 'name', null=True))),

            #Property('sort_name', Column('sort_name', ForeignColumn('artist_name', 'name'))),
            #Property('country', Column('country', ForeignColumn('country', 'name', null=True))),
            #Property('country', Column('country', ForeignColumn('country', 'iso_code', null=True))),
            Property('gender', Column('gender', ForeignColumn('gender', 'name', null=True))),
        ],
        [
            Relation(
                'FROM_AREA',
                start=Reference('artist', Column('id')),
                end=Reference('area', Column('area')),
                properties=[]
            ),
            Relation(
                'BEGAN_IN_AREA',
                start=Reference('artist', Column('id')),
                end=Reference('area', Column('begin_area')),
                properties=[]
            ),
            Relation(
                'ENDED_IN_AREA',
                start=Reference('artist', Column('id')),
                end=Reference('area', Column('end_area')),
                properties=[]
            ),
        ],
    ),
    Entity('artist_alias',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
            Property('type', Column('type', ForeignColumn('artist_alias_type', 'name', null=True))),
        ],
        [
            Relation(
                'HAS_ALIAS',
                start=Reference('artist', Column('artist')),
                end=Reference('artist_alias', Column('id')),
                properties=[]
            ),
        ]
    ),
    Entity('artist_type',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
        ],
    ),
    Entity('artist_credit',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
        ]
    ),
    Entity('artist_credit_name',
        [],
        relations = [
            Relation(
                'CREDITED_AS',
                start=Reference('artist', Column('artist')),
                end=Reference('artist_credit', Column('artist_credit')),
                properties=[
                    IntegerProperty('position', Column('position')),
                    Property('join', Column('join_phrase')),
                ]
            ),
        ]
    ),
    Entity('gender', [
        IntegerProperty('pk', Column('id')),
        Property('name', Column('name')),
    ]),
    Entity('label',
        [
            IntegerProperty('pk', Column('id')),
            Property('mbid', Column('gid')),
            Property('disambiguation', Column('comment')),
            IntegerProperty('code', Column('label_code')),
            Property('name', Column('name')),
            Property('type', Column('type', ForeignColumn('label_type', 'name', null=True))),

            #Property('sort_name', Column('sort_name', ForeignColumn('label_name', 'name'))),
            #Property('country', Column('country', ForeignColumn('country', 'name', null=True))),
            #Property('country', Column('country', ForeignColumn('country', 'iso_code', null=True))),
        ],
        [
            Relation(
                'FROM_AREA',
                start=Reference('label', Column('id')),
                end=Reference('area', Column('area')),
                properties=[]
            ),
        ],
    ),
    Entity('label_type',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
        ]
    ),
    Entity('work',
        [
            IntegerProperty('pk', Column('id')),
            Property('mbid', Column('gid')),
            Property('disambiguation', Column('comment')),
            Property('name', Column('name')),
            Property('type', Column('type', ForeignColumn('work_type', 'name', null=True))),
        ],
        # no relationships
        []
    ),
    Entity('work_type',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
        ]
    ),
    Entity('release_group',
        [
            IntegerProperty('pk', Column('id')),
            Property('mbid', Column('gid')),
            Property('disambiguation', Column('comment')),
            Property('name', Column('name')),
            Property('type', Column('type', ForeignColumn('release_group_primary_type', 'name', null=True))),
            #Property('artist', Column('artist_credit', ForeignColumn('artist_credit', 'name', ForeignColumn('artist_name', 'name')))),
        ],
        [
            Relation(
                'CREDITED_ON',
                start=Reference('artist_credit', Column('artist_credit')),
                end=Reference('release_group', Column('id')),
                properties=[]
            ),
        ]
    ),
    Entity('release_group_primary_type',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
        ]
    ),
    Entity('release',
        [
            IntegerProperty('pk', Column('id')),
            Property('mbid', Column('gid')),
            Property('disambiguation', Column('comment')),
            Property('name', Column('name')),
            Property('status', Column('status', ForeignColumn('release_status', 'name', null=True))),
            Property('packaging', Column('packaging', ForeignColumn('release_packaging', 'name', null=True))),

            #Property('barcode', Column('barcode')),
            #Property('type', Column('release_group', ForeignColumn('release_group', 'type', ForeignColumn('release_group_primary_type', 'name', null=True)))),
            #Property('artist', Column('artist_credit', ForeignColumn('artist_credit', 'name', ForeignColumn('artist_name', 'name')))),
            #Property('country', Column('country', ForeignColumn('country', 'name', null=True))),
            #Property('country', Column('country', ForeignColumn('country', 'iso_code', null=True))),
            #Property('alias', Column('release_group', ForeignColumn('release_group', 'name', ForeignColumn('release_name', 'name')))),
        ],
        [
            Relation(
                'CREDITED_ON',
                start=Reference('artist_credit', Column('artist_credit')),
                end=Reference('release', Column('id')),
                properties=[]
            ),
            Relation(
                'PART_OF',
                start=Reference('release', Column('id')),
                end=Reference('release_group', Column('release_group')),
                properties=[]
            ),
        ]
    ),
    Entity('release_status',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
        ]
    ),
    Entity('release_label',
        [],
        [
            Relation(
                'RELEASED_ON',
                start=Reference('release', Column('release')),
                end=Reference('label', Column('label')),
                properties=[
                    Property('catalog_number', Column('catalog_number')),
                ]
            ),
        ]
    ),
    Entity('release_packaging',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
        ]
    ),
    Entity('release_country',
        # do not create nodes
        [],
        [
            Relation(
                'RELEASED_IN',
                start=Reference('release', Column('release')),
                end=Reference('area',
                                Column('country',
                                    ForeignColumn('country_area', 'area'))),
                properties=[
                    IntegerProperty('year', Column('date_year')),
                    IntegerProperty('month', Column('date_month')),
                    IntegerProperty('day', Column('date_day')),
                ]
            ),
        ]
    ),
    Entity('medium',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
            Property('format', Column('format', ForeignColumn('medium_format', 'name', null=True))),
        ],
        [
            Relation(
                'RELEASED_ON_MEDIUM',
                start=Reference('release', Column('release')),
                end=Reference('medium', Column('id')),
                properties=[]
            ),
        ]
    ),
    Entity('medium_format',
        [
            IntegerProperty('pk', Column('id')),
            Property('name', Column('name')),
        ],
        [
            Relation(
                'PARENT_FORMAT',
                start=Reference('medium_format', Column('id')),
                end=Reference('medium_format', Column('parent')),
                properties=[]
            ),
        ]
    ),
    Entity('recording',
        [
            IntegerProperty('pk', Column('id')),
            Property('mbid', Column('gid')),
            Property('disambiguation', Column('comment')),
            Property('name', Column('name')),

            #Property('artist', Column('artist_credit', ForeignColumn('artist_credit', 'name', ForeignColumn('artist_name', 'name')))),
        ],
        [
            Relation(
                'CREDITED_ON',
                start=Reference('artist_credit', Column('artist_credit')),
                end=Reference('recording', Column('id')),
                properties=[]
            ),
        ]
    ),
    Entity('track',
        [
            IntegerProperty('pk', Column('id')),
            Property('mbid', Column('gid')),
            Property('name', Column('name')),
            IntegerProperty('position', Column('position')),
            IntegerProperty('length', Column('length')),
        ],
        [
            Relation(
                'IS_RECORDING',
                start=Reference('track', Column('id')),
                end=Reference('recording', Column('recording')),
                properties=[]
            ),
            Relation(
                'APPEARS_ON',
                start=Reference('track', Column('id')),
                end=Reference('medium', Column('medium')),
                properties=[]
            ),
            Relation(
                'CREDITED_ON',
                start=Reference('artist_credit', Column('artist_credit')),
                end=Reference('track', Column('id')),
                properties=[]
            ),
        ],
    ),
    Entity('url',
        [
            IntegerProperty('pk', Column('id')),
            Property('mbid', Column('gid')),
            Property('name', Column('url')),
        ],
    ),
    ]
)


nodes = []

def add_node(node_name, type="table"):
    if str(node_name) in nodes:
        print "not added " + str(node_name)
    else:
        graph.create(Node(node_name, type=type, name=str(node_name)))
        nodes.append(str(node_name))
        print "added " + str(node_name)

# importinggg

# debug
graph.delete_all()

# add tables as nodes
for entity in mbschema.list:
    add_node(entity.label)

# add attributes as nodes
for entity in mbschema.list:
    for property in entity.properties:
        add_node(property.column, "attribute")

# create relations for tabels and attributes
for entity in mbschema.list:
    for property in entity.properties:
        u1 = graph.find_one(entity.label)
        u2 = graph.find_one(property.column)

        if property.column.fk:
            # print str(property.column.fk.name) + "/" + str(property.column.fk.label)
            fk = str(property.column.fk.name)
            has_attribute = Relationship(u1, 'has attribute', u2, fk=fk)
        else:
            has_attribute = Relationship(u1, 'has attribute', u2)
        #
        graph.create(has_attribute)
        print "created relation between attributes " + str(entity.label) + " and " + str(property.column)

# relations between tables
for entity in mbschema.list:
    if entity.relations:
        for relation in entity.relations:
            u1 = graph.find_one(relation.start.entity)
            u2 = graph.find_one(relation.end.entity)
            has_attribute = Relationship(u1, relation.name, u2)
            graph.create(has_attribute)
            print "created relation between " + str(relation.start.entity) + " and " + str(relation.end.entity)
