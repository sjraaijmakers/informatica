#!/bin/sh
PERSON="$1"
WORD="$2"

printf ${PERSON}
printf " : "
printf ${WORD}
printf " : "

curl -X POST "http://localhost:10018/mapred" -H "content-type: application/json" \
-d '{
    "inputs": "'"${PERSON}"'",
	"query": [{
		"map": {
			"language": "javascript",
			"source": "
                function(v) {
                    v = v.values[0].data.toLowerCase().match(/'${WORD}'/g)
                    var count = 0;
                    (v || []).forEach(function(val) {
                        count += 1;
                    });
                    return [count]
                }
            "
		}
    },
    {
        "reduce": {
            "language": "javascript",
            "source": "function(v, k){
                var sum = v.reduce(add, 0);
                function add(a, b) {
                    return a + b;
                }
                return [sum];

            }"
        }
    }
    ]
}'
