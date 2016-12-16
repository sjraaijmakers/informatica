#!/bin/bash
rm -rf output/
mvn package
mvn exec:java -Dexec.args="tweets2009-06-brg1of2.txt output/ 1"
