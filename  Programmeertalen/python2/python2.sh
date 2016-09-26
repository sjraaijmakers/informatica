#!/bin/bash
# Robin de Vries, robin@celp.nl, 2013
# Willem Vermeulen, w.r.j.vermeulen@gmail.com, 2014

FILEPREFIX="Python_Sudoku"
NAME="Python Sudoku"

echo "Universiteit van Amsterdam - Programmeertalen"
echo "Leen Torenvliet"
echo $NAME

# Users can name their files themselves

# Ask for the users UvAnetID
echo -e "\nPlease insert your UvAnetID"
read UVANETID

# Ask for the users name
echo -e "\nPlease insert your name"
read STUDENTNAME 
STUDENTNAME=$(echo "$STUDENTNAME" | tr -d " ")

# Create archive.
FILENAME=$FILEPREFIX"_"$UVANETID".tar.gz"
echo -e "\nThe following archive will be created: $FILENAME"
mkdir $STUDENTNAME"_"$UVANETID
cp *.py $STUDENTNAME"_"$UVANETID
# If someone decides to add a report
cp *.pdf $STUDENTNAME"_"$UVANETID
tar  -zcf $FILENAME $STUDENTNAME"_"$UVANETID
rm -rf $STUDENTNAME"_"$UVANETID
