#!/bin/bash
# Robin de Vries, robin@celp.nl, 2013
FILEPREFIX="Erlang"
NAME="Erlang assignment"

echo "Universiteit van Amsterdam - Programmeertalen"
echo "Leen Torenvliet - Rein van den Boomgaard - Robin de Vries"
echo $NAME

# Ask for the users student id
echo -e "\nPlease insert your student id"
read STUDENTID

# Create archive.
FILENAME=$FILEPREFIX"_"$STUDENTID".tar.gz"
echo -e "\nThe following archive will be created: $FILENAME"
tar --transform "s,^,$STUDENTID/," -zcf $FILENAME *.*
