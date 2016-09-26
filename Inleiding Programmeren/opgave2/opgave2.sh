#!/bin/bash
# Robin de Vries, robin@celp.nl, 2013
NAME="Opgave2"
FILES="Opgave2.java"

echo "Universiteit van Amsterdam - Inleiding Programmeren"
echo "Jose Lagerberg - Ilja Kamps - Robin de Vries"
echo $NAME

# Check that all the files exists
NOT_FOUND=""
for file in $FILES; do
    [ -f $file ] || NOT_FOUND="$NOT_FOUND $file"
done

# Show the missing files and ask the user to continue
if [[ -n $NOT_FOUND ]]
    then
        echo -e "\nThe following file is missing: $NOT_FOUND"
            exit
fi

# Ask for the users UvAnetID
echo -e "\nPlease insert your UvAnetID"
read UVANETID

# Create archive.
FILENAME=$NAME"_"$UVANETID".tar.gz"
echo -e "\nThe following archive will be created: $FILENAME"
tar -zcf $FILENAME *.java
