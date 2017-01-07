#!/bin/bash
# Robin de Vries, robin@celp.nl, 2013
NAME="Opgave3"
FILES="Breuk.java TestBreuken.java BreukInterface.java ComplexGetal.java TestComplexeGetallen.java ComplexGetalInterface.java"

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
        echo -e "\nThe following file(s) is/are missing: $NOT_FOUND"
        echo "Do you want to continue (y/n)?"
        read USR
        if [ $USR != "y" ] 
            then
                exit
        fi
fi

# Ask for the users UvAnetID
echo -e "\nPlease insert your UvAnetID"
read UVANETID

# Ask for the users name
echo -e "\nPlease insert your name"
read STUDENTNAME 
STUDENTNAME=$(echo "$STUDENTNAME" | tr -d " ")

# Create archive.
FILENAME=$NAME"_"$UVANETID".tar.gz"
echo -e "\nThe following archive will be created: $FILENAME"
mkdir $STUDENTNAME"_"$UVANETID
cp *.java $STUDENTNAME"_"$UVANETID
tar  -zcf $FILENAME $STUDENTNAME"_"$UVANETID
rm -rf $STUDENTNAME"_"$UVANETID