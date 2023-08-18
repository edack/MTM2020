#!/bin/sh

# HELPFUL WORDS FOR CONTESTANTS!!!
# This script takes three files and puts them into a directory
# and then produces some output based on the file content and
# how the script is invoked.
#
# FILE1, FILE2, and FILE3 should be named "animal1" "animal2" and "animal3"
# and the output directory should be named "uss2output"
# The files need to each have a type of local animal as their content.
# The script needs to be called with your own name as the first and only argument. 
#
FILE1=animal1
FILE2=animal2
FILE3=animal3
DIRECTORY1=uss2output

#This checks for the first parameter. If it's not there, it quits. 
: ${1?Make sure to run this with your name as the first parameter. Example: ./uss2script Baron}
USERNAME=$1

echo "Checking for the files and folders"
#"If there's three files that aren't empty (-s) AND (&&) the directory exist (-d), then do this"
if [ -s "$FILE1" ] && [ -s "$FILE2" ] && [ -s "$FILE3" ] && [ -d "$DIRECTORY1" ]; then
    echo "Everything looks good"
    
    #Copy the three files into that directory
    cp $FILE1 $FILE2 $FILE3 $DIRECTORY1
    echo "Files have been copied!"
    #Produce this output and put it in the "message" file
    echo "We're extremely happy to have $USERNAME on the system" > "$DIRECTORY1"/message
    MODELTYPE=$(uname -m)
    OS=$(uname -I)
    echo "The operating system is $OS running on a model type $MODELTYPE" >> "$DIRECTORY1"/message
    echo "" >> "$DIRECTORY1"/message
    echo "If $USERNAME looks out the window, they might say:" >> "$DIRECTORY1"/message
    
    #For every file (there should be three), do this
    for file in "$DIRECTORY1"/animal*
    do
        CURRENT_ANIMAL=$(cat $file)
        echo "Why hello there, $CURRENT_ANIMAL" >> "$DIRECTORY1"/message
    done

    echo "And they'll say back, \"Congratulations on finishing USS Part 2!\"" >> "$DIRECTORY1"/message

    echo "Script has run successfully! Check the 'message' file in the uss2output directory"


#If anything is missing, then do this
else
    echo "Something is missing. Double-check the instructions and script"
fi
