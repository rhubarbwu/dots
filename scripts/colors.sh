#!/bin/sh

file=~/.config/kitty/colors.conf
rm $file
touch $file

# grab the purest color definition file
input="/home/rusbridger/.cache/wal/colors"
i=0
while IFS= read -r line
do
    if [ $i -eq 0 ]; then 
        echo "background ${line}" >> $file
    elif [ $i -eq 7 ]; then
        echo "foreground ${line}" >> $file
    fi
    echo "color$i ${line}" >> $file
    ((i++))
done < "$input"

