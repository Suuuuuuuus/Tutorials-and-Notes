grep -w $1 -r $2 | cut -d : -f2 | cut -d , -f1,3 > $1.txt
