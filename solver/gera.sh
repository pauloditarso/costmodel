filename=$1
label="${filename%.*}"
CWD="/data/OneDrive/Trabalho/git-area/github/costmodel"

python3 $CWD/solver/generate_lp.py $filename > $CWD/$label.glp
glpsol --lp $CWD/$label.glp -o $CWD/$label.res

#cat $label.res | grep 'x_o' | awk '$4==1 {print $2}' > $label.aux1

cat $CWD/$label.res | awk 'NF==2 { printf $0 " "; getline; print} NF>2 {print}' | grep 'x_o' | awk '$4==1 {print $2}' > $CWD/$label.aux1

cat $CWD/$label.aux1 | awk -F'_' '{print $5, $6+1, $2, $3+1, $4+1}' > $CWD/$label.aux2

cat $CWD/$label.aux2 | sort -V | awk -F" " '{print $4","$5}' > $CWD/$label.opt

rm $CWD/$label.aux1 $CWD/$label.aux2
