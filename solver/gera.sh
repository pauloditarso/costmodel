filename=$1
label="${filename%.*}"

./generate_lp.py $filename > $label.glp
glpsol --lp $label.glp -o $label.res

#cat $label.res | grep 'x_o' | awk '$4==1 {print $2}' > $label.aux1

cat $label.res | awk 'NF==2 { printf $0 " "; getline; print} NF>2 {print}' | grep 'x_o' | awk '$4==1 {print $2}' > $label.aux1

cat $label.aux1 | awk -F'_' '{print $5, $6+1, $2, $3+1, $4+1}' > $label.aux2

cat $label.aux2 | sort -V | awk -F" " '{print $4","$5}' > $label.opt

rm $label.aux1 $label.aux2
