cat $1 | grep "^{" | awk -F';' '{print NF}'
