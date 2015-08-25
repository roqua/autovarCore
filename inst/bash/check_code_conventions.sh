#!/bin/bash
matching_lines=$(grep --include "*.r" -nrE "[^ !=><]=|[,=][^ =]|[^ <]<-|<-[^ ]|[^ ](<=|==|!=|>=)|(<=|==|!=|>=)[^ ]" .)
status_code=$(echo $matching_lines | sed -e $'s/ \\.\\//\\\n\\.\\//g' | wc -l | sed -e 's/^[^0-9]*\([0-9]*\)[^0-9]*/\1/g')
if [ $status_code -gt 0 ]
then
  echo $matching_lines | sed -e $'s/ \\.\\//\\\n\\.\\//g'
fi
exit $status_code
