#!/bin/bash

matching_files=$(ls R | grep -e '^[a-z]' 2>/dev/null)
if [[ $matching_files != '' ]]
then
  echo 'Files should start with a capital'
  echo $matching_files
  exit 1
fi

matching_lines=$(grep --include "*.r" --include "*.R" -nrE "[^ !=><:]=|[^ <]<-|<-[^ ]|[^ ](<=|==|!=|>=)|(<=|==|!=|>=)[^ ]" . 2>/dev/null)
if [[ $matching_lines == '' ]]
then
  exit 0
fi

status_code=$(echo $matching_lines | sed -e $'s/ \\.\\//\\\n\\.\\//g' | wc -l | sed -e 's/^[^0-9]*\([0-9]*\)[^0-9]*/\1/g')
if [ $status_code -gt 0 ]
then
  echo $matching_lines | sed -e $'s/ \\.\\//\\\n\\.\\//g'
fi
exit $status_code
