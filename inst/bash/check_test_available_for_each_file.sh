#!/bin/bash
spec_dir=`find tests/testthat -name '*.R' -type f -exec basename {} \; | sed -e 's/test-//g' | sort | uniq`
app_dir=`find R -name '*.R' -type f -exec basename {} \; | sort | uniq`
result=`comm -13 <(echo "$spec_dir") <(echo "$app_dir")`
result=`echo $result | sed -e 's/[ \t]+/\n/g'`

# Remove accepted missings
array=()

for file in "${array[@]}"
do
  result=`echo $result | sed -e s/$file//g`
done

status_code=$(echo $result | wc -w)
if [ $status_code -gt 0 ]
then
  echo $result | sed -e $'s/ /\\\n/g' | xargs -I{} echo "File {} does not have a test file"
fi
exit $status_code
