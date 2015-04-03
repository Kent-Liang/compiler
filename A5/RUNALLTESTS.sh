#! /bin/bash
WHERE=`dirname $0`

echo "------------------------------------------------------"
echo "$(tput setaf 2)TEST CASES FOR A3:$(tput sgr0)"
echo
for program in `ls $WHERE/testing/semantics/passing/{,*/}*.488`
do
  echo "$(tput setaf 2)Testing: $program$(tput sgr0)"
  ./RUNCOMPILER.sh -X $program
  echo
done

for program in `ls $WHERE/testing/semantics/failing/{,*/}*.488`
do
  echo "$(tput setaf 2)Testing: $program$(tput sgr0)"
  ./RUNCOMPILER.sh -X $program
  echo
done
echo "------------------------------------------------------"

echo "------------------------------------------------------"
echo "$(tput setaf 2)TEST CASES FOR A5:$(tput sgr0)"
echo
for program in `ls $WHERE/testing/pass/*/*.488`
do
  echo "$(tput setaf 2)Testing: $program$(tput sgr0)"
  ./RUNCOMPILER.sh $program
  echo
done
echo "------------------------------------------------------"
