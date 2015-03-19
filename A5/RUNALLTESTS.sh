#! /bin/bash
WHERE=`dirname $0`
echo "------------------------------------------------------"
echo "$(tput setaf 2)SHOULD PASS TEST CASES:$(tput sgr0)"
echo
for program in `ls $WHERE/tests/passing/{,*/}*.488`
do
  echo "$(tput setaf 2)Testing: $program$(tput sgr0)"
  ./RUNCOMPILER.sh $program
  echo
done
echo "------------------------------------------------------"
echo "$(tput setaf 2)SHOULD FAIL TEST CASES:$(tput sgr0)"
echo
for program in `ls $WHERE/tests/failing/{,*/}*.488`
do
  echo "$(tput setaf 2)Testing: $program$(tput sgr0)"
  ./RUNCOMPILER.sh $program
  echo
done
echo "------------------------------------------------------"
