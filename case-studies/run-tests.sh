#!/bin/bash

usage()
{
cat << EOF

usage: $0 <options> <directories>

OPTIONS:
  -h      Show this message
  -t <x>  Timeout for each test (in seconds)
  -k <x>  Index value to generate the at-most-x dimension program
  -f <x>  Name of the file to be tested (without .horn extension). 

EXAMPLES:
$0 -t 15 unit           # run unit tests with 15 seconds for timeout
$0 -t 5 NRC ocaml       # run NRC and OCaml tests with 5 seconds for timeout
$0 -horn-core-simplify  # run all tests with -horn-core-simplify

EOF
}

FILE=

TIMELIMIT=600
EXTRAOPTS=


while (( "$#" )); do
  if [[ "$1" == "-h" ]]; then
    usage
    exit -1
  elif [[ "$1" == "-t" ]]; then
    TIMELIMIT=$2
    shift
  elif [[ "$1" == "-k" ]]; then
    K=$2
    shift
  elif [[ "$1" == "-f" ]]; then
    F=$2
    shift
  elif [[ ${1:0:1} == "-" ]]; then
    EXTRAOPTS="$EXTRAOPTS $1"
  fi

  shift
done

# Running swipl and generating programs P1 and P2 from P0: 

KDIM=kdim.pl
MAIN=../main.pl

base="$F".horn

P0_PATH=P0/$base
P1_PATH=P1/$base
P2_PATH=P2/$base

swipl -f "$KDIM" -g script,halt -- $P0_PATH $K $P1_PATH

echo false:-\'false[$K]\'. >> $P1_PATH

swipl -f "$MAIN" -g script,halt -- $P1_PATH $P2_PATH

# --------------------------------------------------------------------------------

echo "Running test.."
echo "qarmc$EXTRAOPTS"
echo "timeout: $TIMELIMIT"
echo "# test: $F"
echo

GOOD=0
FAILED=0
TIMEOUT=0
MEMORY=0
BEGIN_TIME=`date +%s`


for file in $P0_PATH $P1_PATH $P2_PATH; do
  echo -n "$file.. "

  logfile=$file.`date +%Y.%m.%d.%H.%M`.log
  #Note that it's essential to run this using time here
  OUTPUT=`(./qarmc-latest.osx $EXTRAOPTS $file > $logfile) 2>&1`
  #OUTPUT=`(time gtimeout $TIMELIMIT ./qarmc-latest.osx $EXTRAOPTS $file > $logfile) 2>&1`
#  OUTPUT=`timeout $TIMELIMIT ./qarmc $EXTRAOPTS "$file" >& "$file.log"`
#  OUTPUT=`timeout $TIMELIMIT ./qarmc $EXTRAOPTS "$file" 2>&1`
  RES=$?

  echo $file | grep -q 'false\|fail\|BUG\|error\|unsafe'
  SHOULDPASS=$?

  if [ $RES -eq 124 ] ; then
    echo -n -e "\033[1;33mTIMEOUT\033[0m"
    : $(( ++TIMEOUT ))
#  TODO: reenable when MCC is in place
#  elif ( grep -q "Q'ARMC: program is correct" "$TMP" && grep -A1 "verifying fixpoint..." "$TMP" | grep -q "done." ) ; then
  elif ( ( test $SHOULDPASS == 1 && cat $logfile | grep -q "Q'ARMC: program is correct" ) ||
   ( test $SHOULDPASS == 0 && cat $logfile| grep -q "Q'ARMC: program is not correct" ) ) ; then
    echo -n -e "\033[1;32mPASSED\033[0m"
    : $(( ++GOOD ))
  elif ( echo $OUTPUT | grep -q "Resource error: insufficient memory" ) ; then
    echo -n -e "\033[1;33mOUT OF MEMORY\033[0m"
    : $(( ++MEMORY ))
  else
    echo -n -e "\033[1;31mFAILED\033[0m"
    : $(( ++FAILED ))
  fi
  
  TIME=`echo "$OUTPUT" | grep "^real" | perl -pe 's/^real\s+(\d+)m(\d+)\.(\d+)s\$/(\$1*60+\$2) . "." . \$3/e'`
  echo " (in" `printf "%s" "$TIME"` "s)"
done

END_TIME=`date +%s`
TIME=$(( END_TIME-BEGIN_TIME ))

NUMBER_OF_TESTS=$((GOOD + FAILED + TIMEOUT + MEMORY))
PERCENT=$((GOOD * 100 / NUMBER_OF_TESTS))

echo
echo "passed ${GOOD} out of ${NUMBER_OF_TESTS} tests (${PERCENT}%)"
echo "timeout: ${TIMEOUT}   out of memory: ${MEMORY}"

echo "Total time:" `printf "%'8.0f" "$TIME"` "seconds"


if [ $GOOD -ne $NUMBER_OF_TESTS ] ; then
  exit 1
fi
