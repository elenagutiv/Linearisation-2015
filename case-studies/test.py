#!/bin/python
import sys, getopt,subprocess

argv = sys.argv[1:]

try:
  opts, args = getopt.getopt(argv,"hk:f:",[])
except getopt.GetoptError:
  print 'test.py -k <dimension> -f <inputfile>'
  sys.exit(2)
for opt, arg in opts:
  if opt == '-h':
     print 'test.py -k <dimension> -f <inputfile>'
     sys.exit()
  elif opt in ("-k"):
     k = arg
  elif opt in ("-f"):
     f = arg

# Running swipl and generating programs P1 and P2 from P0: 

kdim="kdim.pl"
main="../main.pl"

base=f+".horn"

p0_path="P0/"+base
p1_path="P1/"+base
p2_path="P2/"+base

#swipl -f "$KDIM" -g script,halt -- $P0_PATH $K $P1_PATH

# echo false:-\'false[$K]\'. >> $P1_PATH

# swipl -f "$MAIN" -g script,halt -- $P1_PATH $P2_PATH
from subprocess import call
call(['swipl','-g','script,halt','-f','kdim.pl','--','P0/fib.horn','3','P1/fib.horn'])