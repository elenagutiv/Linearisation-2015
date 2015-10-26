#!/bin/python

import sys, getopt,subprocess
from subprocess import call

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

call(['swipl','-g','script,halt','-f',kdim,'--',p0_path,k,p1_path])

fp=open(p1_path,'a') 
print >> fp , "false:-\'false["+k+"]\'."
fp.close()

call(['swipl','-g','script,halt','-f',main,'--',p1_path,p2_path])

