#!/usr/bin/env python3
# Gutierrez Viedma, Elena

# SCATTER PLOT DATA GENERATION SCRIPT

# This file contains instructions to run a batch of tests located in directory P0. For each individual test, it generates P1 and P2 programs located in their
# respective  directories P1/ and P2/ (created during execution). Then, it runs QARMC to solve P0, P1 and P2 in order to collect the tool answer and running times for each.
# Two files are generated as output with esentially the same content but different formats:

# A .yml file in ../../plot-scripts/running-times.yml
# A .json file in /results/running-times.json.

# Check Python version > 3: 
import sys
if sys.hexversion < 0x03000000:
    sys.exit("Python 3 or newer is required to run this program.")

import json,subprocess,os,time,re,sys,fileinput
from subprocess import call
from glob import glob
from os.path import join

kdim="kdim.pl"
main="../../Attic/main.pl"

# USER OPTIONS #

tests = glob(join('../P0', '*.horn'))

ks=[1,2,3,4,5]

qarmc_filename = "./qarmc-latest.osx" # Change executable filename if needed.
extraoptions = " -debug "
qarmc_timelimit = "8" # sec.

elp_timelimit = "15" # sec.

YAMLformatfile = '../../plot-scripts/running-times.yml'
JSONformatfile = '../results/running-times.json'

N=0 #number of tests in JSON output
data = []

################

print("Running tests...")
print('k-values = [%s]' % ', '.join(map(str, ks)))
print("QARMC time limit = ",qarmc_timelimit)
print("ELP time limit = ",elp_timelimit)
print()

output = subprocess.Popen(['mkdir','../P1','../P2'], stdout = subprocess.PIPE, stderr = subprocess.PIPE)

if not os.path.exists(os.path.dirname(JSONformatfile)):
    os.makedirs(os.path.dirname(JSONformatfile))
JSONoutfile = open(JSONformatfile, 'w')

YAMLoutfile = open(YAMLformatfile, 'w+')

for files in tests:
	passed = [None]*3
	qarmc_time = [None]*3
	run_P0 = 0 # 1 if P0 runtime has been measured for P0 = files, 0 if not.

	for k in ks:

		k = str(k)
		elp_timeout = 0
		i = 0

		f = os.path.basename(files)
		base = os.path.splitext(f)[0]

		p0_path="../P0/"+base+".horn" ; p1_path="../P1/"+base+".horn" ; p2_path="../P2/"+base+".horn" # Set paths for P0,P1 and P2 programs

		# Build P1 program from P0 (KDIM)
		call(['swipl','-g','script,halt','-f',kdim,'--',p0_path,k,p1_path])
		
		fp = open(p1_path,'a') 
		print("false:-\'false["+k+"]\'.", file = fp)# Add false clause to P1
		fp.close()
		
		begin_time_elp = int(round(time.time() * 1000))

		# Build P2 from P1 (ELP)
		try:
			output = subprocess.call('time gtimeout '+elp_timelimit+' swipl -g script,halt -f '+main+' -- '+p1_path+' '+p2_path, timeout = float(elp_timelimit), shell = True)
		except subprocess.TimeoutExpired:
			elp_timeout = 1
			print("ELP TIMEOUT")

		if elp_timeout == 0:
			end_time_elp = int(round(time.time() * 1000))
			elp_time = float(end_time_elp - begin_time_elp)/1000
			if run_P0 == 0:
				programs = [p0_path,p1_path,p2_path]
			else:
				i = 1 # passed[0] and qarmc_time[0] already have a value
				programs = [p1_path,p2_path] # if P0 runtime has been measured, we do not include it in programs.
		else:
			print(f,"	Discarded")
			continue

		# Create JSON file with test results

		for file in programs:

			qarmc_timeout = 0
			qarmc_grep_error = 0
			qarmc_time_regex_error = 0
			error = 0
			log_sufix = time.strftime(".%Y.%m.%d.%H.%M")
			logfile = file+log_sufix+".log"

			passed[i] = 1

			# Run QARMC 
			try:
				output = subprocess.check_output(["time gtimeout "+qarmc_timelimit+" "+qarmc_filename+" " + extraoptions + file + " > " + logfile], shell = True, stderr = subprocess.STDOUT)
			except subprocess.CalledProcessError as e:
				if e.returncode == 124: #QARMC TIMEOUT
					qarmc_timeout = 1
					print("qarmc TIMEOUT")
				else: 
					sys.exit("QARMC command error")

			if qarmc_timeout == 0:
				try:
					o_passed = subprocess.check_output(['grep'+" 'program is correct' "+logfile], shell = True)
				except subprocess.CalledProcessError as e:
					if e.returncode > 0:
						passed[i] = 0
				try:
					o_time = subprocess.check_output(['grep'+" '\"total_time\":' " + logfile],shell = True)
				except subprocess.CalledProcessError as e:
					if e.returncode > 0:
						qarmc_grep_error = 1
						break

				if qarmc_grep_error == 0: # Extract QARMC time
					try:
						 qarmc_time[i] = float(re.search('((\d+)\.(\d+))}}', str(o_time)).group(1))
					except AttributeError:
						 qarmc_time_regex_error = 1
			else:
				qarmc_time[i]=int(qarmc_timelimit)+0.1

			if (qarmc_grep_error == 1 or qarmc_time_regex_error ==1):
				error=1
				break
			i+=1
			run_P0 = 1 # P0 runtime has already been measured

		if (error == 0) :
			d = {
				'N' : N,
			    'passed0' : passed[0],
			    'passed1' : passed[1],
			    'passed2' : passed[2],
			    'k' : int(k),
			    'file' : base,
			    'qarmctime0' : qarmc_time[0],
			    'qarmctime1': qarmc_time[1],
			    'qarmctime2': qarmc_time[2]
			    }
			data.append(d)
			N+=1
			print(f,"	Done")
		else:
			print(f,"	Discarded"	)

	
json.dump(data, JSONoutfile, sort_keys=True, indent = 2)
JSONoutfile.close()
json.dump(data, YAMLoutfile, sort_keys=True, indent = 2)
YAMLoutfile.close()

#Formating YAMLformatfile to be processed by mustache:

with fileinput.FileInput(YAMLformatfile, inplace=True) as file:
	for line in file:
		print (line.replace ('\n',""), end = '')

with fileinput.FileInput(YAMLformatfile, inplace=True) as file:
	for line in file:
			print (line.replace ('\"',"\\\""), end = '')

with fileinput.FileInput(YAMLformatfile, inplace=True) as file:
	for first in file:
			print (first.replace ('[','{\"translator\" :[{\"data\": \"['), end = '')

with fileinput.FileInput(YAMLformatfile, inplace=True) as file:
	for first in file:
			print (first.replace (']',']\"}]}'), end = '')

print("Sample size: ",N)
print()
print("JSON format output in ",JSONformatfile)
print()
print("YAML format output in ",YAMLformatfile)


