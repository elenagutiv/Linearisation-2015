#!/bin/python
# Gutierrez Viedma, Elena

# This file contains instructions to run a batch of tests located in directory P0. For each individual test, it generates P1 and P2 programs located in their
# respective  directories. Then, it runs QARMC to solve each. To show results a JSON file is generated.

import json,subprocess,os,time,re,sys
from subprocess import call
from glob import glob
from os.path import join

tests = glob(join('P0', '*.horn'))

sys.stdout = open(os.devnull,'a')

# Running swipl and generating programs P1 and P2 from P0: 

kdim="kdim.pl"
main="../main.pl"
k="2"
extraoptions = " -debug "
qarmc_timelimit = "600" # sec.
elp_timelimit = "600" # sec.

outfile = open('data.json', 'w')

for files in tests:

	elp_timeout = 0
	qarmc_timeout = 0
	qarmc_grep_error = 0
	passed = "PASSED"
	qarmc_time = "-1.0"

	f = os.path.basename(files)
	base = os.path.splitext(f)[0]

	p0_path="P0/"+base+".horn" ; p1_path="P1/"+base+".horn" ; p2_path="P2/"+base+".horn" # Set paths for P0,P1 and P2 programs

	# Build P1 program from P0 (KDIM)
	call(['swipl','-g','script,halt','-f',kdim,'--',p0_path,k,p1_path])
	
	fp = open(p1_path,'a') 
	print >> fp , "false:-\'false["+k+"]\'." # Add false clause to P1
	fp.close()
	
	begin_time_elp = int(round(time.time() * 1000))

	# Build P2 from P1 (ELP)
	try:
		output = subprocess.check_output(['time','gtimeout', elp_timelimit, 'swipl','-g','script,halt','-f',main,'--',p1_path,p2_path])
	except subprocess.CalledProcessError, e:
		if e.returncode == 124: #ELP TIMEOUT
			elp_timeout = 1
		else:
			sys.exit("Swipl command error: "+e.output)

	if elp_timeout == 0:
		end_time_elp = int(round(time.time() * 1000))
		elp_time = float(end_time_elp - begin_time_elp)/1000
		programs = [p0_path,p1_path,p2_path]
	else:
		programs = [p0_path,p1_path]
			 

	# Create JSON file with test results

	for file in programs:

		log_sufix = time.strftime(".%Y.%m.%d.%H.%M")
		logfile = file+log_sufix+".log"

		# Run QARMC 
		try:
			output = subprocess.check_output(["time gtimeout "+qarmc_timelimit+" ./qarmc-latest.osx " + extraoptions + file + " > " + logfile],shell = True)
		except subprocess.CalledProcessError,e:
			if e.returncode == 124: #QARMC TIMEOUT
				qarmc_timeout = 1
			else: 
				sys.exit("QARMC command error: "+e.output)

		if qarmc_timeout == 0:
			try:
				o_passed = subprocess.check_output(['grep'+" 'program is correct' "+logfile], shell = True)
			except subprocess.CalledProcessError,e:
				if e.returncode > 0:
					passed = "FAILED"
			try:
				o_time = subprocess.check_output(['grep'+" '\"total_time\":' " + logfile],shell = True)
			except subprocess.CalledProcessError,e:
				if e.returncode > 0:
					qarmc_time = "-1.0"
					qarmc_grep_error = 1

			if qarmc_grep_error == 0: # Extract QARMC time
				try:
				    qarmc_time = re.search('((\d+)\.(\d+))}}', o_time).group(1)
				except AttributeError:
					qarmc_time = "-1.0"
		else:
			passed = "QARMC_TIMEOUT"
			qarmc_time = "-1.0"

		if "P2" in file:
	  		data = {
		    'passed' : passed,
		    'k' : int(k),
		    'file' : file,
		    'ELP time(s)' : elp_time,
		    'QARMC time(s)' : float(qarmc_time)
		    }
		else:
			data = {
		    'passed' : passed,
		    'k' : int(k),
		    'file' : file,
		    'QARMC time(s)' : float(qarmc_time)
		    }
		json.dump(data, outfile,sort_keys=True,indent = 2)

	if elp_timeout==1:
		data = {
		    'passed' : "ELP_TIMEOUT",
		    'k' : int(k),
		    'file' : file,
		    'QARMC time(s)' : "-1.0"
		    }
		json.dump(data, outfile,sort_keys=True,indent = 2)
outfile.close()


