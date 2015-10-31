#!/bin/python
# Gutierrez Viedma, Elena

# This file contains instructions to run a batch of tests located in directory P0. For each individual test, it generates P1 and P2 programs located in their
# respective  directories. Then, it runs QARMC to solve each. To show results a JSON file is generated.

import json,subprocess,os,time,re,sys
from subprocess import call
from glob import glob
from os.path import join

kdim="kdim.pl"
main="../../src/main.pl"

# USER OPTIONS #

tests = glob(join('../P0', '*.horn'))
k="2"
extraoptions = " -debug "
qarmc_timelimit = "10" # sec.
elp_timelimit = "10" # sec.
JSONfile = '../results/k'+k+'.json'

################

print "Running tests..."
print "k = "+k
print "QARMC time limit = "+qarmc_timelimit
print "ELP time limit = "+elp_timelimit
print ""

output = subprocess.Popen(['mkdir','../P1','../P2'], stdout = subprocess.PIPE, stderr = subprocess.PIPE)
outfile = open(JSONfile, 'w')

for files in tests:

	elp_timeout = 0
	
	f = os.path.basename(files)
	base = os.path.splitext(f)[0]

	p0_path="../P0/"+base+".horn" ; p1_path="../P1/"+base+".horn" ; p2_path="../P2/"+base+".horn" # Set paths for P0,P1 and P2 programs

	# Build P1 program from P0 (KDIM)
	call(['swipl','-g','script,halt','-f',kdim,'--',p0_path,k,p1_path])
	
	fp = open(p1_path,'a') 
	print >> fp , "false:-\'false["+k+"]\'." # Add false clause to P1
	fp.close()
	
	begin_time_elp = int(round(time.time() * 1000))

	# Build P2 from P1 (ELP)
	try:
		output = subprocess.Popen(['time','gtimeout', elp_timelimit, 'swipl','-g','script,halt','-f',main,'--',p1_path,p2_path], stdout = subprocess.PIPE, stderr = subprocess.PIPE)
	except subprocess.CalledProcessError, e:
		if e.returncode == 124: #ELP TIMEOUT
			elp_timeout = 1
		else:
			sys.exit("Swipl command error")

	if elp_timeout == 0:
		end_time_elp = int(round(time.time() * 1000))
		elp_time = float(end_time_elp - begin_time_elp)/1000
		programs = [p0_path,p1_path,p2_path]
	else:
		programs = [p0_path,p1_path]
			 

	# Create JSON file with test results

	for file in programs:

		qarmc_timeout = 0
		qarmc_grep_error = 0
		passed = "PASSED"
		qarmc_time = -1.0
		log_sufix = time.strftime(".%Y.%m.%d.%H.%M")
		logfile = file+log_sufix+".log"

		# Run QARMC 
		try:
			output = subprocess.check_output(["time gtimeout "+qarmc_timelimit+" ./qarmc-latest.osx " + extraoptions + file + " > " + logfile], shell = True, stderr = subprocess.STDOUT)
		except subprocess.CalledProcessError,e:
			if e.returncode == 124: #QARMC TIMEOUT
				qarmc_timeout = 1
			else: 
				sys.exit("QARMC command error")

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
					qarmc_grep_error = 1

			if qarmc_grep_error == 0: # Extract QARMC time
				try:
				    qarmc_time = float(re.search('((\d+)\.(\d+))}}', o_time).group(1))
				except AttributeError:
					qarmc_time = -1.0
		else:
			passed = "QARMC_TIMEOUT"

		if "P2" in file:
	  		data = {
		    'passed' : passed,
		    'k' : int(k),
		    'file' : file,
		    'ELP time(s)' : elp_time,
		    'QARMC time(s)' : qarmc_time
		    }
		else:
			data = {
		    'passed' : passed,
		    'k' : int(k),
		    'file' : file,
		    'QARMC time(s)' : qarmc_time
		    }
		json.dump(data, outfile,sort_keys=True,indent = 2)

	if elp_timeout==1:
		data = {
		    'passed' : "ELP_TIMEOUT",
		    'k' : int(k),
		    'file' : file,
		    'QARMC time(s)' : -1.0
		    }
		json.dump(data, outfile,sort_keys=True,indent = 2)

	print f+"	Done"
outfile.close()

print ""
print "JSON output in "+ JSONfile


