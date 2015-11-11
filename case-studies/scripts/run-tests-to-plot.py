#!/bin/python
# Gutierrez Viedma, Elena

# SCATTER PLOT DATA GENERATION SCRIPT

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
qarmc_timelimit = "2" # sec.
elp_timelimit = "2" # sec.
JSONfile = '../results/plot_'+k+'.json'
N=0 #number of tests in JSON output
data = []

################

print "Running tests..."
print "k = "+k
print "QARMC time limit = "+qarmc_timelimit
print "ELP time limit = "+elp_timelimit
print ""

output = subprocess.Popen(['mkdir','../P1','../P2'], stdout = subprocess.PIPE, stderr = subprocess.PIPE)
outfile = open(JSONfile, 'w+')

for files in tests:

	elp_timeout = 0
	i = 0
	passed = [None]*2
	qarmc_time = [None]*2
	
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
		programs = [p1_path,p2_path] # Only care about runtimes of P1 and P2 to build scatterplot
	else:
		print f+"	Discarded"
		break # If P2 is not generated, we discard  P1 as well
			 

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
					passed[i] = 0
			try:
				o_time = subprocess.check_output(['grep'+" '\"total_time\":' " + logfile],shell = True)
			except subprocess.CalledProcessError,e:
				if e.returncode > 0:
					qarmc_grep_error = 1
					break

			if qarmc_grep_error == 0: # Extract QARMC time
				try:
				    qarmc_time[i] = float(re.search('((\d+)\.(\d+))}}', o_time).group(1))
				except AttributeError:
					 qarmc_time_regex_error = 1
		else:
			qarmc_time[i]=int(qarmc_timelimit)+1

		if (qarmc_grep_error == 1 or qarmc_time_regex_error ==1):
			error=1
			break
		i+=1

	if (error == 0) :
		d = {
			'N' : N,
		    'passed1' : passed[0],
		    'passed2' : passed[1],
		    'k' : int(k),
		    'file' : file,
		    'qarmctime1' : qarmc_time[0],
		    'qarmctime2': qarmc_time[1]
		    }
		data.append(d)
		N+=1
		print f+"	Done"
	else:
		print f+"	Discarded"	

json.dump(data, outfile, sort_keys=True, indent = 2)
outfile.close()

print ""
print "JSON output in "+ JSONfile


