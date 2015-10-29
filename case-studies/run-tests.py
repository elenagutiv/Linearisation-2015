#!/bin/python

import json,subprocess,os,time,re
from subprocess import call
from glob import glob
from os.path import join

tests = glob(join('P0', 'fib.horn'))

# Running swipl and generating programs P1 and P2 from P0: 

kdim="kdim.pl"
main="../main.pl"
k="2"
extraoptions = " -debug "
qarmc_timelimit = "60"
elp_timelimit = "60"

elp_timeout = 0
qarmc_timeout = 0


outfile = open('data.json', 'w')

for files in tests:

	f = os.path.basename(files) # Extract the name of the file
	base = os.path.splitext(f)[0] # Split name an extension from the file name. Assign name to base

	p0_path="P0/"+base+".horn" ; p1_path="P1/"+base+".horn" ; p2_path="P2/"+base+".horn"

	call(['swipl','-g','script,halt','-f',kdim,'--',p0_path,k,p1_path])

	fp=open(p1_path,'a') 
	print >> fp , "false:-\'false["+k+"]\'."
	fp.close()
	
	begin_time_elp = int(round(time.time() * 1000))

	try:
		output = subprocess.check_output(['time','gtimeout', elp_timelimit, 'swipl','-g','script,halt','-f',main,'--',p1_path,p2_path])
	except subprocess.CalledProcessError, e:
		if e.returncode == 124: #ELP TIMEOUT
			elp_timeout = 1
		else:
			print "Swipl command error: "+e.output

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

		try:
			output = subprocess.check_output(["time gtimeout "+qarmc_timelimit+" ./qarmc-latest.osx " + extraoptions + file + " > " + logfile],shell = True)
		except subprocess.CalledProcessError,e:
			if e.returncode == 124: #QARMC TIMEOUT
				qarmc_timeout = 1
			else:
				print "QARMC command error: "+e.output

		if qarmc_timeout == 0:
			o_passed = subprocess.check_output(['grep'+" 'program is correct' "+logfile], shell = True)
			o_time = subprocess.check_output(['grep'+" '\"total_time\":' " + logfile],shell = True)

			if o_passed == 1 :
				passed = "FAILED"
			else:
				passed = "PASSED"

			# Extract QARMC time
			try:
			    qarmc_time = re.search('((\d+)\.(\d+))}}', o_time).group(1)
			except AttributeError:
			    qarmc_time = ''
		else:
			passed = "QARMC_TIMEOUT"
			qarmc_time = "-1.0"

		if "P2" in file:
	  		data = {
		    'passed' : passed,
		    'k' : int(k),
		    'file' : file,
		    'ELP time(s)' : elp_time,
		    'QARMC time' : float(qarmc_time)
		    }
		else:
			data = {
		    'passed' : passed,
		    'k' : int(k),
		    'file' : file,
		    'QARMC time' : float(qarmc_time)
		    }
		json.dump(data, outfile,sort_keys=True,indent = 2)

	if elp_timeout==1:
		data = {
		    'passed' : "ELP_TIMEOUT",
		    'k' : int(k),
		    'file' : file,
		    'QARMC time' : "-1.0"
		    }
		json.dump(data, outfile,sort_keys=True,indent = 2)
outfile.close()


