#!/usr/bin/env python3
# Gutierrez Viedma, Elena

# SCATTER PLOT DATA GENERATION SCRIPT

# This file contains instructions to run a batch of tests located in directory programs. For each individual test, it generates P1 and linear-programs programs located in their
# respective  directories P1/ and linear-programs/ (created during execution). Then, it runs QARMC to solve programs, P1 and linear-programs in order to collect the tool answer and running times for each.
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


# USER OPTIONS #

tests = glob(join('../programs', '*.horn'))

ks=[1,2,3,4,5,6,7,8]

linearisation_file = "../../src/linearise.pl"
linearisation_exe = os.path.splitext(linearisation_file)[0]

qarmc_filename = "./qarmc-latest.osx" # Change executable filename if needed.
extraoptions = " -debug "
qarmc_timelimit = "15" # sec.

elp_timelimit = "15" # sec.
pe_timelimit = "15" #sec.

YAMLformatfile = '../../plot-scripts/running-times.yml'
JSONformatfile = '../results/running-times.json'

N=0 #number of tests in JSON output
n_elp_timeouts = 0
n_pe_timeouts = 0
data = []

################

print("Running tests...")
print('k-values = [%s]' % ', '.join(map(str, ks)))
print("QARMC time limit = ",qarmc_timelimit)
print("ELP time limit = ",elp_timelimit)
print("PE time limit = ",pe_timelimit)
print()

output = subprocess.Popen(['mkdir','../linear-programs'], stdout = subprocess.PIPE, stderr = subprocess.PIPE)

if not os.path.exists(os.path.dirname(JSONformatfile)):
    os.makedirs(os.path.dirname(JSONformatfile))
JSONoutfile = open(JSONformatfile, 'w+')

YAMLoutfile = open(YAMLformatfile, 'w+')

# Compile linearise .pl
call(['ciaoc', linearisation_file])

for files in tests:
	qarmc_time = [None]*2

	for k in ks:
		k = str(k)
		elp_timeout = 0
		pe_timeout = 0
		i = 0

		f = os.path.basename(files)
		base = os.path.splitext(f)[0]

		p0_path = "../programs/" + base + ".horn"; ELP_p2_path = "../linear-programs/"+"ELP_"+ base + ".horn"; PE_p2_path = "../linear-programs/" +"PE_"+ base + ".horn"

		# Run ELP
		try:
			output = subprocess.call('time gtimeout ' + elp_timelimit + ' ./' + linearisation_exe + ' -prg ' + p0_path + ' -k ' + k + ' -o ' + ELP_p2_path, timeout = float(elp_timelimit), shell = True )
		except subprocess.TimeoutExpired:
			elp_timeout = 1
		if elp_timeout == 1:
			n_elp_timeouts+=1
			print("[ELP_TIMEOUT,",f,",k=",k,"]","	Discarded")
		else:
			fp = open(ELP_p2_path,'a') 
			print("false:-\'false["+k+"]\'.", file = fp)# Add false clause to linear-programs
			fp.close()

		# Run PE
		try:
			output = subprocess.call('time gtimeout ' + pe_timelimit + ' ./' + linearisation_exe + ' -prg ' + p0_path + ' -k ' + k + ' -o ' + PE_p2_path + ' -pe', timeout = float(pe_timelimit), shell = True )
		except subprocess.TimeoutExpired:
			pe_timeout = 1
		if pe_timeout==1:
			n_pe_timeouts+=1
			print("[PE_TIMEOUT,",f,",k=",k,"]","	Discarded")

		if (pe_timeout==0 and elp_timeout==0):
			programs = [ELP_p2_path,PE_p2_path]
		else:
			continue

		for file in programs:
			qarmc_timeout = 0
			qarmc_grep_error = 0
			qarmc_time_regex_error = 0
			error = 0

			log_sufix = time.strftime(".%Y.%m.%d.%H.%M")
			logfile = file+log_sufix+".log"

			print(file)

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

		if (error == 0) :
			d = {
				'N' : N,
			    'k' : int(k),
			    'file' : base,
			    'qarmctime_elp' : qarmc_time[0],
			    'qarmctime_pe' : qarmc_time[1]
			    }
			data.append(d)
			N+=1
			print(f,"	Done")
		else:
			print(f,"	Discarded"	)

d = {
	    'ks' : len(ks),
	    'qarmc_timelimit' : int(qarmc_timelimit),
	    'elp_timelimit': int(elp_timelimit),
	    'pe_timelimit': int(pe_timelimit),
	    'n_elp_timeouts' : n_elp_timeouts,
	    'n_pe_timeouts' : n_pe_timeouts
	}
data.append(d)


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
print("ELP timeouts: ",n_elp_timeouts)
print()
print("PE timeouts: ",n_pe_timeouts)
print()
print("JSON format output in ",JSONformatfile)
print()
print("YAML format output in ",YAMLformatfile)


