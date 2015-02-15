#!/bin/bash

# Clean up any leftover files (i.e. from previous runs)
rm -rf ebin test.out

# Find the simulation.erl, ignoring any top level directories
loc=`find . -type f -name simulation.erl`

# Get the directory containing the simulation.erl file
dname=`dirname $loc`

# Compile student files
# Specifically, all files with .erl extension
# in same directory as simulation.erl
mkdir ebin
erlc -o ./ebin $dname/*.erl

# Compile test file
# Should be in same directory as this script
erlc -o ./ebin test.erl

# Run specific test, test_public_basic
# Stores stderr output to test.out
erl -pa ./ebin -noshell -s test test_public_basic -s init stop 2> test.out

# Check for any error output
# which should indicate test failure
if [ -s test.out ]; then
	echo "Test FAILED"
else 
	echo "Test PASSED"
fi
