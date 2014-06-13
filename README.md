HDX Repo Dataset Checker
========================

Sometimes we have problems with the datasets that we generate to the HDX repository. This simple tool checks how many datasets have problems and creates a reporting interface.

The scripts are written using R the reporting interface uses `C3.js` to generate graphs.


Tests
-----

The following tests are conducted:

**Test 1**
*This tests if the downloads are consistent.*
- in terms of time: how long it takes for a dataset to download.
	- get the size of the file
	- get the time it took to download
		- generate the indicator: speed per mb.

**Test 2**
- in terms of http response header:
	- get the response header:
		- success or failure bloolean
		- actual HTTP header

**Test 3**
*Check if the datasets have data on them or not.*
- check if they are coming blank or have data.
