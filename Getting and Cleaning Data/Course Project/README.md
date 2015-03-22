##README for run_analysis.R

run_analysis.R requires the reshapse2 package installed.

The run_analysis.md script reads in Galaxy phone signals from the UCI HAR Dataset provided with the script.
The UCI HAR Dataset directory must be included in the working directory of the script for the script to run correctly.

The run_analysis script perfoms the following steps:
* Read in helper name vectors
	* Signal "feature" names.
	* Activity names
- Read in full training data
	- Read raw signal data
	- Read subject mapping
	- Read activity mapping
	- CBind subject and activity maps to raw data
- Read in full testing data
	- Read raw signal data
	- Read subject mapping
	- Read activity mapping
- CBind training and testing data together
	- Append subject and activity labels to the bound data
- Parse only mean and STD columns from the merged dataset
- Replace activity codes with activity string labels
- Transform the parsed dataset into subject/activity mean values
- Output results
		
		
The resulting dataset is the mean values for variables in Activity/Subject combinations.
