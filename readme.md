#Within this folder is the script, codebook and final product of clean and tidy data.

codebook.txt contains descriptions of the variable names for "cleanData.txt"

cleanData.txt is the final dataset required to be produced. The file is tab-delimited and contains a header.

run_analysis.R is the R script used to read, manipulate and write the final dataset.

	The script starts by reading in all necessary files (features.txt, activity_labels.txt, test/X_test.txt, test/y_test.txt, etc)
	Next, the test and training files are combined using the cbind f(x). Column names are pulled from features.txt. the training and test
		datasets are then combined with rbind.
	The activity column is converted from ID# to word description using the activity_labels.txt and mapvalue (plyr package function)
	Fourth, means and standard deviations for the raw measurements were extracted to a new long tidy table for each subject/activity/observation
	Fifth, all measurements for a subject's activity were averaged and outputed to another new datatable.
		this data table was written to cleanData.txt
		
I opted for a long/skinny data table as those to me are most easiliy manipulated in R for things like aggregation and summarizing. The long skinny could
be convereted to a wide and tidy using dcast or cast functions (part of the reshape2 and reshape packages, respectively).
