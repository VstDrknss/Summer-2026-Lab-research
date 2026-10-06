Steps to clean up data for analysis

	1. Get the excel files and Graphpad files, put them in the right folder
	2. Export each drug to its corresponding folder, in ser cleaned, select as input folder. 
	   select CLEAN as output folder
	3. Make sure means_combined_of_[drug] is present in the folder
	4. Run "folder_cleaning means.R" and select the drug folder
	5. Run "folder_means.R" and select the drug


Steps for "one click clean Biosensor"
	1. Have each baseline-corrected data of each drug exported from Graphpad to their corresponding drug folder
	2. Run the code while selecting the biosensor as input folder
