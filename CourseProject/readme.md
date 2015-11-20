# this file explains the logic adopted for the Run_Analysis.R that performs the cleaning up of the dataset and merging activities 

# Download the files into the local directory.
# Upon analysis, there are two main folders - Test / Train which contain the files for the required purpose.
# the main folder has the readme, features.txt and activity labels which are requried for the assignment

# First, Perform the operations on Test Folder. 
# Certain tasks can be resused for Train Datasets. 

#1) Include library dplyr, tidyr

# 2. Read the Test Data Set one by one
# 3At this point all the files are loaded into R, as seperate tables. all of them have 2947 observations.

# 4. only choose required columns using the Contains Option

# 5. Covert the data sets into tbl_df, so that we can tidy up the data.
# 6. Validate this data set. Check row count is same.

# 7. Add Descriptive Variable Name. Find columns with mean and std, and then create a table of the same.

# 8 Write out the variable names into a XLS file


# 9 Update the Variable name in XLS and upload back into another data frame. 

# 10. assign the Variable name using colnames function.


# 11 cbind the tables to include all the columns in one table

# 12 Converty Activity Type to be Descriptive.
# 13 Include Spatstat Package to enable use of Lut Function.
# 14. Create final Dataset by using cbind of the Activity type table, and Subjects table. 


# 15. Repeat same steps for Train Dataset 


# 16. Merge the two Datasets using rbind. both  the datasets have 88 columns and then different number of observations. All are now combined into one Dataset.


# 17. Create Another independent Data set , removing Variables for Gyro, or one's containing XYZ Axis measurmenets, Jerk Signals and Frequency Signals
# Resultant Data set has only Acceleration Time Domain Signals for Analysis. 
# We can create multiple subsets for each of these types and establish co-relation with one another. 
# 18. Group by Activity Type and Subject using group-by function on the merged data set for activity and subject followed by summarize each to get the mean of all the variables. 

# 19. Write the output of the last Dataset into a txt file, with row names = FALSE











