# Getting and cleaning data project

## Tasks

You should create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Script configuration

Just update the ```folder``` variable with the proper working directory and be sure to have write privileges on it as we will be download/creating files there.

Further details on Codebook.md and the script itself

## Dependencies

The script uses reshape2.

### Clean/processed data is on "cleanData.txt"