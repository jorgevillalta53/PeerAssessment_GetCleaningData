Code Book:
=========

Data Set Information:
======================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Feature Info 
============

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ, tGravityAcc-XYZ, tBodyAccJerk-XYZ, tBodyGyro-XYZ, tBodyGyroJerk-XYZ, tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag, fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccMag, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value

std(): Standard deviation

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean

VARIABLE AND TRANSFORMATIONS
=============================

VARIABLES & FILES:

- features - "UCI HAR Dataset/features.txt": The information variables that were measured
- act_label - "UCI HAR Dataset/activity_labels.txt": The information of the activities that was made
TEST INFORMATION
- y_test - "UCI HAR Dataset/test/y_test.txt": The information of the testing activities for each row 
- x_test - "UCI HAR Dataset/test/x_test.txt": The information of  measured test (2947 rows)
- subject_test - "UCI HAR Dataset/test/subject_test.txt": The information of the subject that made the test
TRAINING INFORMATION
- y_train - "UCI HAR Dataset/train/y_train.txt": The information of the training activities for each row 
- x_train - "UCI HAR Dataset/train/x_train.txt": The information of  measured train (7352 rows)
- subject_train - "UCI HAR Dataset/train/subject_train.txt": The information of the subject that made the training

SCRIPT's VARIABLES:

- names(x_test) & names(x_train) : store the name that we extract from feature file
- x_test_f &  x_train_f: store the combine information of subject, y test/train and x test/train
- x_all: the complete info that we merged with train and test data (10299 rows & 563 variables)
- features_wanted: extract the variable of mean-desvest est info
- col_index: store the index of the mean-desvest variable
- x_general: store the tidy data set with only the information that we are going to analyze (10299 rows & 68 variables)
- x_final: store the tidy data & the activities information (FINAL TIDY DATA SET) (10299 rows & 69 variables)

- x_melt : store the data melt that we are going to summarize
- summ_X: store de final summarize data (180 observations & 68 variables)
