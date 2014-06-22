## Hey! my name is JORGE ALBERTO VILLALTA MONTENEGRO
## This script create one R does the following: 
##1.Merges the training and the test sets to create one data set.
##2.Extracts only the measurements on the mean and standard deviation for each measurement. 
##3.Uses descriptive activity names to name the activities in the data set
##4.Appropriately labels the data set with descriptive variable names. 
##5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## Thanks! and nice to meet you!
#############################################################################
run_analysis <- function() { ## initial function
 ## read all the files that is containt in the 
 ## directory of "UCI HAR Dataset"
  #####################################################
  ## This are the files that contains the general information
  features<-read.table("UCI HAR Dataset/features.txt")
  act_label<-read.table("UCI HAR Dataset/activity_labels.txt")
  ## This are the files that contains the test information
  y_test<-read.table("UCI HAR Dataset/test/y_test.txt")
  x_test<-read.table("UCI HAR Dataset/test/x_test.txt")
  subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
  ## This are the files that contains the training information
  y_train<-read.table("UCI HAR Dataset/train/y_train.txt")
  x_train<-read.table("UCI HAR Dataset/train/x_train.txt")
  subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")
  #######################################################
  ## PUTTING THE NAMES OF THE COLUMNS (VARIABLES)
  names(x_test)<-features[,2]
  names(x_train)<-features[,2]
  #######################################################
  ## CONSTRUCT THE 2 COMPLETE DATA FRAMES (TRAINING AND TEST)
  x_test_f<-cbind(subject_test[,1],y_test,x_test) ## join subject, activiy & data info
  x_train_f<-cbind(subject_train[,1],y_train,x_train)
  names(x_train_f)[1:2]<-c("subject_id","activity_id")## putting the names at the 
  names(x_test_f)[1:2]<-c("subject_id","activity_id") ## new columns_id
  #######################################################
  ## CREATE THE GENERAL DATA FRAME (MERGE TEST AND TRAIN INFO)
  x_all<-rbind(x_test_f,x_train_f)
  #######################################################
  ## EXTRACT THE INDEX OF THE COLUMNS WITH MEAN_DESVEST VARIBLE COLUMNS 
  features_wanted<-features[grep("mean\\(\\)|std\\(\\)",features$V2),] ## got the columns with mean & desvest info
  col_index<-c(1,2,(features_wanted[[1]]+2)) ## columns to extract of the general data frame
  ###################### CREATE THE FINAL TIDY DATA FRAME
  x_general<-x_all[,col_index] ## data frame with only the mean and desvest info
  ###################### JOIN WITH THE INFO ACTIVITY
  x_final<-merge(x_general,act_label,by.x="activity_id",by.y="V1",all=TRUE)
  ###################### PUTTING THE ACTIVITY's NAME
  names(x_final)[69]<-"activity_name" ## at the moment to do the merge, it create a new column with the name of the activity
  ###################### SUMMARIZE THE DATA
  library(reshape2) ## load reshape2 library
  x_melt<-melt(x_final,id=c("activity_name","subject_id"),measure.vars=names(x_final)[seq(from=3,to=68)]) ## melt the info
  summ_X<-dcast(x_melt,subject_id+activity_name~names(x_final)[seq(from=3,to=68)],mean) ## using dcast for make the summary
  write.table(summ_X, 'UCI HAR Dataset/summ_X.txt', sep='\t') ## create the new text file with the summary info
  message("Process finish, file creates on /UCI HAR Dataset/ ") ## gives a message when the process finishes
} ## final function
