#This script follows all the steps as provided in the documentation 
#of the asignment of the 4th week for getting and clean data


#clearing the workspace
rm(list = ls())

#Establish if the variables of the workspace are going to be removed when not used
rmflag <- TRUE

library(reshape2)

###A. Declaration of filename variables and separator

  #Separator of the platform
  fsep <- .Platform$file.sep
  #working directories
  data.directory <- "UCI HAR Dataset"
  training.directory <- "train"
  test.directory <- "test"
  
  #Working files 
  activity_labels.filename <- "activity_labels.txt"
  features.filename <- "features.txt"
  #training set files
  training.set.filename <- "X_train.txt";
  training.labels.filename <- "y_train.txt"
  training.subjects.filename <- "subject_train.txt"
  #test set files
  test.set.filename <- "X_test.txt";
  test.labels.filename <- "y_test.txt"
  test.subject.filename <- "subject_test.txt"

###B. Load activity labels from file

  #activity labels complete file path  
  activity_labels.complete_path <- paste(data.directory, activity_labels.filename, sep = fsep)
  
  #if the file does not exist in the specified path, script stops
  if(file.exists(activity_labels.complete_path )==FALSE){
    stop("Activity Labels file (activity_labels.txt) is not present")}
  
  #reading data 
  activity_labels.data <- read.table(activity_labels.complete_path)
  
  #casting factor into characters
  activity_labels.data$V2 <- as.character(activity_labels.data$V2)
  
  if(rmflag == TRUE){
  #removing unused variables
  rm(activity_labels.complete_path, activity_labels.filename)}

###C. Load features from file

  #features complete file path  
  features.complete_path <- paste(data.directory, features.filename, sep = fsep)
  
  #if the file does not exist in the specified path, script stops
  if(file.exists(features.complete_path )==FALSE){
    stop("Features file (features.txt) is not present")}
  
  #reading data 
  features.data <- read.table(features.complete_path)
  
  #casting factor into characters
  features.data$V2 <- as.character(features.data$V2)
  
  if(rmflag == TRUE){
  #removing unused variables
  rm(features.complete_path)}

###D. Extracts only the measurements on the mean and standard deviation for each measurement.
  
  features.data.selection <- grep(".*mean.*|.*std.*", features.data$V2)
  features.data.selected_names <- features.data$V2[features.data.selection]
  features.data.names <- gsub('[()]', '', features.data.selected_names)
  
  if(rmflag == TRUE){
  #removing unused variables
  rm(features.data, features.data.selected_names, features.filename)}
  
###E. Loading the training files and binding them
  
  ##Loading the training data
  training.set.complete_path <- paste(data.directory, training.directory, training.set.filename, sep = fsep)
  #if the file does not exist in the specified path, script stops
  if(file.exists(training.set.complete_path )==FALSE){
    stop("Training set file (X_train.txt) is not present")}
  training.set.data <- read.table(training.set.complete_path)[features.data.selection]

  ##Loading the training labels
  training.labels.complete_path <- paste(data.directory, training.directory, training.labels.filename, sep = fsep)
  #if the file does not exist in the specified path, script stops
  if(file.exists(training.labels.complete_path )==FALSE){
    stop("Training label file (Y_train.txt) is not present")}
  training.labels.data <- read.table(training.labels.complete_path)
  
  ##Loading the training labels
  training.subject.complete_path <- paste(data.directory, training.directory, training.subjects.filename, sep = fsep)
  #if the file does not exist in the specified path, script stops
  if(file.exists(training.subject.complete_path )==FALSE){
    stop("Training subject file (subject_train.txt) is not present")}
  training.subject.data <- read.table(training.subject.complete_path)
  
  #Binding the data by columns
  final.training.data <- cbind(training.subject.data, training.labels.data, training.set.data)

  if(rmflag==TRUE){
  #removing unused variables
  rm(training.directory, training.set.filename,
     training.subjects.filename,training.labels.filename,
    training.set.complete_path,training.set.data,
     training.labels.complete_path,training.labels.data,
     training.subject.complete_path,training.subject.data)}
  
###F. Loading the test files and binding them
  
  ##Loading the test data
  test.set.complete_path <- paste(data.directory, test.directory, test.set.filename, sep = fsep)
  #if the file does not exist in the specified path, script stops
  if(file.exists(test.set.complete_path )==FALSE){
    stop("Test set file (X_test.txt) is not present")}
  test.set.data <- read.table(test.set.complete_path)[features.data.selection]
  
  ##Loading the test labels
  test.labels.complete_path <- paste(data.directory, test.directory, test.labels.filename, sep = fsep)
  #if the file does not exist in the specified path, script stops
  if(file.exists(test.labels.complete_path )==FALSE){
    stop("Test label file (Y_test.txt) is not present")}
  test.labels.data <- read.table(test.labels.complete_path)
  
  ##Loading the test labels
  test.subject.complete_path <- paste(data.directory, test.directory, test.subject.filename, sep = fsep)
  #if the file does not exist in the specified path, script stops
  if(file.exists(test.subject.complete_path )==FALSE){
    stop("Training subject file (subject_test.txt) is not present")}
  test.subject.data <- read.table(test.subject.complete_path)
  
  #Binding the data by columns
  final.test.data <- cbind(test.subject.data, test.labels.data, test.set.data ) 
  
  if(rmflag==TRUE){
  #removing unused variables
  rm(data.directory, fsep, features.data.selection,
     test.directory, test.set.filename,
     test.subject.filename, test.labels.filename,
    test.set.complete_path,test.set.data,
     test.labels.complete_path,test.labels.data,
     test.subject.complete_path,test.subject.data)}
  
###G. Merging the training and test data into a Whole Dataset and convert the variables: 
  #"Subject" and "Activity_label" into factors. The numerical values of the column "Activity_label"
  #are substituted by the activities labels extracted at the point B.
  
  #Binding the data by rows
  Whole_Dataset <- rbind(final.training.data, final.test.data)
  
  #Names asignment to the columns of the table 
  colnames(Whole_Dataset) <- c("Subject", "Activity_label",features.data.names)
  
  #Substituting the activity label number by the actual name and making the variables being factors
  Whole_Dataset$Subject <-as.factor(Whole_Dataset$Subject)
  Whole_Dataset$Activity_label <- factor(Whole_Dataset$Activity_label, 
                                        levels = activity_labels.data$V1,
                                        labels = activity_labels.data$V2)
  #removing unused variables
  if(rmflag==TRUE){
  rm(activity_labels.data,final.training.data, final.test.data, features.data.names)}
  
###H. Melting the data and producing the average (mean) for every variable and activity label
  
  Whole_Dataset.melted <- melt(Whole_Dataset, id = c("Subject", "Activity_label"))
  Whole_Dataset.mean <- dcast(Whole_Dataset.melted, Subject + Activity_label ~ variable, mean)

  if(rmflag==TRUE){
  #removing unused variables
  rm(Whole_Dataset.melted, Whole_Dataset)}
  
###I. Writing the tidy file (tidy.txt) in the "R" working directory  
  write.table(Whole_Dataset.mean, "tidy.txt", col.names = TRUE, row.names = FALSE, quote = FALSE)

  rm(rmflag)
  