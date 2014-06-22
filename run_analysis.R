## Christian Peikert

## Methode to load combine 2 files rowwise
combineFiles<- function(file1_path,file2_path) {
  data_file1  <- read.table(file1_path)           # load file
  data_file2  <- read.table(file2_path)           # load file
  print(paste0("data_file1: rows = ",nrow(data_file1),", columns = ",ncol(data_file1)))
  print(paste0("data_file2: rows = ",nrow(data_file2),", columns = ",ncol(data_file2)))
  combined <- rbind(data_file1,data_file2)        # combined file
  print(paste0("combined: rows = ",nrow(combined),", columns = ",ncol(combined)))
  return(combined)
}

## Merges the training and the test sets to create one data set
data_x <- combineFiles("test/X_test.txt","train/X_train.txt")
activities <- combineFiles("test/Y_test.txt","train/Y_train.txt")
subjects <- combineFiles("test/subject_test.txt","train/subject_train.txt")


## Extracts only the measurements on the mean and standard deviation for each measurement. 

header  <- readLines("features.txt")
header <- gsub("\\d{,3} ", "", header)
colnames(data_x) <- header

header_to_keep <- grep("-mean\\(\\)|-std\\(\\)", colnames(data_x)) ## here the unwanted header were filterd out

data <- data_x[,header_to_keep] ## data contain now the filterd matrix
#print(colnames(data))

## Appropriately labels the data set with descriptive variable names. 
colnames(data) <- tolower(colnames(data))
colnames(data) <- gsub("\\(|\\)|_", "", colnames(data))

#print(colnames(data))

## Uses descriptive activity names to name the activities in the data set.
colnames(activities) <- "activity"
data_a  <- read.table("activity_labels.txt", header = FALSE)
activities[,"activity"] <- data_a[activities[,"activity"],2]
print(activities[,"activity"])

## combine data
colnames(subjects) <- "subject"
all_data = cbind(subjects,activities,data)
colnames(all_data)
write.table(all_data, "merged_column_condensed_data.txt")

## Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject
uniqueSubjects = sort(unique(subjects)[,1])
uniqueActivities = sort(data_a[,2])
numSubjects = length(uniqueSubjects)
numActivities = length(uniqueActivities)

filter_columns <- c("subject","activity")
value_columns <- setdiff(colnames(all_data),filter_columns)
melt_matrix = data[,value_columns]
melt_header = paste(colnames(melt_matrix),sep=";",collapse = '')
fileConn<-file("summary.txt","w")
outputheader = gsub(";","\t",paste("subject","actvity","#rows","mean",melt_header,sep="\t",collapse = ''))
write(outputheader, fileConn, ,append=FALSE)
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
  keep_rows = all_data[,"subject"]== s
  subdata = all_data[keep_rows,]
  keep_rows = subdata[,"activity"] == as.character(uniqueActivities[a])
  subdata = subdata[keep_rows,] 
  melt_matrix = subdata[,value_columns]
  cmeans = colMeans(melt_matrix)
  cmeans = paste(cmeans,sep="\t",collapse = ';')

  outputline = gsub(";","\t",paste(as.character(uniqueSubjects[s]),as.character(uniqueActivities[a]),nrow(subdata),cmeans,sep="\t",collapse = ''))
  write(outputline, fileConn, ,append=TRUE)
  }
  
}
close(fileConn)




