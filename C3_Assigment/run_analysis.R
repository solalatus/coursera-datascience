#install.packages("cwhmisc")
# library(cwhmisc)
library(stringi)
library(data.table)

feature_filename <- "~/course-work/C3_Assigment/UCI HAR Dataset/features.txt"
test_data_filename <-"~/course-work/C3_Assigment/UCI HAR Dataset/test/X_test.txt"
training_data_filename <-"~/course-work/C3_Assigment/UCI HAR Dataset/train/X_train.txt"
test_labeling_filename <- "~/course-work/C3_Assigment/UCI HAR Dataset/test/y_test.txt"
training_labeling_filename <- "~/course-work/C3_Assigment/UCI HAR Dataset/train/y_train.txt"
labels_filename <- "~/course-work/C3_Assigment/UCI HAR Dataset/activity_labels.txt"
subject_ids_test <- "~/course-work/C3_Assigment/UCI HAR Dataset/test/subject_test.txt"
subject_ids_train <- "~/course-work/C3_Assigment/UCI HAR Dataset/train/subject_train.txt"


#reading activity labels, preprocessing as factors
label <- read.table(labels_filename, sep="")
test_labeling <- read.table(test_labeling_filename,sep="")
train_labeling <- read.table(training_labeling_filename,sep="")
labelings <- factor(c(test_labeling[,1], train_labeling[,1]),labels=label[,2])

# reading feature names
feature_names <- read.table(feature_filename, sep="")

# select good features
processed_features_mask <- vector(mode="character", length=0)
processed_features_names <- vector(mode="character", length=0)

for (feat in 1:nrow(feature_names)) {
  good <- TRUE
  good <- stri_detect_fixed(toString(feature_names[feat,2]),"mean(")
  #good <- grepl("mean",toString(feature_names[feat,2]), fixed=T)
  #good <- cpos(toString(feature_names[feat,2]),"mean()")

  if (good==F) {
    good <- stri_detect_fixed(toString(feature_names[feat,2]),"std(")
    #good <- grepl("std",toString(feature_names[feat,2]), fixed=T)
    #good <- cpos(toString(feature_names[feat,2]),"std()")
  }
  
  if (good==T) {
    processed_features_mask <- c(processed_features_mask, "numeric")
    #print(toString(feature_names[feat,2]))
    #print(feat)
    processed_features_names <- c(processed_features_names, toString(feature_names[feat,2]))
  }
  else {
    processed_features_mask <- c(processed_features_mask, "NULL")
  }
}


# read only those data that are relvant based on the mask

# read test data
data <- read.table(test_data_filename, sep="", header=F,  colClasses=processed_features_mask)

# read training data and merge
data <- rbind(data,read.table(training_data_filename, sep="", header=F,  colClasses=processed_features_mask))

# make names tidy
names(data)<- processed_features_names

# adding labelings to dataframe
data$Labeling <-labelings

# read subject ID and merge columns
subject_ids <- read.table(subject_ids_test, sep="")
subject_ids <- rbind(subject_ids,read.table(subject_ids_train, sep=""))
data$Subject_id <- subject_ids[,1]

# ------
# calculate needed averages of measurements per subject, per activity

# means_data <- by(data[,1:66],list(data$Labeling,data$Subject_id),colMeans)
# this would be ideal, but output is problematic :-(
# have to iterate through again to format results into table :-(((
# deadline is in an hour :-((

# print(means_data)
#str(means_data)
#tab <-data.table(data)
#a<-tab[,Mean:=mean(data$),by=list(data$Labeling,data$Subject_id)]
#write.table(a,"~a.txt")

# very ugly, but works. :-(
result<-data.frame()
for (sub in unique(data$Subject_id)) {
  #print(sub)
  for (lab in unique(data$Labeling)) {
    a<-colMeans(data[(data$Subject_id==sub) & (data$Labeling==lab),][,1:66])
    result<-rbind(result,cbind(a,lab,sub))    
  }
    
}
names(result)<-c("Feature / Mean","Label","Subject_id")
#print(head(result))
write.table(result,"~/result.txt")
print("Done")
