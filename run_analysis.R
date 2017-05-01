wd<-getwd()

library(data.table)

# 1)Merges the training and the test sets to create one data set.

features<-read.table('features.txt')
features

activity_labels<-read.table('activity_labels.txt')
activity_labels

#read test data
test<-read.table(paste(wd,'/test/X_test.txt',sep=''))
class(test)
head(test)
sapply(test,class)


names_test<-unlist(read.table(paste(wd,'/test/y_test.txt',sep='')))
class(names_test)
head(names_test)
table(names_test)

subject_test<-unlist(read.table(paste(wd,'/test/subject_test.txt',sep='')))
class(subject_test)
head(subject_test)
table(subject_test)

test$label<-names_test
test$subject<-subject_test



#read train data
train<-read.table(paste(wd,'/train/X_train.txt',sep=''))
head(train)
sapply(train, class)

names_train<-unlist(read.table(paste(wd,'/train/y_train.txt',sep='')))
head(names_train)
table(names_train)

subject_train<-unlist(read.table(paste(wd,'/train/subject_train.txt',sep='')))
head(subject_train)
table(subject_train)

train$label<-names_train
train$subject<-subject_train


data_all<-rbind(train, test)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.

mean<-features[grep('mean\\()', tolower(features$V2)),]
std<-features[grep('std\\()', tolower(features$V2)),]

mean_std<-rbind(mean,std)
mean_std
mean_std$V3<-paste("V",mean_std$V1,sep='')
head(mean_std)


selected_vars<-c(unique(mean_std$V3),c("label", "subject"))
selected_vars


data_all_2<-data_all[selected_vars]


# 3 Uses descriptive activity names to name the activities in the data set
head(data_all_2)
names(activity_labels)<-c("label", "activity_desc")
activity_labels

data_all_3<-merge(data_all_2, activity_labels, by.x="label", by.y="label", all.x=TRUE)
head(data_all_3)


# 4 Appropriately labels the data set with descriptive variable names.
head(features)

features$desc<-gsub('\\()',"",gsub("-","_",substring(features$V2,1,length(features$V2)-4)))
head(features)

selected_features<-subset(features, V1 %in% mean_std$V1)
selected_features$V3<-paste("V",selected_features$V1,sep='')
head(selected_features)

for(i in 1:nrow(selected_features)){
        colnames(data_all_3)[colnames(data_all_3)==selected_features[i,4]]<-selected_features[i,3]
}

head(data_all_3)

tidy_data<-data_all_3
str(tidy_data)

# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

tidy_data_means<-aggregate(tidy_data[, 2:67], list(tidy_data$subject, tidy_data$activity_desc), mean)

colnames(tidy_data_means)[1]<-"subject"
colnames(tidy_data_means)[2]<-"activity_desc"

str(tidy_data_means)

# saving the tidy dataset

write.table(tidy_data_means, 'tidy_data_means.txt', row.name=FALSE)

# GitHub contains a code book that modifies and updates 
# the available codebooks with the data to indicate all 
# the variables and summaries calculated, along with units, 
# and any other relevant information.

cols<-colnames(tidy_data_means)
cols

categories<-unique(substring(cols, 1, (regexpr('_',cols)-1)))
