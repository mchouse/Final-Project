# set the working directory to the unzipped datafile

setwd("C:/Users/mchupwin/Desktop/Data Science/Getting and cleaning data/getdata_projectfiles_UCI HAR Dataset/projectdata")

# read in 6 files(test and train for subject, x (feature), y (activitie))

train_subject <- read.table("./train/subject_train.txt")
train_feature <-read.table("./train/x_train.txt")
train_activity <-read.table("./train/y_train.txt")

test_subject <- read.table("./test/subject_test.txt")
test_feature <-read.table("./test/x_test.txt")
test_activity <-read.table("./test/y_test.txt")

# merge test tables into a single dataset

test_combined <- cbind(test_subject,test_activity,test_feature)

# merge train tables into a single dataset

train_combined <- cbind(train_subject,train_activity,train_feature)

# stack test and train tables

fulldata <- rbind(train_combined,test_combined)

# read in variable names file

varnames <- read.table("./features.txt")

#extract the variable names column and turn it into a clean vector of names

varlist <-varnames[,2]
varlist <- gsub('-','_',varlist)
varlist <- gsub('(','',varlist, fixed=TRUE)
varlist <- gsub(')','',varlist, fixed=TRUE)
varlist <- gsub(',','',varlist, fixed=TRUE)
varlist <- tolower(varlist)


#add names for first two columns

first_column_names <- c("subject","activity")

#add varnames to full dataset

full_var_names <- c(first_column_names,varlist)

names(fulldata) <- full_var_names

#label activities

fulldata$activity <- factor(fulldata$activity, levels=c(1,2,3,4,5,6),
                            labels = c('Walking','Walking_upstairs','Walking_downstairs','Sitting',
                                       'Standing','Laying'))

#select means and STD columns only

library(dplyr)


fulldata_mean<- fulldata[,grepl("mean", colnames(fulldata))]
fulldata_std<- fulldata[,grepl("std", colnames(fulldata))]

fulldata_act_sub <- fulldata[,c(1,2)]

fulldata_mean_std <- cbind(fulldata_act_sub,fulldata_mean,fulldata_std)

#create a new dataset with the summary stats of the old by activity and subject


summary_data <- fulldata_mean_std %>% group_by(subject, activity) %>% summarise_each(funs(mean), vars=
                                                                                      tbodyacc_mean_x,
                                                                                    tgravityacc_mean_x,
                                                                                    tbodyaccjerk_mean_x,
                                                                                    tbodyaccjerk_mean_y,
                                                                                    tbodygyro_mean_y,
                                                                                    tbodygyrojerk_mean_y,
                                                                                    tgravityaccmag_mean,
                                                                                    tbodygyrojerkmag_mean,
                                                                                    fbodyacc_mean_z,
                                                                                    fbodyacc_meanfreq_z,
                                                                                    fbodyaccjerk_mean_z,
                                                                                    fbodyaccjerk_meanfreq_z,
                                                                                    fbodygyro_mean_z,
                                                                                    fbodygyro_meanfreq_z,
                                                                                    fbodybodyaccjerkmag_mean,
                                                                                    fbodybodygyromag_meanfreq,
                                                                                    angletbodyaccmeangravity,
                                                                                    angletbodygyrojerkmeangravitymean,
                                                                                    anglezgravitymean,
                                                                                    tbodyacc_std_z,
                                                                                    tgravityacc_std_z,
                                                                                    tbodyaccjerk_std_z,
                                                                                    tbodygyro_std_z,
                                                                                    tbodygyrojerk_std_z,
                                                                                    tbodyaccjerkmag_std,
                                                                                    fbodyacc_std_x,
                                                                                    fbodyaccjerk_std_x,
                                                                                    fbodygyro_std_x,
                                                                                    fbodyaccmag_std,
                                                                                    fbodybodygyrojerkmag_std,
                                                                                    tbodyacc_mean_y,
                                                                                    tgravityacc_mean_y,
                                                                                    tbodyaccjerk_mean_z,
                                                                                    tbodygyro_mean_z,
                                                                                    tbodygyrojerk_mean_z,
                                                                                    tbodyaccjerkmag_mean,
                                                                                    fbodyacc_mean_x,
                                                                                    fbodyacc_meanfreq_x,
                                                                                    fbodyaccjerk_mean_x,
                                                                                    fbodyaccjerk_meanfreq_x,
                                                                                    fbodygyro_mean_x,
                                                                                    fbodygyro_meanfreq_x,
                                                                                    fbodyaccmag_mean,
                                                                                    fbodybodyaccjerkmag_meanfreq,
                                                                                    fbodybodygyrojerkmag_mean,
                                                                                    angletbodyaccjerkmeangravitymean,
                                                                                    anglexgravitymean,
                                                                                    tbodyacc_std_x,
                                                                                    tgravityacc_std_x,
                                                                                    tbodyaccjerk_std_x,
                                                                                    tbodygyro_std_x,
                                                                                    tbodygyrojerk_std_x,
                                                                                    tbodyaccmag_std,
                                                                                    tbodygyromag_std,
                                                                                    fbodyacc_std_y,
                                                                                    fbodyaccjerk_std_y,
                                                                                    fbodygyro_std_y,
                                                                                    fbodybodyaccjerkmag_std,
                                                                                    tbodyacc_mean_z,
                                                                                    tgravityacc_mean_z,
                                                                                    tbodygyro_mean_x,
                                                                                    tbodygyrojerk_mean_x,
                                                                                    tbodyaccmag_mean,
                                                                                    tbodygyromag_mean,
                                                                                    fbodyacc_mean_y,
                                                                                    fbodyacc_meanfreq_y,
                                                                                    fbodyaccjerk_mean_y,
                                                                                    fbodyaccjerk_meanfreq_y,
                                                                                    fbodygyro_mean_y,
                                                                                    fbodygyro_meanfreq_y,
                                                                                    fbodyaccmag_meanfreq,
                                                                                    fbodybodygyromag_mean,
                                                                                    fbodybodygyrojerkmag_meanfreq,
                                                                                    angletbodygyromeangravitymean,
                                                                                    angleygravitymean,
                                                                                    tbodyacc_std_y,
                                                                                    tgravityacc_std_y,
                                                                                    tbodyaccjerk_std_y,
                                                                                    tbodygyro_std_y,
                                                                                    tbodygyrojerk_std_y,
                                                                                    tgravityaccmag_std,
                                                                                    tbodygyrojerkmag_std,
                                                                                    fbodyacc_std_z,
                                                                                    fbodyaccjerk_std_z,
                                                                                    fbodygyro_std_z,
                                                                                    fbodybodygyromag_std)                     
                
write.table(summary_data, "./final_project_summary_table.txt", sep=",", row.name=FALSE)                      
                     
                     