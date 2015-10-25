# require dplyr library
library(dplyr)

# change to your working directory
setwd('/home/sata/tmp/3/r test/coursera/getdata/project/')

# fucntion for gathering data
# join features, activities and subjects
getData = function (data, labels, subjects, activities, features) {
    features = read.table(features)
    labels = read.table(labels, col.names = c('activity'))
    activities = read.table(activities, col.names = c('id', 'name'))
    activities = labels %>%
        left_join(activities, c('activity' = 'id')) %>%
        rename(activity_name = name)
    subjects = read.table(subjects, col.names = c('subject'))
    data = read.table(data, col.names = features[,2])
    
    cbind(subjects, activities, data)
}

# ensure that data directory is exists
if (!dir.exists('data')) {
    dir.create('data')
}

# download & unzip data if necessary
src = 'data/Dataset.zip'
if (!file.exists(src)) {
    download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', src)
    unzip(src, exdir = 'data')
}

dataDirPath = 'data/UCI HAR Dataset'
testDataDirPath = file.path(dataDirPath, 'test')
trainDataDirPath = file.path(dataDirPath, 'train')

# gather test data
test = getData(
    file.path(testDataDirPath, 'X_test.txt'),
    file.path(testDataDirPath, 'y_test.txt'),
    file.path(testDataDirPath, 'subject_test.txt'),
    
    file.path(dataDirPath, 'activity_labels.txt'),
    file.path(dataDirPath, 'features.txt')
    )
# gather train data
train = getData(
    file.path(trainDataDirPath, 'X_train.txt'),
    file.path(trainDataDirPath, 'y_train.txt'),
    file.path(trainDataDirPath, 'subject_train.txt'),
    
    file.path(dataDirPath, 'activity_labels.txt'),
    file.path(dataDirPath, 'features.txt')
)

# join data to one set
all = full_join(train, test)
# filter measurements columns (mean & std)
partial = select(all, subject:activity_name, contains('mean'), contains('std'))
# rename columns
names(partial) = lapply(names(partial), function(col) {
    gsub('\\.', '', gsub('\\.([A-z])', '\\U\\1', col, perl = TRUE))
})
# tidy data
tidy = partial %>%
    group_by(subject, activity, activity_name) %>%
    summarise_each(funs(mean), -(subject:activity_name))

# save tidy data
write.table(tidy, 'tidy.txt', row.names = F)
