# Natural Language Processing

# Importing the dataset
dataset_original = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE) #for tsv file (separator = /t ('tab'))

#Cleaning the texts
#install.packages('tm')
install.packages('SnowballC')
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(dataset$Review)) #data needing cleaning
corpus = tm_map(corpus, content_transformer(tolower)) #transform all CAPS to lower case
corpus = tm_map(corpus, removeNumbers) #remove all numbers
corpus = tm_map(corpus, removePunctuation) #remove punctuation
corpus = tm_map(corpus, removeWords, stopwords()) #remove irrelevant words (this, the, a...)
corpus = tm_map(corpus, stemDocument) #only take root of the word (loved --> love)
corpus = tm_map(corpus, stripWhitespace) #remove extra spaces

# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus) #Sparse Matrix of features
dtm = removeSparseTerms(dtm, 0.999) #filter 99.9% most frequent words
dataset = as.data.frame(as.matrix(dtm)) # transform sparse matrix into dataframe     Dependent Variable
dataset$Liked = dataset_original$Liked #new column addition

#Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

#Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#Fitting random forest classifier to the training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

#Predicting the test set results
y_pred = predict(classifier, newdata = test_set[-692])

#Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)
