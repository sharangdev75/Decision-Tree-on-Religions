library(rpart)
library(caret)
library(dplyr)
library(caTools)
library(rworldmap)
library(ggplot2)
flags_df <-
  read.csv(
    "C:\\Users\\Sharang\\Documents\\Assignment Sem 2\\Random Forrest\\flag.data",
    header = FALSE
  )
flags_df

colnames(flags_df) <-
  c(
    'Names',
    'landmass',
    'zone',
    'area',
    'population',
    'language',
    'religion',
    'bars',
    'stripes',
    'colours',
    'red',
    'green',
    'blue',
    'gold',
    'white',
    'black',
    'orange',
    'mainhue',
    'circles',
    'crosses',
    'saltires',
    'quarters',
    'sunstars',
    'crescent',
    'triangle',
    'icon',
    'animate',
    'text',
    'topleft',
    'botright'
  )
colnames(flags_df)
view(flags_df)
#Deleting "Name" column from Data Set as it is unncessary
flags_df <- flags_df[-1]
flags_df$religion <- factor(flags_df$religion)
#-------------World Map Exploratary
#get coarse resolution world from rworldmap
sPDF <- getMap()
#mapCountries using the 'continent' attribute
mapCountryData(sPDF, nameColumnToPlot = 'continent')


# ----------Exploratory data Analysis Using GGPLOT-----------

ggplot(flags_df, aes(religion, area)) + geom_point(aes(color = religion), size =
                                                     4, alpha = 0.5)
#Histogram Between Zone and Religion
ggplot(flags_df, aes(zone)) + geom_histogram(aes(fill = religion),
                                             color = 'Black',
                                             bins = 50,
                                             alpha = 0.5) + theme_bw()
#Histogram Between Landmass and Religion
ggplot(flags_df, aes(landmass)) + geom_histogram(aes(fill = religion),
                                                 color = 'Black',
                                                 bins = 50,
                                                 alpha = 0.5) + theme_bw()



#------------------Spliting up of the data using Catools-------

set.seed(101)
sample <- sample.split(flags_df, SplitRatio = .70)
train <- subset(flags_df, sample == T)
test <- subset(flags_df, sample == F)

############Decision Tree Model#############

#----------Decision Tree -- Install rpart package
#Using Library R-parts for the Decision Tree
tree <- rpart(religion ~ ., data = train, method = "class")
tree.pred <-
  predict(tree, test) #Prediction test and output variable
head(tree.pred)
#plotting the Decision Tree
par(xpd = NA)
plot(tree)
text(tree, 3)
print(tree, 2)
mean(tree.pred == test$religion)
#Mean between test set and output variable is 15.4%

#------------------ Pruning with caret
#Using library Caret for Pruning
#Perious model gave the accuracy of 15.4% so to imporve the accuracy of the model, Pruning is done on model.
set.seed(123)
model2 <- train(
  religion ~ .,
  data = train,
  method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

plot(model2)
model2$bestTune
#plotting the Decision Tree
par(xpd = NA)
plot(model2$finalModel)
text(model2$finalModel, digits = 3)
predi_class <- model2 %>% predict(test)
mean(predi_class == test$religion)
#Accuracy of the model has been improved to 61.7% after the pruning..
