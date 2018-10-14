library(grid)
library(jpeg)
library(ggplot2)
library(e1071)
library(RCurl)
library(dplyr)
library(corrplot)
library(caret)
library(caTools)

df<- read.csv('D:\\MY-DOC\\Documents\\NBA Data\\NBA shot log 16-17-regular season\\NBA shot log 16-17-regular season\\Shot data\\shot log LAL.csv')
stats<- read.csv('D:\\MY-DOC\\Documents\\NBA Data\\NBA shot log 16-17-regular season\\NBA shot log 16-17-regular season\\Player Regular 16-17 Stats.csv') 

# Half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
           width=unit(1,"npc"), height=unit(1,"npc"))

# include the other half
df$x <- ifelse(df$location.x >470,940-df$location.x,df$location.x)


# Shot chart of Lakers 2016-2017 season 
ggplot(df, aes(x=location.y, y=x)) + 
      annotation_custom(court, 0, 500, 0, 470) +
	labs(title = "Shot Chart Lakers 2016-2017\n",color = "SHOT\n") +
	scale_color_manual(labels = c("Missed", "Scored"), values = c("tomato", "green")) +
      geom_point(aes(colour = ifelse(df$current.shot.outcome == 'SCORED','tomato','green'))) +
      xlim(0, 500) +
      ylim(0, 470)


# Shot Distance in feet
df$shot_dist <- (df$x-53)/10
summary(df$shot_dist)


# Add range and make/miss columns to the datset. make/miss will be dependent
df$range<- ifelse(df$shot_dist<10,1,ifelse(df$shot_dist>10 & df$shot_dist<20,2,3))
df$make_miss<- ifelse(df$current.shot.outcome == 'SCORED',1,0)

# Season Stats for Lakers Players
LA_player_stats <- subset(stats, X.Team.Name == 'Lakers')
LA_player_stats<-LA_player_stats[,-c(1)] 
View(LA_player_stats)

# Best 3 point shooters: Nick Young at 38.7% and Lou Williams at 35.9%
df %>%
  filter(range == 3) %>%
  group_by(shoot.player) %>%
  summarise(count = n(), makes = sum(make_miss), FG_pct = (makes/count)*100) %>%
  filter(makes > 50) %>%
  arrange(desc(FG_pct))

# Lou Williams is 29.2% on catch and shoot threes.
# However, he is 50% and above on fadeaways, pullups, and step backs. Sweet Lou.
df %>%
  filter(shoot.player == 'Lou Williams' & range == 3) %>%
  group_by(shot.type) %>%
  summarise(count = n(), makes = sum(make_miss), FG_pct = (makes/count)*100) %>%
  arrange(desc(FG_pct))

# ----------------------------------------------------------------------------------------
# Brandon Ingram's rookie season

# GOOD: Pullup Jumpshot, Driving Layup, Running Layup, Cutting Layup
# BAD: Catch and Shoot Jumpshot, Turnaround Fadeaway, Contested Layup
df %>%
  filter(shoot.player == 'Brandon Ingram') %>%
  group_by(shot.type) %>%
  summarise(count = n(),makes = sum(make_miss),FG_percent = (makes/count)*100) %>%
  arrange(desc(count))

BI<- df %>%
  filter(shoot.player == 'Brandon Ingram')

# Brandon Ingram Shot Chart
ggplot(BI, aes(x=location.y, y=x)) + 
      annotation_custom(court, 0, 500, 0, 470) +
	labs(title = "Shot Chart Brandon Ingram 2016-2017\n",color = "SHOT\n") +
	scale_color_manual(labels = c("Missed", "Scored"), values = c("red", "green")) +
      geom_point(aes(colour = ifelse(BI$current.shot.outcome == 'SCORED','red','green'))) +
      xlim(0, 500) +
      ylim(0, 470)


# Create a model that predicts whether or not Brandon Ingram will make a shot

# Replace missing values
BI$shot_dist <- ifelse(is.na(BI$shot_dist), mean(BI$shot_dist, na.rm=TRUE), BI$shot_dist)
BI$location.y <- ifelse(is.na(BI$location.y), mean(BI$location.y, na.rm=TRUE), BI$location.y)

# Create new columns for the classifier
# x,location.y,shot.type,shot_dist,current.shot.outcome
BI$range<- ifelse(BI$shot_dist<10,1,ifelse(BI$shot_dist>10 & BI$shot_dist<20,2,3))
BI$shot.type<-as.numeric(BI$shot.type)
BI$make_miss<- ifelse(BI$current.shot.outcome == 'SCORED',1,0)

# Remove unwanted columns
BI<-BI[,-c(1,2,3,4,5,6,8,9,11,12,13,14,15,16)] 
BI<-BI[,c(1,7,2,3,4,5,6)]


# Variable correlation suggests that shot selection is the biggest factor in
# whether the shot goes in or not
BI %>%
    cor(use='complete',method='pearson') %>%
  corrplot(type='lower', diag = F)

# X and range have the same effect as shot_dist, so drop the columns
BI<-BI[,-c(3,5)] 

#Split into training and test set
set.seed(123)
split = sample.split(BI$make_miss, SplitRatio = 550/687)
training_set = subset(BI, split == TRUE)
test_set = subset(BI, split == FALSE)


classifier = svm(formula = make_miss ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-4])

View(y_pred)
# Making the Confusion Matrix
cm = table(test_set[, 4], y_pred)
cm
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy

# Accuracy of the predictive model is 0.69

# Try with cross validation
folds = createFolds(training_set$make_miss, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]

  classifier = svm(formula = make_miss ~.,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')

  y_pred = predict(classifier, newdata = test_fold[-4])

  cm = table(test_fold[, 4], y_pred)
	# accuracy = total corect predictions/total observations

  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

cv
accuracy = mean(as.numeric(cv))
accuracy
#The accuracy is still 0.69





































