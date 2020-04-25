# PLS-PM analysis for Impact Predition on Tweets
# Author: Manuel de la Torre

# Load libraries
library("plspm")
library("plsdepot")
library("corrplot")
library("e1071")  

# Load data
setwd("/Users/manuelmhtr/Projects/TweetsImpactPrediction/PLSPM");
list.files("./data")
tweetsRaw = read.csv("./data/tweetsSummary_v4.0newSet.csv")
head(tweetsRaw)
dim(tweetsRaw)

# Build normalization factor formula.
getNormFactor = function(collection) {
  med   = mean(collection);
  eqs   = skewness(collection) / 100;
  dest  = 0.5 - 0.5 * (exp(eqs) - exp(-eqs)) / (exp(eqs) + exp(-eqs));
  return ((med - dest * med) / dest);
}

# Filter useless data
# tweets = subset(tweetsRaw, select=-c(id, userIdStr, twitterIdStr, messageIsDirect, postHourOfDay, postDayOfWeek))
# Normalizagin data (Inverting data where higher is worst)
tweets = subset(tweetsRaw, select=c(id))
tweetsRaw$messageReach         = (tweetsRaw$messageIsDirect) * tweetsRaw$messageMentionsCount + (1 - tweetsRaw$messageIsDirect) * (tweetsRaw$userFollowersCount) + 1
tweets$messageReachRatio       = 1 - 1 / ((tweetsRaw$messageReach   / getNormFactor(tweetsRaw$messageReach))   + 1) 
tweets$clicksRatio             = 1 - 1 / ((tweetsRaw$clicksCount    / getNormFactor(tweetsRaw$clicksCount))    + 1) 
tweets$retweetsRatio           = 1 - 1 / ((tweetsRaw$retweetsCount  / getNormFactor(tweetsRaw$retweetsCount))  + 1) 
tweets$favoritesRatio          = 1 - 1 / ((tweetsRaw$favoritesCount / getNormFactor(tweetsRaw$favoritesCount)) + 1)
tweets$userKloutLevel          = tweetsRaw$userKloutScore / 100
tweets$userMozLevel            = tweetsRaw$userMozScore   / 100
tweets$messageHasMedia         = tweetsRaw$messageHasMedia
tweets$userFollowersRatio      = 1 - 1 / ((tweetsRaw$userFollowersCount / getNormFactor(tweetsRaw$userFollowersCount)) + 1)
tweets$userListedRatio         = 1 - 1 / ((tweetsRaw$userListedCount    / getNormFactor(tweetsRaw$userListedCount))    + 1)
tweets$userVerified            = tweetsRaw$userVerified
tweets$id = NULL

print(getNormFactor(tweetsRaw$messageReach));
print(getNormFactor(tweetsRaw$clicksCount));
print(getNormFactor(tweetsRaw$retweetsCount));
print(getNormFactor(tweetsRaw$favoritesCount));
print(getNormFactor(tweetsRaw$userFollowersCount));
print(getNormFactor(tweetsRaw$userListedCount));

summary(tweets)
# Display data
head(tweets, n=6)
dim(tweets)
names(tweets)

printSummary(tweets$favoritesRatio);
printSummary = function(collection) {
  print(round(min(collection),4));
  print(round(max(collection),4));
  print(round(mean(collection),4));
  print(round(median(collection),4));
  print(round(sd(collection),4));
  print(round(skewness(collection),4));
  print(round(kurtosis(collection),4));
}

# Basic plots
# Number of clicks
boxplot(tweets$clicksRatio)
hist(tweets$clicksRatio)
hist(tweets$retweetsRatio)
hist(tweets$favoritesRatio)
# Followers count
boxplot(tweets$userFollowersRatio)
hist(tweets$userFollowersRatio)

# Correlation
summary(tweets);
M <- cor(tweets)
corrplot.mixed(M, tl.pos="lt");
# ((.44+.48+.42+.67+.72+.2+.46) + (.42+.45+.39+.75+.76+.12+.42) + (.45+.48+.42+.66+.71+.2+.45)) / (3*7)

# CPA analysis
help(prcomp)
tweetsPca = prcomp(tweets, center = TRUE, scale. = TRUE) 
tweetsPca$rotation
tweetsRotationAbs = abs(tweetsPca$rotation)
tweetsRotationAbs
plot(tweetsPca, type = "l")
tweetsPca
summary(tweetsPca)
names(tweets)



# GET PRINCIPAL COMPONENTS:

# 1: messageReachRatio, userKloutScore, userMozScore, userFollowersCount, userListedCount,
tweetsRotationAbs[order(-tweetsRotationAbs[,"PC1"]),1]
audience_cols = c(1,5,6,8,9);
plot(nipals(tweets[,audience_cols]), main = "Audience indicators (circle of correlations)", cex.main = 1)

# 2: favoritesRatio, retweetsRatio, clicksRatio
tweetsRotationAbs[order(-tweetsRotationAbs[,"PC2"]),2]
impact_cols = c(2,3,4);
#impact_cols = c(13);
plot(nipals(tweets[,impact_cols]), main = "Impact indicators (circle of correlations)", cex.main = 1)

# 3: messageHasMedia, userVerified
tweetsRotationAbs[order(-tweetsRotationAbs[,"PC3"]),3]
messageContent_cols = c(7,10);
plot(nipals(tweets[,messageContent_cols]), main = "Message content indicators (circle of correlations)", cex.main = 1)

# Building inner model
MessageContent = c(0, 0, 0);
Audience       = c(0, 0, 0);
Impact         = c(1, 1, 0);

# Matrix created by row binding
tweetsInner = rbind(MessageContent, Audience, Impact)
colnames(tweetsInner) = rownames(tweetsInner)

# plot the inner matrix
innerplot(tweetsInner)

# define list of indicators
tweetsOuter = list(messageContent_cols, audience_cols, impact_cols)

# Tell variables are reflexive or formative
tweetsModes = rep("A", 3)

tweetsPls = plspm(tweets, tweetsInner, tweetsOuter, tweetsModes, maxiter=100)

# Goodness of fit (should be higher than 0.70)
tweetsPls$gof

# summarized results
summary(tweetsPls)

# Show results
tweetsPls
tweetsPls$path_coefs
tweetsPls$inner_model
tweetsPls$inner_summary
plot(tweetsPls)
tweetsPls$crossloadings
plot(tweetsPls, what="loadings")
plot(tweetsPls, what="weights")


# Unidimensionallity
tweetsPls$unidim

#  Alphas must be higher than 0.7 to be acceotable (rule of thumb)
tweetsPls$unidim[, 3, drop = FALSE]

# Loadings and communalities
# communality must be higher than 0.7 (comes form 0.7^2 = 50% of variance)
tweetsPls$outer_model

# Cross loadings
# Does parameter is useful to describe blocks?
tweetsPls$crossloadings

# Explanation of the blocks
tweetsPls$inner_model
tweetsPls$inner_summary

# Validation
# Bootstraping: Add some noise to the original data to make sure that the model correctly
# describes data.
bootPls = plspm(tweets, tweetsInner, tweetsOuter, tweetsModes, boot.val = TRUE, br = 100)

plot(bootPls)
bootPls$crossloadings
plot(bootPls, what="loadings")
plot(bootPls, what="weights")

# Bootstraping results
bootPls$boot



# Plot Neural Network Error
library("RMySQL");
db <- dbConnect(RMySQL::MySQL(), user='root', password='', dbname='viral_development', host='127.0.0.1')
#dbSendQuery(db, "UPDATE stats_raw set predictedClicksRatio = NULL, predictedRetweetsRatio = NULL, predictedFavoritesRatio = NULL, predictedImpactRatio = NULL WHERE 1");

# Control test
#result <- dbSendQuery(db, "SELECT actualRetweetsRatio, actualFavoritesRatio, actualClicksRatio, actualImpactRatio FROM stats_raw WHERE 1 LIMIT 100000");
#controlData = dbFetch(result, n=-1)
#controlData$avgImpact  = controlData$actualRetweetsRatio + controlData$actualFavoritesRatio + controlData$actualClicksRatio
#meanAvgImpact          = mean(controlData$avgImpact)
#controlData$stdError   = (controlData$avgImpact - meanAvgImpact)
#errors  = controlData$stdError;
#results = controlData$avgImpact;

# Regular Neural network test
result <- dbSendQuery(db, "SELECT id, actualClicksRatio, actualRetweetsRatio, actualFavoritesRatio, predictionError FROM stats_raw WHERE predictionError != -1 LIMIT 100000");
dbData  = dbFetch(result, n=-1);

mean(dbData$actualClicksRatio)
mean(dbData$actualRetweetsRatio)
mean(dbData$actualFavoritesRatio)

# Control errors
controlPrediction = data.frame(mean(dbData$actualClicksRatio), mean(dbData$actualRetweetsRatio), mean(dbData$actualFavoritesRatio));
names(controlPrediction) = c("clicks", "retweets", "favorites")
meanError = c();
for (i in c(1:dim(dbData)[1])) {
  currentError = abs(dbData[i,]$actualClicksRatio    - controlPrediction$clicks);
  currentError = currentError + abs(dbData[i,]$actualRetweetsRatio  - controlPrediction$retweets);
  currentError = currentError + abs(dbData[i,]$actualFavoritesRatio - controlPrediction$favorites);
  meanError    = c(meanError, currentError);
}
errors = meanError;

# Prediction errors
errors = dbData$predictionError;

# Show summary
summary(errors);
summary(results);

# Init vars
data = c();

data$mad = mean(abs(errors));
data$mse = mean(errors^2);
data$Emin = sum(errors^2) / 2;
data$sse = sum(errors^2);
data$maxError = max(errors^2);

print(data);

data$totalError = sum(errors);
data$meanError  = mean(errors);
data$maxError   = max(errors);
data$minError   = min(errors);
data$success    = length(errors[errors < 0.25]);
data$successPer = data$success / length(errors);

print(data);

# Build linear model
errorsLm = lm(errors ~ c(1:length(errors)))
data$coefA = errorsLm$coefficients[[1]];
data$coefB = errorsLm$coefficients[[2]];

# Plots
hist(errors, breaks=100)
?boxplot
boxplot(data.frame(e=errors, s=stdErrors))
boxplot(errors)
summary(errors)
length(errors)

# Plot data and linear model
par(mfrow=c(1,2), mar=c(5,5,1,1));
?par
controlError       = meanError^2;
neuralNetworkError = dbData$predictionError^2;
plot(controlError, type="o", col="#00000033", ylim=c(0, 7));
plot(neuralNetworkError, type="o", col="#00000033", ylim=c(0, 7));
#plot(errors, type="o", col="#00000033", ylim=c(0, 7));
#abline(errorsLm, col="red");

# Finally, print data
print(data);

