# PLS-PM analysis for Impact Predition on Tweets
# Author: Manuel de la Torre

# Load libraries
library("plspm")
library("plsdepot")
library("corrplot")

# Load data
setwd("/Users/manuelmhtr/Projects/TweetsImpactPrediction/PLSPM");
list.files("./data")
tweetsRaw = read.csv("./data/tweetsSummary_v4.0newSet.csv")
head(tweetsRaw)
dim(tweetsRaw)

# Filter useless data
# tweets = subset(tweetsRaw, select=-c(id, userIdStr, twitterIdStr, messageIsDirect, postHourOfDay, postDayOfWeek))
# Normalizagin data (Inverting data where higher is worst)
tweets = subset(tweetsRaw, select=c(id))
tweets$messageReach            = (tweetsRaw$messageIsDirect) * tweetsRaw$messageMentionsCount + (1 - tweetsRaw$messageIsDirect) * (tweetsRaw$userFollowersCount) + 1
tweets$messageReachRatio       = 1 - 1 / ((tweets$messageReach / 10000) + 1) 
tweets$clicksRatio             = 1 - 1 / ((tweetsRaw$clicksCount / 20) + 1) 
tweets$retweetsRatio           = 1 - 1 / ((tweetsRaw$retweetsCount / 10) + 1) 
tweets$favoritesRatio          = 1 - 1 / ((tweetsRaw$favoritesCount / 10) + 1)
tweets$userKloutLevel          = tweetsRaw$userKloutScore / 100
tweets$userMozLevel            = tweetsRaw$userMozScore   / 100
tweets$messageHasMedia         = tweetsRaw$messageHasMedia
tweets$userFollowersRatio      = 1 - 1 / ((tweetsRaw$userFollowersCount / 10000) + 1)
tweets$userListedRatio         = 1 - 1 / ((tweetsRaw$userListedCount / 500) + 1)
tweets$userVerified            = tweetsRaw$userVerified
tweets$id = NULL

tweets$clicksRatio             = 1 - 1 / ((tweetsRaw$clicksCount * 100000) + 1) 
tweets$retweetsRatio           = 1 - 1 / ((tweetsRaw$retweetsCount * 100000) + 1) 
tweets$favoritesRatio          = 1 - 1 / ((tweetsRaw$favoritesCount * 100000) + 1)

# Custom normalization
tweets = subset(tweetsRaw, select=c(id))
tweets$messageReach            = (tweetsRaw$messageIsDirect) * tweetsRaw$messageMentionsCount + (1 - tweetsRaw$messageIsDirect) * (tweetsRaw$userFollowersCount) + 1
tweets$messageReachRatio       = customNormalizationMean(tweets$messageReach)
tweets$clicksRatio             = customNormalizationMean(tweetsRaw$clicksCount) 
tweets$retweetsRatio           = customNormalizationMean(tweetsRaw$retweetsCount)
tweets$favoritesRatio          = customNormalizationMean(tweetsRaw$favoritesCount) 
tweets$userKloutLevel          = tweetsRaw$userKloutScore / 100
tweets$userMozLevel            = tweetsRaw$userMozScore   / 100
tweets$messageHasMedia         = tweetsRaw$messageHasMedia
tweets$userFollowersRatio      = customNormalizationMean(tweetsRaw$userFollowersCount)
tweets$userListedRatio         = customNormalizationSd(tweetsRaw$userListedCount)
tweets$userVerified            = tweetsRaw$userVerified
tweets$id = NULL

tweets$clicksRatio
hist(tweets$messageReachRatio)
hist(tweets$clicksRatio)
hist(tweets$retweetsRatio)
hist(tweets$favoritesRatio)
hist(tweets$userFollowersRatio)
hist(tweets$userListedRatio)

summary(tweets);
M <- cor(tweets)
corrplot.mixed(M, tl.pos="lt")
?corrplot.mixed

# Tests for normalizagin data
originalVar = tweets$messageReach
summary(originalVar);
sd(originalVar)
boxplot(originalVar)
hist(originalVar)

checkingVar = 1 - 1 / ((1 * originalVar / (sd(originalVar) ^ 0.7)) + 1) 
#checkingVar = 1 - 1 / ((originalVar / 10000) + 1)
summary(checkingVar)
sd(checkingVar)
boxplot(checkingVar)
hist(checkingVar, breaks=20)

customNormalizationMean = function(original) {
  return(1 - 1 / (((original) / (mean(originalVar))) + 1))
}

customNormalizationSd = function(original) {
  return(1 - 1 / ((original / sd(originalVar)) + 1))
}

# Display data
head(tweets, n=6)
dim(tweets)
names(tweets)

# Basic plots
# Number of clicks
boxplot(tweets$clicksCount)
hist(tweets$clicksCount)
# Followers count
boxplot(tweets$userFollowersCount)
hist(tweets$userFollowersCount)
# Followers count
boxplot(tweets$userFollowFriendRatio)
hist(tweets$userFollowFriendRatio)
# CTR
boxplot(tweets$clicksCount*100 / max(tweets$userFollowersCount,1))
hist(tweets$clicksCount*100 / tweets$userFollowersCount)


# CPA analysis
help(prcomp)
tweetsPca = prcomp(tweets, center = TRUE, scale. = TRUE) 
tweetsPca$rotation
tweetsRotationAbs = abs(tweetsPca$rotation)
tweetsRotationAbs
plot(tweetsPca, type = "l")
names(tweets)


# GET PRINCIPAL COMPONENTS:

# 1: messageReachRatio, userKloutScore, userMozScore, userFollowersCount, userListedCount,
tweetsRotationAbs[order(-tweetsRotationAbs[,"PC1"]),1]
audience_cols = c(2,6,7,9,10);
plot(nipals(tweets[,audience_cols]), main = "Audience indicators (circle of correlations)", cex.main = 1)

# 2: favoritesRatio, retweetsRatio, clicksRatio
tweetsRotationAbs[order(-tweetsRotationAbs[,"PC2"]),2]
impact_cols = c(3,4,5);
plot(nipals(tweets[,impact_cols]), main = "Impact indicators (circle of correlations)", cex.main = 1)

# 3: messageHasMedia, userVerified
tweetsRotationAbs[order(-tweetsRotationAbs[,"PC3"]),3]
messageContent_cols = c(8,11);
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
#dbSendQuery(db, "UPDATE stats_raw set predictedClicksRatio = NULL, predictedRetweetsRatio = NULL, predictedFavoritesRatio = NULL WHERE 1");

# Control test
result <- dbSendQuery(db, "SELECT actualRetweetsRatio, actualFavoritesRatio, actualClicksRatio FROM stats_raw WHERE 1 LIMIT 100000");
controlData = dbFetch(result, n=-1)
controlData$avgImpact = controlData$actualRetweetsRatio + controlData$actualFavoritesRatio + controlData$actualClicksRatio
meanAvgImpact = mean(controlData$avgImpact)
controlData$stdError  = (controlData$avgImpact - meanAvgImpact)

# Regular Neural network test
result <- dbSendQuery(db, "SELECT id, predictionError FROM stats_raw WHERE predictionError IS NOT NULL LIMIT 100000");
errorData = dbFetch(result, n=-1)

# Square error
#errors = controlData$stdError^2;
errors = errorData$predictionError^2;

# Init vars
data = c();

# Build linear model
errorsLm = lm(errors ~ c(1:length(errors)))
data$coefA = errorsLm$coefficients[[1]];
data$coefB = errorsLm$coefficients[[2]];

# Show summary
summary(errors)
data$totalError = sum(errors);
data$meanError  = mean(errors);
data$maxError   = max(errors);
data$minError   = min(errors);
data$success    = length(errors[errors < 0.25]);
data$successPer = data$success / length(errors);

# Plot data and linear model
#par(mfrow=c(1,1), mar=c(1,1,1,1))
plot.ts(errors, type="c");
abline(errorsLm, col="red");

# Finally, print data
print(data);
