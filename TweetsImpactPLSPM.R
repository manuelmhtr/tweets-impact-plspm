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


tweets = subset(tweetsRaw, select=c(id))
tweets$messageReach            = (tweetsRaw$messageIsDirect) * tweetsRaw$messageMentionsCount + (1 - tweetsRaw$messageIsDirect) * (tweetsRaw$userFollowersCount) + 1
tweets$messageReachRatio       = 1 - 1 / ((tweets$messageReach / getFactor3(tweets$messageReach)) + 1) 
tweets$clicksRatio             = 1 - 1 / ((tweetsRaw$clicksCount / getFactor3(tweetsRaw$clicksCount)) + 1) 
tweets$retweetsRatio           = 1 - 1 / ((tweetsRaw$retweetsCount / getFactor3(tweetsRaw$retweetsCount)) + 1) 
tweets$favoritesRatio          = 1 - 1 / ((tweetsRaw$favoritesCount / getFactor3(tweetsRaw$favoritesCount)) + 1)
tweets$userKloutLevel          = tweetsRaw$userKloutScore / 100
tweets$userMozLevel            = tweetsRaw$userMozScore   / 100
tweets$messageHasMedia         = tweetsRaw$messageHasMedia
tweets$userFollowersRatio      = 1 - 1 / ((tweetsRaw$userFollowersCount / getFactor3(tweetsRaw$userFollowersCount)) + 1)
tweets$userListedRatio         = 1 - 1 / ((tweetsRaw$userListedCount / getFactor3(tweetsRaw$userListedCount)) + 1)
tweets$userVerified            = tweetsRaw$userVerified
tweets$id = NULL

hist(tweets$messageReachRatio);
hist(tweets$clicksRatio);
hist(tweets$retweetsRatio);
hist(tweets$favoritesRatio);
hist(tweets$userFollowersRatio);
hist(tweets$userListedRatio);

sd(tweets$messageReachRatio);
sd(tweets$clicksRatio);
sd(tweets$retweetsRatio);
sd(tweets$favoritesRatio);
sd(tweets$userFollowersRatio);

sd(tweetsRaw)
summary(tweetsRaw)
#tweets$impactCount             = tweetsRaw$clicksCount + tweetsRaw$retweetsCount + tweetsRaw$favoritesCount
#tweets$impactRatio             = 1 - 1 / ((tweets$impactCount / 10) + 1)
#sd(tweets$impactRatio)
tweets$clicksRatio             = 1 - 1 / ((tweetsRaw$messageReach / 20) + 1) 
sd(tweets$clicksRatio);
skewness(tweets$clicksRatio);
hist(tweets$favoritesRatio);

sd = c();
kt = c();
sk = c();
x  = seq(10000, 100000, by=1000);
for(i in x) {
  value = 1 - 1 / ((tweets$messageReach / i) + 1);
  sd  = c(sd, sd(value) / mean(value));
  kt  = c(kt, kurtosis(value));
  sk  = c(sk, skewness(value));
}
sd
plot(x, sd, col="blue");

normValue = function(value) {
  print(mean(value));
  print(sd(value));
  print(skewness(value));
  print(kurtosis(value));
  print(sd(value) / mean(value));
  print(mean(value)^3 / sd(value)^3);
  print(quantile(value));
}
normValue(tweets$messageReach);
normValue(tweetsRaw$retweetsCount);
normValue(tweets$messageReachRatio);
normValue(tweets$clicksRatio);

boxplot(tweets$messageReach);
summary(tweets$messageReach);
summary(tweetsRaw$retweetsCount);
median(tweetsRaw$clicksCount);
hist(1 - 1 / ((tweetsRaw$clicksCount / 7.8) + 1));
hist(1 - 1 / ((tweets$messageReach / 8243) + 1));
hist(1 - 1 / ((tweetsRaw$favoritesCount / 2.5) + 1));
hist(1 - 1 / ((tweets$messageReach / (mean(tweets$messageReach))) + 1));
hist(1 - 1 / ((tweetsRaw$clicksCount / (mean(tweetsRaw$clicksCount)*5)) + 1));

tweets$clicksRatio
hist(tweets$messageReachRatio)
hist(tweets$clicksRatio)
hist(tweets$retweetsRatio)
hist(tweets$favoritesRatio)
hist(tweets$userFollowersRatio)
hist(tweets$userListedRatio)
quantile(tweetsRaw$clicksCount)
quantile(tweets$retweetsRatio)[[2]]


hist(1 - 1 / ((tweetsRaw$clicksCount / 18) + 1));
hist(1 - 1 / ((tweets$messageReach / 30000) + 1));

getFactor1 = function(collection) {
  quants = quantile(collection);
  print(quants);
  wide   = max((quants[[4]] - quants[[2]]), 1) * 2;
  print(wide);
  med    = max(median(collection), 0.001);
  dest   = med / wide;
  print(med);
  print(dest);
  return ((med - dest * med) / dest);
}
factor1 = getFactor1(tweets$messageReach);
print(factor1);
factor1 = getFactor1(tweetsRaw$clicksCount);
print(factor1);

getFactor2 = function(collection) {
  quants = quantile(collection);
  #wide   = (max(collection) - min(collection));
  wide   = 3 * sd(collection);
  med    = mean(collection);
  dest   = med / wide;
  return ((med - dest * med) / dest);
}
print(getFactor2(tweets$messageReach));
print(getFactor2(tweetsRaw$clicksCount));
print(getFactor2(tweetsRaw$favoritesCount));

getFactor3 = function(collection) {
  quants = quantile(collection);
  
  #med   = (quants[[4]] - quants[[2]]) / 2;
  med    = mean(collection);
  eqs   = skewness(collection) / 75;
  dest  = 0.5 - 0.5 * (exp(eqs) - exp(-eqs)) / (exp(eqs) + exp(-eqs));
  #dest   = 0.75 - 0.5 / (1+exp(-skewness(collection)/1));
  print(med);
  print(dest);
  return ((med - dest * med) / dest);
}
print(getFactor3(tweets$messageReach));
print(getFactor3(tweetsRaw$clicksCount))
print(getFactor3(tweetsRaw$favoritesCount));


summary(tweets);
M <- cor(tweets)
corrplot.mixed(M, tl.pos="lt")
?corrplot.mixed

# Tests for normalizagin data
originalVar = tweets$impactCount
summary(originalVar);
sd(originalVar)
boxplot(originalVar)
hist(originalVar)

checkingVar = 1 - 1 / ((1 * originalVar / (sd(originalVar) ^ 0.7)) + 1) 
checkingVar = 1 - 1 / ((originalVar / 10) + 1)
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
#impact_cols = c(13);
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
result <- dbSendQuery(db, "SELECT id, actualRetweetsRatio, actualFavoritesRatio, actualClicksRatio, predictionError FROM stats_raw WHERE predictionError IS NOT NULL LIMIT 100000");
dbData  = dbFetch(result, n=-1);
results = controlData$actualRetweetsRatio + controlData$actualFavoritesRatio + controlData$actualClicksRatio;

# Control errors
stdErrors = (results - mean(results));
errors = stdErrors;

# Prediction errors
errors = errorData$predictionError;



# Show summary
summary(errors);
summary(results);

# Init vars
data = c();

data$mad = mean(abs(errors));
data$mse = mean(errors^2);
data$sse = sum(errors^2);
data$maxError = max(errors^2);

print(data);
(0.1480-0.1987)/0.1987
(0.0574-0.0986)/0.0986
(2651.45-4550.20)/4550.20
(5.7538-7.3601)/7.3601

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
#par(mfrow=c(1,1), mar=c(1,1,1,1))
controlError = controlData$stdError^2;
neuralNetworkError = errorData$predictionError^2;
plot.ts(controlError, type="o", col="#00000033", ylim=c(0, 7));
plot.ts(neuralNetworkError, type="o", col="#00000033", ylim=c(0, 7));
plot.ts(errors, type="o", col="#00000033", ylim=c(0, 7));
#abline(errorsLm, col="red");

# Finally, print data
print(data);

