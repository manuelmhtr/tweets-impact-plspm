# Viral Impact Predition on Tweets
# Author: Manuel de la Torre

# Load libraries
library("plspm")
library("plsdepot")
# library("devtools")
# install_github("vqv/ggbiplot")
# library("ggbiplot")

# Load data
setwd("/Users/manuelmhtr/Projects/Learning-R/ViralImpact");
list.files("./data")
tweetsRaw = read.csv("./data/tweetsSummary_v3.2withMozScore.csv")
head(tweetsRaw)

# Filter useless data
# tweets = subset(tweetsRaw, select=-c(id, userIdStr, twitterIdStr, messageIsDirect, postHourOfDay, postDayOfWeek))
# Normalizagin data (Inverting data where higher is worst)
daysSinceTwitter = (as.numeric(Sys.time()) - 1142899200) / 86400 # Days since 21 march 2015
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

# tweets$userFriendsCount        = 1 - 1 / ((tweetsRaw$userFriendsCount / 10000) + 1)
# tweets$userStatusesCount       = 1 / ((tweetsRaw$userStatusesCount / 1000) + 1) - 1
# tweets$userTweetsPerDay        = 1 / ((tweetsRaw$userTweetsPerDay / 500) + 1) - 1
# tweets$userAccountAge          = tweetsRaw$userAccountAge / daysSinceTwitter
# tweets$userDefaultProfile      = tweetsRaw$userDefaultProfile
# tweets$userHasProfileImage     = 1 - tweetsRaw$userDefaultProfileImage

summary(tweets);

# Tests for normalizagin data
originalVar = tweetsRaw$retweetsCount
summary(originalVar);
boxplot(originalVar)
hist(originalVar)

checkingVar = 1 - 1 / ((1 * originalVar / 10) + 1) 
summary(checkingVar);
boxplot(checkingVar)
hist(checkingVar, breaks=20)

hist(tweets$messageReachRatio)

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
