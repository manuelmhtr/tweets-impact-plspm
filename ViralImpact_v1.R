library("plspm")
# Viral Impact Predition on Tweets

setwd("/Users/manuelmhtr/Projects/Learning-R/ViralImpact");

list.files("./data")
tweets = read.csv("./data/tweetsSummary_v1.csv")

# Display data
head(tweets, n=6)
dim(tweets)
names(tweets)

# Basic plots
# Text length
boxplot(tweets$messageTextLength)
hist(tweets$messageTextLength)
# Number of words
boxplot(tweets$messageWordsCount)
hist(tweets$messageWordsCount)
# Number of clicks
boxplot(tweets$clicksCount)
hist(tweets$clicksCount)
# Followers count
boxplot(tweets$userFollowersCount)
hist(tweets$userFollowersCount)

# Basic stats
summary(tweets)

# Build the model
Content     = c(0, 0, 0)
Influencer  = c(0, 0, 0)
ViralImpact = c(1, 1, 0)

# Matrix created by row binding
tweetsInner = rbind(Content, Influencer, ViralImpact)
colnames(tweetsInner) = rownames(tweetsInner)
tweetsInner

# plot the inner matrix
innerplot(tweetsInner)

# define list of indicators
names(tweets)
tweetsOuter = list(7:13, c(17:26,28), 3:6)
tweetsOuter

# Tell variables are reflexive or formative
tweetsModes = c("A", "A", "A")

# run plspm analysis
help(plspm)
analyzeData = tweets
analyzeData$messageIsDirect         = analyzeData$messageIsDirect * -1
analyzeData$userDefaultProfile      = analyzeData$userDefaultProfile * -1
analyzeData$userDefaultProfileImage = analyzeData$userDefaultProfileImage * -1
analyzeData$userStatusesCount       = analyzeData$userStatusesCount * -1
head(analyzeData)
tweetsPls = plspm(analyzeData, tweetsInner, tweetsOuter, tweetsModes, maxiter=200)

# summarized results
summary(tweetsPls)

# Show results
tweetsPls$path_coefs
tweetsPls$inner_model
tweetsPls$inner_summary
plot(tweetsPls)
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

# Goodness of fit (should be higher than 0.70)
tweetsPls$gof

# Validation
# Bootstraping: Add some noise to the original data to make sure that the model correctly
# describes data.
bootPls = plspm(analyzeData, tweetsInner, tweetsOuter, tweetsModes, boot.val = TRUE, br = 100)

# Bootstraping results
bootPls$boot



# principal components
tweetsPc = princomp(tweets);
summary(tweetsPc, loadings=TRUE)
help(princomp);

