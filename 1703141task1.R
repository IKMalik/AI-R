
#read input data and do pre-processing
mydata= read.csv('C:/Users/Ibrahim/Desktop/Rstuff\\iris.csv', sep=",")
myrealdata = read.csv('C:/Users/Ibrahim/Desktop/Rstuff\\iris_real.csv', sep=",")

mydata = na.omit(mydata) # remove missing
mydata = scale(mydata) # standardize variables

# K MEANS

# below are different sized k means clusterings
fit <- kmeans(mydata, 5) # 5 clusters
fitt <- kmeans(mydata, 4) # 4 clusters
fittt <- kmeans(mydata, 3) # 3 clusters

# get each cluster group so they can be used to view on plot
kgroups1 <- fit$cluster
kgroups2 <- fitt$cluster
kgroups3 <- fittt$cluster

# scatter plot shows each cluster grouping vs just plot(mydata) which does not
plot(mydata, col=kgroups1, main='cluster of size 5')
plot(mydata, col=kgroups2, main='cluster of size 4')
plot(mydata, col=kgroups3, main='cluster of size 3')

# WK COMPARISONS

# get wr file
source('C:/Users/Ibrahim/Desktop/Rstuff\\WK_R.r')

# calc wk val between differnt k sized clusterings
wk <- WK_R(kgroups1, kgroups2)
wk2 <- WK_R(kgroups2, kgroups3)
wk3 <- WK_R(kgroups1, kgroups3)

# HIERACHICAL 

distMatrix <- dist(mydata, method = 'euclidean') # create dist matrix of Euclidean dist between data points

fit <- hclust(distMatrix, method='average') # clustering perfomed using average
fitt <- hclust(distMatrix, method='single') # clustering perfomed uisng single
fittt <- hclust(distMatrix, method='complete') # clustering perfomed using complete 

plot(fit) # dendogram plots of the different method types
plot(fitt)
plot(fittt)


