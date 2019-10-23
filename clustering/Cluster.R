#read input data and do pre-processing

mydata= read.csv('H:/R\\iris.csv', sep=",")

myrealdata = read.csv('H:/R\\iris_real.csv', sep=",")



mydata = na.omit(mydata) # remove missing
mydata = scale(mydata) # standardize variables
myrealdata = scale(myrealdata)



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


# HIERACHICAL 


distMatrix <- dist(mydata, method = 'euclidean') # create dist matrix of Euclidean dist between data points


hclus1 <- hclust(distMatrix, method='average') # clustering perfomed using average dist between pairs

hclus2 <- hclust(distMatrix, method='single') # clustering perfomed uisng single // smallest dist 

hclus3 <- hclust(distMatrix, method='complete') # clustering perfomed using complete  // largest dist

plot(hclus1) # dendograms
plot(hclus2)
plot(hclus3)

hgroups1 <- cutree(hclus1, k=5) # split into k clusters
hgroups2 <- cutree(hclus2, k=5)
hgroups3 <- cutree(hclus3, k=5)

plot(mydata, col=hgroups1)
plot(mydata, col=hgroups2)
plot(mydata, col=hgroups3) # scatter of split hierachial clusters


# WK COMPARISONS


# get wr file

source('H:/R\\WK_R.r')


# calc wk val between differnt k sized clusterings

wk <- WK_R(kgroups1, myrealdata)

wk2 <- WK_R(kgroups2, myrealdata)

wk3 <- WK_R(kgroups3, myrealdata)


# calc wk val between differnt hierachial clusterings

wk4 <- WK_R(hgroups1, myrealdata)

wk5 <- WK_R(hgroups2, myrealdata)

wk6 <- WK_R(hgroups3, myrealdata)

kkappa <- c(wk, wk2, wk3)
hkappa <- c(wk4, wk5, wk6)

plot(kkappa)
plot(hkappa)


