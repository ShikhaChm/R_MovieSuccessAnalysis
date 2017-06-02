library(corrplot)

#Set working directory
setwd("C:/Work/movieProject/MovieProjectShikha")

source("C:\\Work\\movieProject\\MovieProjectShikha\\code\\helperFunction.r")
dataPath <- "C:\\Work\\movieProject\\MovieProjectShikha\\data\\dataExtract\\movie_data.csv"
# Nature of data
allData = loadData(dataPath)


png(filename = 'results\\preProcessGraphs\\responseVars1.png')
par(mfrow=c(2,2))
# Step 0: Check the distribution of Independent variables
hist(allData[abs(allData$ROI)<quantile(abs(allData$ROI),0.95,na.rm=TRUE),]$ROI, breaks=60,col="blue",main="Histogram of ROI",xlab="ROI", 
     ylab="Frequency",xaxt='n') 
axis(side=1, at=seq(-1,10, 1), labels=seq(-1,10,1))
hist(allData[abs(allData$NetProfit)<quantile(abs(allData$NetProfit),0.95,na.rm=TRUE),]$NetProfit, breaks=60,col="blue",main="Histogram of NetProfit",xlab="NetProfit", ylab="Frequency")
hist(allData[allData$gross<quantile(allData$gross,0.95,na.rm=TRUE),]$gross, breaks=60,col="blue",main="Histogram of Gross",xlab="Gross", ylab="Frequency")
hist(allData$imdb_score, breaks=40,col="blue",main="Histogram of imdb_score",xlab="imdb_score", ylab="Frequency",xaxt='n')
axis(side=1, at=seq(0,10, 1), labels=seq(0,10,1))
dev.off()

png(filename = 'results\\preProcessGraphs\\responseVars2.png')
par(mfrow=c(2,2))
hist(allData[(allData$movie_facebook_likes>150)&(allData$movie_facebook_likes<50000)]$movie_facebook_likes, breaks=100,
     col="blue",main="Hist:Movie_facebook_likes",xlab="movie_facebook_likes", ylab="Frequency")
hist(allData[allData$num_voted_users<quantile(allData$num_voted_users,0.95,na.rm=TRUE),]$num_voted_users, breaks=60,col="blue",main="Hist:Num_voted_users",xlab="num_voted_users", ylab="Frequency")
hist(allData[allData$num_critic_for_reviews<quantile(allData$num_critic_for_reviews,0.95,na.rm=TRUE),]$num_critic_for_reviews, 
     breaks=60,col="blue",main="Hist:Num_critic_for_reviews",xlab="num_critic_for_reviews", ylab="Frequency")
hist(allData[allData$num_user_for_reviews<quantile(allData$num_user_for_reviews,0.95,na.rm=TRUE),]$num_user_for_reviews, breaks=60,col="blue",main="Hist:Num_user_for_reviews",xlab="num_user_for_reviews", ylab="Frequency")
dev.off()

responseVars = allData[!allData$ROI=='NA'][,c('ROI','NetProfit','imdb_score','movie_facebook_likes','num_voted_users','num_critic_for_reviews','num_user_for_reviews')]
colnames(responseVars) = c('ROI','NetProfit','imbd','movieFbLikes','votedUsers','criticReviews','userReviews')
responseVars[is.na(responseVars)]=0
respCor = cor(responseVars)
png(filename = 'results\\preProcessGraphs\\responseVarsCor.png')
corrplot(respCor,type='upper')
dev.off()

#demographic factors
#### Language
langDataAll = getLanguageData(allData)
langData = langDataAll[, list(ROI=median(ROI),NetProfit=median(NetProfit),numMovies=length(ROI)),by=language][order(ROI),]
langData[langData$numMovies>5]
#### Language Over years
langDataYr = langDataAll[, list(ROI=median(ROI),Med_NetProfit=median(NetProfit),Total_NetProfit=sum(NetProfit),
                                numMovies=length(ROI)),by=c('decade','language')][order(decade),]
engData = langDataYr[langDataYr$language=='English']

# Country
countryDataAll = getCountryData(allData)
countryData = countryDataAll[, list(Med_ROI=median(ROI),Med_NetProfit=median(NetProfit),Total_NetProfit=sum(NetProfit),
                                    numMovies=length(ROI)),by=country][order(Med_ROI),]
tail(countryData[order(Total_NetProfit),])
#### Country Over Years
countryData = countryDataAll[, list(Med_ROI=median(ROI),Med_NetProfit=median(NetProfit),Total_NetProfit=sum(NetProfit),numMovies=length(ROI)),by=c('decade','country')][order(Med_ROI),]
countryData[(countryData$country=='USA')&(countryData$numMovies>10)][order(decade),]
countryData[(countryData$country=='UK')&(countryData$numMovies>10)][order(decade),]

#Content Rating Data
contentDataAll = allData[(!(allData$ROI=='NA'))&(!(allData$language=="")),c('content_rating','ROI','NetProfit')]
contentData = contentDataAll[, list(Med_ROI=median(ROI),Med_NetProfit=median(NetProfit)),by=content_rating][order(Med_ROI),]

summary(allData$color)

png(filename = 'results\\preProcessGraphs\\durationAspect.png')
par(mfrow=c(2,1))
#check the impact of technical factors
durData = getDurationData(allData)
par(mar = c(5,5,2,5))
with(durData, plot(DurationRange, ROI, type="l", col="red3",xlab="Duration(minutes)", ylab="avg ROI", ylim=c(0,30)))
# par(new = T)
with(durData, plot(DurationRange, numMovies,type="l", col="blue", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'numMovies')
legend("topright", legend=c("ROI", "numMovies"), lty=c(1,1), pch=c(NA, NA), col=c("red3", "blue"))

par(mar = c(5,5,2,5))
with(durData, plot(DurationRange, NetProfit, type="l", col="red3",xlab="Duration(minutes)", ylab="median Net Profits ", ylim=c(-10e7,10e7)))
par(new = T)
with(durData, plot(DurationRange, numMovies,type="l", col="blue", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'numMovies')
legend("topright", legend=c("Med NetProfit", "numMovies"), lty=c(1,1), pch=c(NA, NA), col=c("red3", "blue"))


#bounding aspect ratio with 5 percentile and 99 percentile
aspect= getAspectData(allData)
# aspectRatio over years
aspectDataDecade = aspectDataAll[, list(aspect_ratio=median(aspect_ratio),numMovies=length(aspect_ratio)),by=c('decade')]
par(mar = c(5,5,2,5))
with(aspectDataDecade, plot(decade,aspect_ratio, type="l", col="red3", xlab="Year",ylab="Aspect Ratio"))
par(new = T)
with(aspectDataDecade, plot(decade, numMovies,type="l", col="blue", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'numMovies')
legend("topleft", legend=c("aspectRatio", "numMovies"), lty=c(1,1), pch=c(NA, NA), col=c("red3", "blue"))

dev.off()


dev.copy(png,'FactorsNext.png')
par(mfrow=c(2,2))
aspectData= aspect[[1]]
aspectDataAll = aspect[[2]]
par(mar = c(5,5,2,5))
with(aspectData, plot(aspect_ratio, ROI, type="l", col="red3",xlab="Aspect Ratio", ylab="avg ROI", ylim=c(0,15)))
par(new = T)
with(aspectData, plot(aspect_ratio, numMovies,type="l", col="blue", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'numMovies')
legend("topright", legend=c("ROI", "numMovies"), lty=c(1,1), pch=c(NA, NA), col=c("red3", "blue"))

par(mar = c(5,5,2,5))
with(aspectData, plot(aspect_ratio, NetProfit, type="l", col="red3",xlab="Aspect Ratio", ylab="median Net Profits", ylim=c(-1e8,3e8)))
par(new = T)
with(aspectData, plot(aspect_ratio, numMovies,type="l", col="blue", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'numMovies')
legend("topright", legend=c("Med NetProfits", "numMovies"), lty=c(1,1), pch=c(NA, NA), col=c("red3", "blue"))



# Step 3: genre factors genres and plot keywords
genreRoiAll = getGenreData(allData)
genreRoi = genreRoiAll[(!genreRoiAll$ROI=='NA'),][, list(Med_ROI=median(ROI),Med_NetProfit=median(NetProfit), numMovies=length(ROI)), by = Genre]
topGenresNP = tail(genreRoi[order(genreRoi$Med_NetProfit)])
topGenresROI = tail(genreRoi[order(genreRoi$Med_ROI)])
topGenresNP
topGenresROI

genreByDecade = genreRoiAll[(!genreRoiAll$ROI=='NA'),][, list(Med_ROI=median(ROI),Med_NetProfit=median(NetProfit), numMovies=length(ROI)), by = c('decade','Genre')]
tail(genreByDecade[genreByDecade$Genre=='Animation'])
tail(genreByDecade[genreByDecade$Genre=='Family'])

png(filename = 'results\\preProcessGraphs\\genre.png')
par(mfrow=c(2,1))
genre2010 =genreByDecade[genreByDecade$decade==2010]
topGenres2010=tail(genre2010[order(genre2010$Med_NetProfit)])
counts <- topGenres2010$Med_NetProfit
barplot(counts, main="Top Genres 2000-2010", names.arg=topGenres2010$Genre, col='blue', ylab = "Med Net Profits")

genre2020 =genreByDecade[genreByDecade$decade==2020]
topGenres2020=tail(genre2020[order(genre2020$Med_NetProfit)])
counts <- topGenres2020$Med_NetProfit
barplot(counts, main="Top Genres 2010-2020", names.arg=topGenres2020$Genre, col='blue', ylab = "Med Net Profits")

dev.off()

### Plot Keywords Analysis
plotRoiAll = getPlotData(allData)
plotTable = plotRoiAll[(!plotRoiAll$ROI=='NA'),][, list(Med_ROI=median(ROI),Med_NetProfit=median(NetProfit),numMovies=length(ROI)), by = Plot]
plotRoi = plotTable[plotTable$numMovies>20]
tail(plotRoi[order(plotRoi$Med_ROI)])
tail(plotRoi[order(plotRoi$numMovies)])


plotByDecade = plotRoiAll[(!plotRoiAll$ROI=='NA'),][, list(Med_ROI=median(ROI),Med_NetProfit=median(NetProfit),numMovies=length(ROI)), by = c('decade','Plot')]
tail(plotByDecade[plotByDecade$Plot=='friend'])
tail(plotByDecade[plotByDecade$Plot=='police'])

png(filename = 'results\\preProcessGraphs\\plotKeywords.png', width = 1000,height = 1000)
par(mfrow=c(2,1))

plot2010 =plotByDecade[(plotByDecade$decade==2010)&(plotByDecade$numMovies>5)]
topPlots2010=tail(plot2010[order(plot2010$Med_NetProfit)])
counts <- topPlots2010$Med_NetProfit
barplot(counts, main="Top Plots 2000-2010", names.arg=topPlots2010$Plot, col='blue', ylab = "Med Net Profits")

plot2020 =plotByDecade[(plotByDecade$decade==2020)&(plotByDecade$numMovies>5)]
topPlots2020=tail(plot2020[order(plot2020$Med_NetProfit)])
counts <- topPlots2020$Med_NetProfit
barplot(counts, main="Top Plots 2010-2020", names.arg=topPlots2020$Plot, col='blue', ylab = "Med Net Profits")
dev.off()

png(filename = 'results\\preProcessGraphs\\facebook.png')
#checking relevance of facebook data
fbData = getFbData(allData)
plot(fbData$title_year,fbData$avgLikesPrMovie,main="Distrib. of FB activity over years",xlab="Year", ylab="Avg Likes Per Movie", pch=19,col="blue")
lines(lowess(fbData$title_year,fbData$avgLikesPrMovie), col="red")
dev.off()


