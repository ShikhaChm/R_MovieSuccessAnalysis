library(corrplot)

source("C:\\Work\\movieProject\\MovieProjectShikha\\code\\helperFunction.r")
dataPath <- "C:\\Work\\movieProject\\MovieProjectShikha\\data\\dataExtract\\movie_data.csv"
#### Factor Model
allData = loadData(dataPath)

genreRoiAll = getGenreData(allData)
genreRoi = genreRoiAll[(!genreRoiAll$ROI=='NA'),][, list(Med_ROI=median(ROI),Med_NetProfit=median(NetProfit), numMovies=length(ROI)), by = Genre]
plotRoiAll = getPlotData(allData)
plotTable = plotRoiAll[(!plotRoiAll$ROI=='NA'),][, list(Med_ROI=median(ROI),Med_NetProfit=median(NetProfit),numMovies=length(ROI)), by = Plot]

### Prepare Data For Regression
RegPrep = allData[(!allData$ROI=='NA'),c('movie_title','ROI','NetProfit','director_facebook_likes','actor_1_facebook_likes','actor_2_facebook_likes','actor_3_facebook_likes','cast_total_facebook_likes','duration','genres','plot_keywords')]
colnames(RegPrep) = c('movie','ROI','NetProfit','DirectorFb','actor1Fb','actor2Fb','actor3Fb','castTotalFb','duration','genres','plot')
topGenresNP = tail(genreRoi[order(genreRoi$Med_NetProfit)])
for(g in tail(topGenresNP,5)$Genre) {
  RegPrep[,paste('is',g,sep='')]= apply(RegPrep, 1, function(x) as.numeric(is.element(g,strsplit(x[10],'\\|')[[1]])) )
}
### Creating the unified genreRank
RegPrep[,'genreRank'] = apply(RegPrep, 1, function(x) genreRank(genreRoi,x[10]) )
#### Creating the unified plotRank
RegPrep[,'plotRank'] = apply(RegPrep, 1, function(x) plotRank(plotTable,x[11]) )


regData = RegPrep[,c('ROI','NetProfit', 'DirectorFb', 'actor1Fb', 'actor2Fb', 'actor3Fb', 'castTotalFb', 'isMusic', 'isFantasy', 'isHorror', 'isAnimation', 'isFamily','duration','genreRank', 'plotRank')]
regData = sapply(regData,as.double)
regData[is.na(regData)]=0

## Bound the Data for reducing the impact of outliers
columnsToBound = c('ROI','NetProfit','DirectorFb','actor1Fb','castTotalFb','genreRank','plotRank')
bndPercentile=0.95
for(n in columnsToBound) {
  print(n)
  bnd = as.double(quantile(abs(regData[,n]),bndPercentile,na.rm = TRUE))
  regData[abs(regData[,n])>bnd,n] = sign(regData[abs(regData[,n])>bnd,n])*bnd
}
regData = as.data.frame(regData)

#### Correlation Analysis: Before we go ahead with regression we do a basic correlation analysis
png(filename = 'results\\preProcessGraphs\\factorCorr.png')
respCor = cor(regData)
corrplot(respCor,type='upper',mar = c(5,0,5,0))
dev.off()
#### We see FB Likes are highly correlated

##### ROI Regression :
#### Regression Model 1 : ROI ~ DirectorFb+castTotalFb+isMusic+isFantasy+isHorror+isAnimation+isFamily+duration+genreRank+plotRank
reg1 = lm(formula = ROI ~ DirectorFb +castTotalFb+isMusic+isFantasy+isHorror+isAnimation+isFamily+duration+genreRank+plotRank, data = regData)
summary(reg1)
#After t-stat based factor selection
#### Regression Model 2 : ROI ~ DirectorFb+isMusic+isFantasy+isHorror+plotRank
reg2 = lm(formula = ROI ~ DirectorFb+isMusic+isFantasy+isHorror+plotRank, data = regData)
summary(reg2)
##### Note F-stat jumps
plot(regData$DirectorFb,regData$ROI,col='blue')
regData$plotRank2 = regData$plotRank*regData$plotRank
## Post regression analysis
plot(regData$ROI,fitted(reg2), col='blue')
plot(reg2)


##### NetProfit Regression :
#### Regression Model 3 : NetProfit ~ DirectorFb +castTotalFb+isMusic+isFantasy+isHorror+isAnimation+isFamily+genreRank+plotRank
reg3 = lm(formula = NetProfit ~ DirectorFb +castTotalFb+isMusic+isFantasy+isHorror+isAnimation+isFamily+duration+genreRank+plotRank, data = regData)
summary(reg3)
#After t-stat based factor selection ## Note that different factors are getting selected.
#### Regression Model 4 : NetProfit ~ DirectorFb +castTotalFb+isMusic+isFantasy+isHorror+isAnimation+isFamily+genreRank+plotRank
reg4 = lm(formula = NetProfit ~ DirectorFb +castTotalFb+isHorror+isFamily+genreRank+plotRank, data = regData)
summary(reg4)
#### As expected there is considerable improvement in F-stat
#### Post regression analysis
