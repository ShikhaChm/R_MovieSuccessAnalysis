library("data.table")

## Load Data
loadData <- function(path) 
{
  allData = as.data.table(read.csv(dataPath))
  allData[,"NetProfit"] = (allData[,"gross"]-allData[,"budget"])
  allData[,"ROI"] = (allData[,"gross"]-allData[,"budget"])/allData[,"budget"]
  allData = allData[,c("movie_title", "title_year", "movie_imdb_link", "gross", "budget","ROI","NetProfit", "imdb_score", "movie_facebook_likes", "num_voted_users", "num_critic_for_reviews", "num_user_for_reviews", "director_name", "director_facebook_likes", "actor_1_name", "actor_1_facebook_likes", "actor_2_name", "actor_2_facebook_likes", "actor_3_name", "actor_3_facebook_likes", "cast_total_facebook_likes", "color", "duration", "aspect_ratio", "genres", "facenumber_in_poster", "plot_keywords", "language", "country", "content_rating")]
  allData[,'decade'] = 10*ceiling(allData$title_year/10.0)
  allData = allData[order(title_year),]
  return(allData)
}
## Get all the facebook related data
getFbData <- function(allData) {
  fbDataAll = allData[,c('title_year','movie_facebook_likes','director_facebook_likes','actor_1_facebook_likes','actor_2_facebook_likes','actor_3_facebook_likes','cast_total_facebook_likes')]
  fbDataAll = fbDataAll[!(fbDataAll$title_year=='NA')]
  fbDataAll[is.na(fbDataAll)] = 0
  fbData = fbDataAll[, list(movie_facebook_likes = sum(movie_facebook_likes)), by = title_year]
  fbData[,"director_facebook_likes"] = fbDataAll[, list(director_facebook_likes = sum(director_facebook_likes)), by = title_year]$director_facebook_likes
  fbData[,"actor_1_facebook_likes"] = fbDataAll[, list(actor_1_facebook_likes = sum(actor_1_facebook_likes)), by = title_year]$actor_1_facebook_likes
  fbData[,"actor_2_facebook_likes"] = fbDataAll[, list(actor_2_facebook_likes = sum(actor_2_facebook_likes)), by = title_year]$actor_2_facebook_likes
  fbData[,"actor_3_facebook_likes"] = fbDataAll[, list(actor_3_facebook_likes = sum(actor_3_facebook_likes)), by = title_year]$actor_3_facebook_likes
  fbData[,"cast_total_facebook_likes"] = fbDataAll[, list(cast_total_facebook_likes = sum(cast_total_facebook_likes)), by = title_year]$cast_total_facebook_likes
  fbData[,"numMovies"] = fbDataAll[, list(numMovies = length(cast_total_facebook_likes)), by = title_year]$numMovies
  fbData[,"avgLikesPrMovie"] = (fbData[,'movie_facebook_likes']+fbData[,'director_facebook_likes']+fbData[,'actor_1_facebook_likes']+fbData[,'actor_2_facebook_likes']+fbData[,'actor_3_facebook_likes']+fbData[,'cast_total_facebook_likes'])/fbData[,'numMovies']
  return(fbData)
}
extractTags <- function(decade,roi,netProfit,tagList)
{
  ret = as.data.frame(strsplit(toString(tagList),'\\|'))
  colnames(ret)=c('Tags')
  ret[,"ROI"] = roi
  ret[,"NetProfit"] = netProfit
  ret[,"decade"] = decade
  ret
}
getDurationData <- function(allData) {
  allData[,"DurationRange"] = 10
  for(i in seq(10,420,10)){
    allData[allData$duration>i,"DurationRange"]=i+10
  }
  durData = allData[!(allData$ROI=='NA'),c("DurationRange","ROI","NetProfit")] [, list(ROI = mean(ROI),NetProfit=mean(NetProfit),numMovies=length(ROI)), 
                                                                               by = DurationRange][order(DurationRange),]
  return(durData)
}

getAspectData <- function(allData) {
  aspectData = allData[(!(allData$ROI=='NA'))&(allData$aspect_ratio>1.5)&(allData$aspect_ratio<4),
                       c("aspect_ratio","ROI","NetProfit")][, list(ROI = median(ROI),NetProfit=median(NetProfit),numMovies=length(ROI)),
                                                            by = aspect_ratio][order(aspect_ratio),]
  aspectDataAll = allData[(!allData$decade=='NA')&(!allData$aspect_ratio=='NA'),c("decade","aspect_ratio")]
  
  return(list(aspectData,aspectDataAll))
}
plotRank <- function(plotTable,plotList) 
{
  wts = 0
  sm = 0;
  for(p in strsplit(plotList,'\\|')[[1]]) {
    tmp = plotTable[(plotTable$Plot==p),c('Med_ROI','numMovies')]
    tmp[,'prd'] = tmp$Med_ROI*tmp$numMovies
    if(dim(tmp)[1]>0) {
      sm = sm + as.numeric(tmp[,'prd'][1,])
      wts = wts + as.numeric(tmp[,'numMovies'][1,])
    }
  }
  mn = 0
  if(wts>0) {
    mn = sm/wts
  }
  return(mn)
}

genreRank <- function(genreRoi,genreList) 
{
  wts = 0
  sm = 0;
  for(p in strsplit(genreList,'\\|')[[1]]) {
    tmp = genreRoi[(genreRoi$Genre==p),c('Med_ROI','numMovies')]
    tmp[,'prd'] = tmp$Med_ROI*tmp$numMovies
    if(dim(tmp)[1]>0) {
      sm = sm + as.numeric(tmp[,'prd'][1,])
      wts = wts + as.numeric(tmp[,'numMovies'][1,])
    }
  }
  mn = 0
  if(wts>0) {
    mn = sm/wts
  }
  return(mn)
}

getGenreData <- function(allData) {
  genreData = allData[!(allData$ROI=='NA'),c("decade","ROI","NetProfit","genres")]
  qq= apply(genreData, 1, function(x) extractTags(x[1],x[2],x[3],x[4]))
  genreRoiAll = as.data.table(do.call("rbind", qq))
  colnames(genreRoiAll) = c("Genre","ROI","NetProfit","decade")
  genreRoiAll = transform(genreRoiAll, ROI = as.numeric(ROI))
  genreRoiAll = transform(genreRoiAll, NetProfit = as.numeric(NetProfit))
  return(genreRoiAll)
}

getPlotData <- function(allData) {
  plotData = allData[(!(allData$ROI=='NA'))&(!(allData$plot_keywords=="")),c("decade","ROI","NetProfit","plot_keywords")]
  # bounding the data
  # plotData[plotData$ROI>quantile(plotData$ROI,0.95,na.rm=TRUE),"ROI"] =quantile(plotData$ROI,0.95,na.rm=TRUE)
  pp= apply(plotData, 1, function(x) extractTags(x[1],x[2],x[3],x[4]))
  plotRoiAll = as.data.table(do.call("rbind", pp))
  colnames(plotRoiAll) = c("Plot","ROI","NetProfit","decade")
  plotRoiAll = transform(plotRoiAll, ROI = as.numeric(ROI))
  plotRoiAll = transform(plotRoiAll, NetProfit = as.numeric(NetProfit))
  return(plotRoiAll)
}

getLanguageData <- function(allData) {
  langDataAll = allData[(!(allData$ROI=='NA'))&(!(allData$language=="")),c('decade','language','ROI','NetProfit')]
  return(langDataAll)
}
getCountryData <- function(allData) {
  countryDataAll = allData[(!(allData$ROI=='NA'))&(!(allData$country=="")),c('decade','country','ROI','NetProfit')]
  return (countryDataAll)
}
getContentdata <- function(allData) {
  contentDataAll = allData[(!(allData$ROI=='NA'))&(!(allData$language=="")),c('content_rating','ROI','NetProfit')]
  return(contentDataAll)
}

