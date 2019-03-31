#
#-----------------------------------#
#Clean the system
#

rm(list=ls())

#
#-----------------------------------#
#Download, install packages
#
packages.list<-c("corrplot",  "BMA", "betapart","vegan", "MuMIn", "lme4", "tidyverse","devtools")
required.packages <- packages.list
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(packages.list, require, character.only=T)

#
#-----------------------------------#
#Load and clean up data
#

#load data
birdCount = read_csv("birdCount.csv")
landscape = read_csv("landscape.csv")
guilds = read_csv("guildMembers.csv")
plotData = read.csv("PlotData.csv") %>% arrange(PLOT)

#prep the abundance data
plotData$Length100 = plotData$Length/100 #factor for scaling counts
birdCount100 = sweep(birdCount[,-c(1:2)], 1, c(plotData$Length100,plotData$Length100), '/') # count/100m of transect
datCount = bind_cols(birdCount[,c(1:2)],birdCount100,landscape[,-c(1:2)])

# prep guild abundance data
guildList = data.frame(matrix(nrow = nrow(birdCount100),ncol = 7))
for(i in 1:7){
  gg = data.frame(mapply(`*`,birdCount100[,-c(1:2)],guilds[i,-1])) %>% mutate(rowsum = rowSums(.)); guildList[,i] = gg$rowsum
}
colnames(guildList) = guilds$guild
guildCount = bind_cols(landscape[,c(1,2)],guildList,landscape[,-c(1,2)])
gCountLong = guildCount %>% gather('Resident':'Synanthope', key='guild', value='counts')
gCount1970 = filter(gCountLong,Year == 1970)
gCount2000 = filter(gCountLong,Year == 2000)
guildRA = bind_cols(landscape[,c(1,2)], sweep(guildList, 1, rowSums(guildList), '/'),landscape[,-c(1:2)])
guildRALong = guildRA %>% gather('Resident':'Synanthope', key='guild', value='RA')

#Prep relative abundance data
birdRA = bind_cols(birdCount100[,c(1:2)], sweep(birdCount100[,-c(1:2)], 1, rowSums(birdCount100[,-c(1:2)]), '/'),landscape[,-c(1:2)])
birdRALong = birdRA %>% gather('ACFL':'YTVI', key='species', value='RA')

#
#-----------------------------------#
#T-tests for indepdendent variables
#

#get yearly data, change in variables
ls70 = landscape[landscape$Year == 1970,]%>% arrange(PLOT)
ls00 = landscape[landscape$Year == 2000,]%>% arrange(PLOT)
lsDiff = ls00[,-c(1:2)] - ls70[,-c(1:2)]; lsDiff = bind_cols(PLOT = plotData$PLOT, lsDiff) 
#plot change in variables
lsDifflong = lsDiff %>% gather('Core250m':'House2000m', key='var', value='vals')
lsPlot = ggplot(lsDifflong, aes(var,vals))
pdf("dLandscape.pdf", width = 8, height = 10)
lsPlot+geom_boxplot() + geom_jitter(width = 0.2) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

lsLong = landscape %>% gather('Core250m':'House2000m', key='var', value='vals')

# t-test for independent variables
tTestdf = data.frame(matrix(nrow = 16, ncol = 6));names(tTestdf) = c("var","stat","Pval","Est","CIlow","CIhigh")
i = 3
for(i in 3:ncol(landscape)){
  tTestdf[i-2,1] = colnames(ls70[,i])
  my_data <- data.frame( group = rep(c("ls70", "ls00"), each = nrow(ls70)),var = rbind(ls70[,i],  ls00[,i]))
  ggplot(my_data, aes(x=my_data[,2], color=my_data[,1])) +geom_histogram(fill="white", alpha=0.5, position="identity", bins = 5)
  tt = t.test(my_data[,2] ~ my_data[,1], data = my_data, paired = TRUE, alternative = "two.sided")
  tTestdf[i-2,2] = tt$statistic; tTestdf[i-2,3] = tt$p.value; tTestdf[i-2,4] = tt$estimate; tTestdf[i-2,5] = tt$conf.int[1]; tTestdf[i-2,6] = tt$conf.int[2]
}

#correlation of variables
pdf(file = "CorrPlots.pdf", width = 8, height = 10)
corrplot(cor(ls70[,-c(1:2)]),type = "upper", method = "number", number.cex=0.7)
corrplot(cor(ls00[,-c(1:2)]),type = "upper", method = "number", number.cex=0.7)
dev.off()