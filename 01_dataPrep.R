#
#-----------------------------------#
#Clean the system
#

rm(list=ls())

#
#-----------------------------------#
#Download, install packages
#
packages.list<-c("corrplot",  "BMA", "betapart","vegan", "MuMIn", "lme4", "tidyverse","sjPlot")
required.packages <- packages.list
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(packages.list, require, character.only=T)

#
#-----------------------------------#
#Load bird data
#

birdCount = read_csv("birdCount.csv")
landscape = read_csv("landscape.csv")
guilds = read_csv("guildMembers.csv")
plotData = read.csv("PlotData.csv") %>% arrange(PLOT)

ls70 = landscape[landscape$Year == 1970,]%>% arrange(PLOT)
ls00 = landscape[landscape$Year == 2000,]%>% arrange(PLOT)
lsDiff = ls00[,-c(1:2)] - ls70[,-c(1:2)]; lsDiff = bind_cols(PLOT = plotData$PLOT, lsDiff) 
lsDifflong = lsDiff %>% gather('Core250m':'House2000m', key='var', value='vals')
lsPlot = ggplot(lsDifflong, aes(var,vals))
lsPlot+geom_boxplot(notch = TRUE) + geom_jitter(width = 0.2) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

tTestdf = data.frame(matrix(nrow = 16, ncol = 6));names(tTestdf) = c("var","stat","Pval","Est","CIlow","CIhigh")
for(i in 3:ncol(landscape)){
  tTestdf[i-2,1] = colnames(ls70[,i])
  my_data <- data.frame( 
    group = rep(c("ls70", "ls00"), each = nrow(ls70)),
    var = rbind(ls70[,i],  ls00[,i])
  )
  tt = t.test(my_data[,2] ~ my_data[,1], data = my_data, paired = TRUE, alternative = "two.sided")
  resid(tt)
  tTestdf[i-2,2] = tt$statistic; tTestdf[i-2,3] = tt$p.value; tTestdf[i-2,4] = tt$estimate; tTestdf[i-2,5] = tt$conf.int[1]; tTestdf[i-2,6] = tt$conf.int[2]
}


guildList = data.frame(matrix(nrow = nrow(birdCount),ncol = 7))
for(i in 1:7){
  gg = data.frame(mapply(`*`,birdCount[,-c(1:2)],guilds[i,-1])) %>% mutate(rowsum = rowSums(.))
  guildList[,i] = gg$rowsum
}
colnames(guildList) = guilds$guild
guildCount = bind_cols(landscape[,c(1,2)],guildList,landscape[,-c(1,2)])

gCountLong = guildCount %>% gather('Resident':'Synanthope', key='guild', value='counts')

datCount = bind_cols(birdCount,landscape[,-c(1:2)])
datRA = bind_rows(birdRA,landscape)

nsite <- 18
nx <- 2 #number of fixed effects 
nspp <- length(unique(gCountLong$guild)) #number of species

gCount1970 = filter(gCountLong,Year == 1970)
