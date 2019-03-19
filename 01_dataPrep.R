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

birdCount = read_csv("bircCount.csv")
landscape = read_csv("landscape.csv")
guilds = read_csv("guildMembers.csv")

birdRA = read_csv()

ls70 = landscape[landscape$Year == 1970,]
ls00 = landscape[landscape$Year == 2000,]
lsDiff = ls00[,-c(1:2)] - ls70[,-c(1:2)]; lsDiff = bind_cols(ls70[1,], lsDiff)



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