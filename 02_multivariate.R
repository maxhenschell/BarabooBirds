#source('01_MossmanAnalysis.R')
lapply(packages.list, require, character.only=T)

lsMRPP = mrpp(landscape[,-1], grouping = 'Year')

dist70 = vegdist(ls70[,-c(1:2)]);dist00 = vegdist(ls00[,-c(1:2)])
mantel(dist70,dist00)

ord.dat = select(birdRA, .data$ACFL:.data$YTVI)
mm.Norm <- wisconsin(ord.dat)
adonisForm <- as.formula("mm.Norm~Core250m + Edge250m + House250m + Core1000m +  Edge1000m +  House1000m")
d.adonis <- adonis2(mm.Norm~1, strata = c(rep(1970,18),rep(2000,18)), permutations = 10000)
d.adonis


# sp.bin <- sp.RA; sp.bin[]<- +(sp.RA > 0); sp.bin[,1] = sp.RA[,1]
# ord.dat <- sp.bin[, -1]
# NMSdat = rbind(ls.1978.r, ls.2001.r)
# colForm = as.formula(paste("mm.NMS~",paste(colnames(ls.1978.r[-1]), collapse =  "+")))
# colForm = as.formula('mm.NMS ~ Core_1000_4200 + Edge_250_4200 + House_250m + House_1000m ')
# mm.NMS <- metaMDS(ord.dat, trymax = 500, distance = 'bray', k = 3)
# mm.NMS
# mm.env <- envfit(colForm, choices = c(1,2), data = rbind(ls.1978.r, ls.2001.r), perm = 10000)
# mm.env
# 
# dat.dist <- vegdist(ord.dat)
# env.dist <- vegdist(rbind(ls.1978.r[,-1], ls.2001.r[,-1]))
# mm.Mantel <- mantel(env.dist, dat.dist, method="pearson", permutations=999)
# 
# source("GG_NMS.R")
# survey <- as.data.frame(c(rep('1970',18), rep('2000',18))) # two groups of sites
# colnames(survey) <- 'year'
# 
# ##NMDS points
# mm.NMS.dat<-data.frame(NMSdat[,'Edge_1000_4200'],
#                        "YEAR" = factor(survey$year),
#                        "NMDS1" = mm.NMS$points[ ,1],
#                        "NMDS2" = mm.NMS$points[ ,2])
# #there are other ways of doing this. But this is the way I do it for ease of plotting
# 
# plots2D <- GG_NMS(mm.NMS, mm.NMS.dat, ord.dat, mm.env = mm.env)
# plots2D[[1]]
# ggsave(filename = "BinaryNMS.pdf", height = 10, width = 10, plot = plots2D[[1]], useDingbats = FALSE)
# 
# plots2D[[2]]
# ggsave(filename = "BinaryNMS_Species.pdf", height = 10, width = 10, plot = plots2D[[2]], useDingbats = FALSE)
# 
# 
# colForm.3d = colForm#as.formula(paste("mm.NMS.3d~",paste(colnames(ls.1978.r[-1]), collapse =  "+")))
# mm.NMS.3d <- metaMDS(ord.dat, trymax = 500, distance = 'bray', k = 3)
# mm.NMS.3d
# 
# mm.env12 <- envfit(colForm.3d, choices = c(1,2), data = rbind(ls.1978.r, ls.2001.r), perm = 10000)
# mm.env12
# 
# mm.env13 <- envfit(colForm.3d, choices = c(1,3), data = rbind(ls.1978.r, ls.2001.r), perm = 10000)
# mm.env13
# 
# mm.env23 <- envfit(colForm.3d, choices = c(2,3), data = rbind(ls.1978.r, ls.2001.r), perm = 10000)
# mm.env23
# 
# 
# 
# mm.Norm <- wisconsin(ord.dat)
# mm.Dist <- vegdist(mm.Norm,method="bray",binary=FALSE)#calculate dissimilarity matrix
# (d.anosim <- anosim(mm.Dist, as.vector(survey[,1]), permutations = 10000))
# summary(d.anosim)
#   plot(d.anosim)
# 
# adonisForm <- as.formula("mm.Norm~Core_250_4200 + Edge_250_4200 + House_250m + Core_1000_4200 +  Edge_1000_4200 +  House_1000m")
# d.adonis <- adonis(adonisForm, data = rbind(ls.1978.r, ls.2001.r), strata = c(rep(1970,18),rep(2000,18)), permutations = 10000)
# d.adonis
# 
# 
# 
#   #SIMPER
# attach(ord.dat)
# ord.env <- rbind(ls.1978.r, ls.2001.r)
# ord.env$year = c(rep('1970',18), rep('2000',18))
# attach(ord.env)
# (sim <- with(ord.env, simper(ord.dat, year, permutations = 10000)))
# sim.sum <- summary(sim, ordered = TRUE, digits = max(3,getOption("digits") - 3))
# write.csv(sim.sum$`1970_2000`, "SIMPER.csv")
# 
# # (core.surf <- ordisurf(mm.NMS, dat.ls$Core_2000_4200))
# # ordi.grid <- core.surf$grid #extracts the ordisurf object
# # str(ordi.grid) #it's a list though - cannot be plotted as is
# # ordi.core <- expand.grid(x = ordi.grid$x, y = ordi.grid$y) #get x and ys
# # ordi.core$z <- as.vector(ordi.grid$z) #unravel the matrix for the z scores
# # ordi.core.na <- data.frame(na.omit(ordi.core)) #gets rid of the nas
# # ordi.core.na #looks ready for plotting!
# 
# 
# 
# #
# #------------------------------------#
# # Plot the ordination in ggplot2
# # script from https://oliviarata.wordpress.com/2014/04/17/ordinations-in-ggplot2/
# #
# 
# 
# ##NMDS points
# mm.NMS.dat<-data.frame(ls.diff[,mm.env13$vectors$pvals < 0.1],
#                        "YEAR" = factor(survey$year),
#                        "NMDS1" = mm.NMS$points[ ,1],
#                        "NMDS2" = mm.NMS$points[ ,2])
#                        #there are other ways of doing this. But this is the way I do it for ease of plotting
# 
# plots12 <- GG_NMS(mm.NMS, mm.NMS.dat, ord.dat, mm.env = mm.env12)
# ggsave(plot = plots12[[1]], filename = paste0('SiteOrd_ggplot_12.pdf'),height = 10, width = 10) 
# ggsave(plot = plots12[[2]], filename = 'SpeciesOrd_ggplot_12.pdf',height = 10, width = 10) 
# 
# mm.NMS.dat<-data.frame(  ls.diff[,mm.env13$vectors$pvals < 0.1],
#                        "YEAR" = factor(survey$year),
#                        "NMDS1" = mm.NMS$points[ ,1],
#                        "NMDS2" = mm.NMS$points[ ,3])
# plots13 <- GG_NMS(mm.NMS, mm.NMS.dat, ord.dat, mm.env = mm.env13)
# #plots13[[1]]
# ggsave(plot = plots13[[1]], filename = paste0('SiteOrd_ggplot_13.pdf'),height = 10, width = 10) 
# ggsave(plot = plots13[[2]], filename = 'SpeciesOrd_ggplot_13.pdf',height = 10, width = 10) 
# 
# 
# mm.NMS.dat<-data.frame(  ls.diff[,mm.env13$vectors$pvals < 0.1],
#                          "YEAR" = factor(survey$year),
#                          "NMDS1" = mm.NMS$points[ ,2],
#                          "NMDS2" = mm.NMS$points[ ,3])
# plots23 <- GG_NMS(mm.NMS, mm.NMS.dat, ord.dat, mm.env = mm.env23)
# ggsave(plot = plots23[[1]], filename = paste0('SiteOrd_ggplot_23.pdf'),height = 10, width = 10) 
# ggsave(plot = plots23[[2]], filename = 'SpeciesOrd_ggplot_23.pdf',height = 10, width = 10) 
# 
# # 
# # #Ordisurf plots
# # 
# # # set limits for plot area
# # m.in <- min(min(mm.NMS.dat$NMDS2), min(mm.NMS.dat$NMDS1))
# # m.ax <- max(max(mm.NMS.dat$NMDS2), max(mm.NMS.dat$NMDS1))
# # lims <- max(round(abs(m.in), 1), round(abs(m.ax), 1))+0.05
# # ggplot(mm.NMS.dat, aes(x = NMDS1, y = NMDS2))+
# #   stat_contour(data = ordi.core.na, aes(x = x, y = y, z = z, colour = rev(..level..)))+ #can change the binwidth depending on how many contours you want
# #   geom_point(aes(shape = YEAR), size = 3) + #plots the NMDS points, with shape by topo type
# #   #theme_bw() + #for aesthetics
# #   scale_shape_manual(values = c(1,8,19,5)) + theme_classic() + #sets the name of the legend for shape, and says which symbols we want (equivalent to the pch command in plot)
# #   labs(colour = 'Core area')+ #another way to set the labels, in this case, for the colour legend
# #   scale_colour_gradient(high = 'darkgreen', low = 'darkolivegreen1')+ #here we set the high and low of the colour scale.  Can delete to go back to the standard blue, or specify others
# #   theme(legend.key = element_blank(),  #removes the box around each legend item
# #         legend.position = 'bottom', #legend at the bottom
# #         legend.direction = 'horizontal',
# #         legend.box = 'horizontal',
# #         legend.box.just = 'centre') +
# #   coord_cartesian(xlim = c(-lims,lims), ylim = c(-lims,lims)) 
# # 
# # 
# # 
# # 
# # m.1 <- aggregate(NMDS1 ~ cols,spps, FUN = mean)
# # m.2 <- aggregate(NMDS2 ~ cols,spps, FUN = mean)
# # sd.1 <- aggregate(NMDS1 ~ cols,spps, FUN = sd)
# # sd.2 <- aggregate(NMDS2 ~ cols,spps, FUN = sd)
# # 
# # sp.av <- data.frame(NMDS1 = m.1[,2],NMDS2 = m.2[,2], 
# #                     sd1 = sd.1[,2], sd2 = sd.2[,2], 
# #                     se1 = (sd.1[,2]/sqrt(nrow(spps))), se2 = sd.2[,2]/sqrt(nrow(spps)), 
# #                     d1.95 = qt(0.975,df=nrow(spps)-1)*sp.av$sd1/sqrt(nrow(spps)),
# #                     d2.95 = qt(0.975,df=nrow(spps)-1)*sp.av$sd2/sqrt(nrow(spps)),
# #                     col = c( 'other', 'Interior'))
# # row.names(sp.av) <- c('Nonforest', 'Interior')
# # lims = 0.75
# # spp.NMDS.gg2 <- ggplot(data = sp.av, aes(y = NMDS2, x = NMDS1)) + #sets up the plot. brackets around the entire thing to make it draw automatically
# #   #geom_path(data = df_ell.abund.YEAR, aes(x = NMDS1, y = NMDS2, group = YEAR, alpha=YEAR, fill = YEAR))+ #this is the ellipse, seperate ones by Site. If you didn't change the 'alpha' (the shade) then you need to keep the 'group 
# #   #scale_alpha_manual(guide = FALSE,values=c(0.3, 0.5, 0.7))+ #sets the shade for the ellipse
# #   #annotate('text',x = NMDS.mean$NMDS1,y = NMDS.mean$NMDS2,label=NMDS.mean$group) + #labels for the centroids - I haven't used this since we have a legend. but you could also dithc the legend, but plot will get v messy
# #   geom_segment(data = env.scores.abund,
# #                aes(x = 0, xend = mult*NMDS1, y = 0, yend = mult*NMDS2),
# #                arrow = arrow(length = unit(0.25, 'cm')), colour = 'black', size = 1) + #arrows for envfit.  doubled the length for similarity to the plot() function. NB check ?envfit regarding arrow length if not familiar with lengths
# #   geom_text(data = env.scores.abund, #labels the environmental variable arrows * 'mult' as for the arrows
# #             aes(x = mult*NMDS1, y = mult*NMDS2, label=c('Core forest', 'Homestead density'#paste0(env.variables, ' (',
# #                                                             # format(round(env.scores.abund$NMDS1,2), nsmall = 2),', ',
# #                                                             # format(round(env.scores.abund$NMDS2,2), nsmall =2),')'
# #             )),
# #             size = 5,
# #             hjust = -0.05)+
# #   geom_point(data=spps, alpha = .6, shape = 4, color = spps$cols) +
# #     geom_point(data=sp.av, alpha = .6, shape = 4) + 
# #   geom_errorbarh(aes(xmax = sp.av$NMDS1 + sp.av$d1.95, xmin = sp.av$NMDS1 - sp.av$d1.95, height = 0.05, color = col)) + #geom_text(aes(label=row.names(spps2)),hjust=0, vjust=0)+ #these are the species points, made lighter and a specific shape
# #   geom_errorbar(aes(ymax = sp.av$NMDS2 + sp.av$d2.95, ymin = sp.av$NMDS2 - sp.av$d2.95, width = 0.05, color = col))+
# #   scale_shape_manual(values = c(1,8,19,5)) + theme_bw() +
# #   theme(text=element_text(size=20),
# #         legend.title=element_blank(), legend.position = 'bottom') +
# #   geom_hline(aes(yintercept = 0), color = 'grey', linetype = 'dashed') +
# #   geom_vline(aes(xintercept = 0), color = 'grey', linetype = 'dashed') +
# #   coord_cartesian(xlim = c(-lims,lims), ylim = c(-lims,lims))
# # spp.NMDS.gg2
# # 
# # ggsave(plot = spp.NMDS.gg2, filename = 'SpeciesOrd_ggplot_simple.pdf',height = 10, width = 10) 
# 
# 
# 
# 
