
glmCount = glmer(counts ~ (1|guild)+ Edge250m+(0+Edge250m|guild)+House250m+(0+House250m|guild) + (1|PLOT), data = gCountLong, family = "poisson")
summary(glmCount)
anova(glmCount)
ranef(glmCount)

require(sjPlot)
sjp.lmer(glmCount)

z1 = glmer(counts ~ (1|guild)+ Edge250m+House250m+(0+House250m|guild) + (1|PLOT), data = gCountLong, family = "poisson")
z2 = glmer(counts ~ (1|guild)+ Edge250m+(0+Edge250m|guild)+House250m+ (1|PLOT), data = gCountLong, family = "poisson")
names(glmCount)
attributes(glmCount)$deviance
LL <- c(deviance(glmCount), deviance(z1), deviance(z2))

mlm.pvals <- c(1-pchisq(LL[2] - LL[1],1), 1-pchisq(LL[3] - LL[1],1))

names(mlm.pvals)=c("Elevation","Herb","LITU","Ca","P")
mlm.pvals

# compute residual effect of random effects environmental variables
z.r <- glmer(counts ~ (1|guild)+ Edge250m+House250m + (1|PLOT), data = gCountLong, family = "poisson")
attributes(z$eta)
z@eta
MLM.fitted <- array(glmCount@resp$eta - z.r@resp$eta,c(nsite,nspp))
MLM.fitted
model.matrix(glmCount)
# Xb 
fix <- getME(glmCount,'X') %*% fixef(glmCount)
# Zu
ran <- t(as.matrix(getME(glmCount,'Zt'))) %*% unlist(ranef(glmCount))
# Xb + Zu
fixran <- fix + ran
fitted(glmCount)
glmCount@x%*%fixef(glmCount)
fixran
rownames(MLM.fitted)=unique(gCountLong$PLOT)
colnames(MLM.fitted)=unique(gCountLong$guild)
MLM.fitted
# standardize over spp
MLM.fitted.standard <- MLM.fitted
for(j in 1:nspp) MLM.fitted.standard[,j] <- (MLM.fitted[,j]-mean(MLM.fitted[,j]))/sd(MLM.fitted[,j])

ss <- cor(MLM.fitted.standard)
U <- svd(ss)
mlm.fit <- MLM.fitted.standard %*% U$v
mlm.fit <- mlm.fit[,1:2]
mlm.fit
fixef(z)
fitted(z)
# environmental variables (only those with random effects)
envir.vars <- cbind(gCountLong$Edge250m, gCountLong$House250m)
mlm.envir <- NULL
for(j in 1:nx)
  mlm.envir <- cbind(mlm.envir, envir.vars[,j]*mlm.fit[,1],envir.vars[,j]*mlm.fit[,2])

envir.points <- t(array(colMeans(mlm.envir),c(2,dim(mlm.envir)[2]/2)))

# plot mlm
par(mfcol=c(1,1))
plot(-mlm.fit,xlab="PC1",ylab="PC2",type="n")
text(-mlm.fit,label=1:nsite,cex=.5)

arrow.coordMLM <- cbind(array(0,dim(envir.points)),-envir.points)

arrows(arrow.coordMLM[,1],arrow.coordMLM[,2],arrow.coordMLM[,3],arrow.coordMLM[,4], col="black", length=0.1)

text(1.3*-envir.points,label=c("Elevation", "Herb", "LITU", "Ca", "P"),cex=.7)
