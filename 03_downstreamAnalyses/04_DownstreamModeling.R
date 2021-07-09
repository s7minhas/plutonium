dapth = "/Users/maxgallop/Dropbox/booz_allen_gdmm/03_downstreamAnalyses/"
load(paste0(dapth, "chiData.rda"))


###BaseModels
bl1 = glm(econScores_tradeDepSend_lfm~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)
bl2 = glm(diplomScores_agree_lfm~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)
bl3 = glm(icewsScores_gov_lfm~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)

bt1 = glm(econScores_tradeDepSend~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)
bt2 = glm(diplomScores_agree~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)
bt3 = glm(icewsScores_gov~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)

###BaseModels + Region FEs
r1 = glm(econScores_tradeDepSend_lfm~USf1.l1 + USf2.l1 + USf3.l1 + region, data = chiData)
r2 = glm(diplomScores_agree_lfm~USf1.l1 + USf2.l1 + USf3.l1 + region, data = chiData)
r3 = glm(icewsScores_gov_lfm~USf1.l1 + USf2.l1 + USf3.l1 + region, data = chiData)

###BaseModels + LDV
l1 = glm(econScores_tradeDepSend_lfm~USf1.l1 + USf2.l1 + USf3.l1 + econScores_tradeDepSend_lfm.l1, data = chiData)
l2 = glm(diplomScores_agree_lfm~USf1.l1 + USf2.l1 + USf3.l1 + diplomScores_agree_lfm.l1, data = chiData)
l3 = glm(icewsScores_gov_lfm~USf1.l1 + USf2.l1 + USf3.l1 + icewsScores_gov_lfm.l1, data = chiData)

chiData$econDelta = chiData$econScores_tradeDepSend_lfm - chiData$econScores_tradeDepSend_lfm.l1
chiData$diplomDelta = chiData$diplomScores_agree_lfm - chiData$diplomScores_agree_lfm.l1
chiData$icewsDelta = chiData$icewsScores_gov_lfm - chiData$icewsScores_gov_lfm.l1

###BaseModels Delta
d1 = glm(econDelta~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)
d2 = glm(diplomDelta~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)
d3 = glm(icewsDelta~USf1.l1 + USf2.l1 + USf3.l1 , data = chiData)


###BaseModels Delta
d1 = glm(econDelta~USf1.l1 + USf2.l1 + USf3.l1 + polity.l1, data = chiData)
d2 = glm(diplomDelta~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)
d3 = glm(icewsDelta~USf1.l1 + USf2.l1 + USf3.l1 , data = chiData)


?glmnet
useVars = c("USf1.l1", "USf2.l1", "USf3.l1", "polity.l1",  "GDP.l1", "pop.l1", "beijDist", "washDist", "IdealPointDistance")

use = c(useVars, "econScores_tradeDepSend", "diplomScores_agree", "icewsScores_gov","econScores_tradeDepSend_lfm", "diplomScores_agree_lfm", "icewsScores_gov_lfm")
regionMat = model.matrix(~chiData$region)
regionMat = data.frame(regionMat)
chiData = cbind(chiData, regionMat)

regionVars = names(regionMat)


lassoData = na.omit(chiData[,c(use, regionVars)])
lasso1 = glmnet(lassoData[,c(useVars,regionVars)], y = lassoData$econScores_tradeDepSend, alpha = 1)


###Control Model
c1 = glm(econScores_tradeDepSend_lfm ~ USf1.l1 + USf2.l1 + USf3.l1 + polity.l1+GDP.l1+ pop.l1+beijDist+washDist + as.factor(region), data = chiData)
c2 = glm(diplomScores_agree ~ USf1.l1 + USf2.l1 + USf3.l1 + polity.l1+GDP.l1+ pop.l1+beijDist+washDist + as.factor(region), data = chiData)
c3 = glm(icewsScores_gov ~ USf1.l1 + USf2.l1 + USf3.l1 + polity.l1+GDP.l1+ pop.l1+beijDist+washDist + as.factor(region), data = chiData)

cl1 = glm(econScores_tradeDepSend ~ USf1.l1 + USf2.l1 + USf3.l1  + econScores_tradeDepSend.l1+ polity.l1+GDP.l1+ pop.l1+beijDist+washDist + as.factor(region), data = chiData)
cl2 = glm(diplomScores_agree ~ USf1.l1 + USf2.l1 + USf3.l1 +diplomScores_agree.l1+ polity.l1+GDP.l1+ pop.l1+beijDist+washDist + as.factor(region), data = chiData)
cl3 = glm(icewsScores_gov ~ USf1.l1 + USf2.l1 + USf3.l1 + diplomScores_agree.l1 + polity.l1+GDP.l1+ pop.l1+beijDist+washDist + as.factor(region), data = chiData)


plot_glmnet(lasso1, xvar="dev", label = 20, lwd=2, cex=2)


source('~/Dropbox/lagger.r')
chiData$econScores_tradeDepSend.l1 = lagger(chiData$econScores_tradeDepSend, chiData$cname1, chiData$year, 1)
chiData$econDelta = chiData$econScores_tradeDepSend - chiData$econScores_tradeDepSend.l1
chiData$econDelta_lfm = chiData$econScores_tradeDepSend_lfm - chiData$econScores_tradeDepSend_lfm.l1


###BaseModels, FD
fd1 = glm(econDelta~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)
fd2 = glm(econDelta_lfm~USf1.l1 + USf2.l1 + USf3.l1, data = chiData)

#####BaseModels + Region FEs
r1 = glm(econDelta~USf1.l1 + USf2.l1 + USf3.l1 + region, data = chiData)
r2 = glm(econDelta_lfm~USf1.l1 + USf2.l1 + USf3.l1 + region, data = chiData)

###ControlModels
c1 = glm(econDelta ~ USf1.l1 + USf2.l1 + USf3.l1 + polity.l1+GDP.l1+ pop.l1+beijDist+washDist + IdealPointDistance + as.factor(region), data = chiData)
c2 = glm(econDelta_lfm ~ USf1.l1 + USf2.l1 + USf3.l1 + polity.l1+GDP.l1+ pop.l1+beijDist+washDist + IdealPointDistance + as.factor(region), data = chiData)

useVars = c("USf1.l1", "USf2.l1", "USf3.l1", "polity.l1",  "GDP.l1", "pop.l1", "beijDist", "washDist", "IdealPointDistance")
use = c(useVars, "econDelta_lfm", "econDelta")
regionMat = model.matrix(~chiData$region)
regionMat = data.frame(regionMat)
chiData = cbind(chiData, regionMat)

regionVars = names(regionMat)


lassoData = na.omit(chiData[,c(use, regionVars)])
library(glmnet)
lasso1 = glmnet(lassoData[,c(useVars,regionVars)], y = lassoData$econDelta_lfm, alpha = 1)
lasso2 = glmnet(lassoData[,c(useVars,regionVars)], y = lassoData$econDelta, alpha = 1)
plot_glmnet(lasso1, xvar="dev", label = 20, lwd=2, cex=2)
plot_glmnet(lasso2, xvar="dev", label = 20, lwd=2, cex=2)

library(lme4)

c1 = lmer(econDelta ~ USf1.l1 + USf2.l1 + USf3.l1 + polity.l1+log(GDP.l1)+ log(pop.l1)+scale(beijDist)+scale(washDist) + IdealPointDistance + (1 |region), data = chiData)
c2 = lmer(econDelta_lfm ~ USf1.l1 + USf2.l1 + USf3.l1 + polity.l1+log(GDP.l1)+ log(pop.l1)+scale(beijDist)+scale(washDist) + IdealPointDistance  + (1|region), data = chiData)
ic1 = lmer(econDelta ~ polity.l1+log(GDP.l1)+ log(pop.l1)+scale(beijDist)+scale(washDist) + IdealPointDistance + (1 + USf1.l1 + USf2.l1 + USf3.l1|region), data = chiData)
ic2 = lmer(econDelta_lfm ~ polity.l1+log(GDP.l1)+ log(pop.l1)+scale(beijDist)+scale(washDist) + IdealPointDistance  + (1 + USf1.l1 + USf2.l1 + USf3.l1|region), data = chiData)

save(lassoData, chiData, file = "forMD.rda")
