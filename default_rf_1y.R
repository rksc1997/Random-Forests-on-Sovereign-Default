library(dplyr)
library(gplots)
library(haven)
library(kableExtra)
library(latexpdf)
library(readxl)
library(tidyr)
library(tikzDevice)
library(zoo)

setwd(".")
d1<- read_dta("./Data/for_rforest_v2.dta") #Cross Country Data of default and economic indicators
require(randomForest)
require(MASS)
set.seed(101)
train=sample(1:nrow(d1),300)
keeps<-c("z_mean_rank", "z_lngdppc_ppp", "z_gdp_c", "z_cpi", "z_debt_to_gdp", "z_gvt_bal", "z_interest_revenue", "z_current_acc_pcgdp", "z_net_fdi", "z_ln_bm", "d1", "z_cr_to_govt_st_own_ent", "z_rta", "z_bnk_zscore",  "z_prop_in_def")
d1[keeps]->icr

na.omit(icr)->icr
train2=sample(1:nrow(icr),300)
rf.icr=randomForest(d1~., data=icr, subset = train2)
View(icr)
rf.icr
oob.err=double(14)
test.err=double(14)
mtry=4
for(mtry in 1:14){
  fit=randomForest(d1~.,data=icr,subset=train2,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,icr[-train2,])
  test.err[mtry]=with(icr[-train2,],mean((d1-pred)^2))
  cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))

require(gbm)
boost.icr=gbm(d1~.,data=icr[train2,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.icr)
summary(boost.icr)->sum_stat_1
write.table(sum_stat_1, file = "sum_stat_1y.csv", quote = FALSE, sep=",", row.names = FALSE)
stargazer(boost.icr)
plot(boost.icr,i="z_lngdppc_ppp")
plot(boost.icr,i="z_mean_rank")
plot(boost.icr,i="z_prop_in_def")
plot(boost.icr,i="pent_up_reform")


n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.icr,newdata=icr[-train2,],n.trees=n.trees)
dim(predmat)
berr=with(icr[-train2,],apply( (predmat-d1)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error")
abline(h=min(test.err),col="red")
