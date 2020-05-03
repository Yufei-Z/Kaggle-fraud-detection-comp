# data_trans<-read.csv("train_transaction.csv",header=FALSE)
load("1.RData")
test_trans=read.csv("test_transaction.csv")
saveRDS(test_trans,"test.RDS")
saveRDS(trans,"train.RDS")
standardize<-function(v){
  m<-mean(v)
  sigma<-sd(v)
  z<-(v-m)/sigma
  return(z)
}

miss <- function(x){sum(is.na(x))/length(x)*100}
#first half
mis_rate1=apply(trans[,1:(length(trans)/2)],2,miss)
#second half
mis_rate2=apply(trans[,(length(trans)/2):length(data_trans)],2,miss)
keep1=which(mis_rate1<0.9)
keep2=which(mis_rate2<0.9)

saveRDS(mis_rate,"missrate.RDS")
mis_rate<-readRDS("missrate.RDS")

good<-which(mis_rate<0.5)
# data.na.omit<-na.omit(data)

var_class <- function(x){
  bin<-NULL #is binary
  conti<-NULL# is continuous
  categ<-NULL#is category
  numb<-NULL #all number of observations
  for (i in 1:dim(x)[2]) {
    xx<-na.omit(x[,i])
    numb[i]<-length(unique(xx))
    if(numb[i]<=2) #binary
      bin<-c(bin,i)
    else if (is.numeric(unique(xx))==1) #continuous
      conti<-c(conti,i)
    else #categorical
      categ<-c(categ,i)
  }
  info_list<- list(numb,bin,conti,categ)
  # Give names to the elements in the list.
  names(info_list) <- c("number for all cols", "binary index","continuous index","categorical index")
  return(info_list)
}

coln<-var_class(trans)
saveRDS(coln,"class_col")
coln<-readRDS("class_col")
# data processing--variable selection
# For test of binary and continuous
y<-trans[,2]

library(ltm)
ttest = array()
Xcon<-trans[,coln[[3]]]
ycon_coln<-NULL
for(con in 1:ncol(Xcon)){
  ycon_coln<-which(!is.na(Xcon[,con]))
  n<-length(ycon_coln)
  co = biserial.cor(Xcon[ycon_coln,con], y[ycon_coln])
  ttest[con] = co*sqrt((n-2)/(1-co^2))
}
con_use<-which(abs(ttest) <= 1.96)
newcon=Xcon[,con_use]

# For test of binary and binary, odds ratio
library(questionr)
bintest = array()
Xbin<-trans[,coln[[2]]]
Xbin<-Xbin[,-1]
ybin_coln<-NULL
for(bin in 1:ncol(Xbin)){
  ybin_coln<-which(!is.na(Xbin[,bin]))
  n<-length(ybin_coln)
  mat = colSums(cbind(y[ybin_coln],Xbin[ybin_coln,bin]-1))
  mat = rbind(mat,n-mat)
  bintest[bin] = odds.ratio(mat,level = 0.95)
}

bin_chi<-NULL
for(bin in 1:ncol(Xbin)){
  ybin_coln<-which(!is.na(Xbin[,bin]))
  print(chisq.test(Xbin[ybin_coln,bin],y[ybin_coln]))
}
newbin=Xbin[,c(2,3,4,5,6,7,8,11)]

#test whether the result is valid
# yy<-rbinom(nrow(Xbin),1,0.5)
# mat = colSums(cbind(y,yy))
# n=length(yy)
# mat = rbind(mat,n-mat)
# bintest[bin] = odds.ratio(mat)$p

# For test of binary and categorical
Xcat<-trans[,coln[[4]]]
ycat_coln<-NULL
cattest = array()
for(cat in 1:ncol(Xcat)){
  ycat_coln<-which(!is.na(Xcat[,cat]))
  n<-length(ycat_coln)
  cattest[cat] = cor.test(x=Xcat[ycat_coln,cat], y=y[ycat_coln], method = 'spearman',exact = F)$p.value
}

cat_chi<-NULL
for(cat in 1:ncol(Xcat)){
  ycat_coln<-which(!is.na(Xcat[,cat]))
  print(chisq.test(Xcat[ycat_coln,cat],y[ycat_coln]))
}
newcat=Xcat

newtrans<-cbind(y,newcon,newbin,newcat)
saveRDS(newtrans,"newtrans")
saveRDS(newcon,"newcon")
saveRDS(newbin,"newbin")
saveRDS(newcat,"newcat")

newmisr=apply(newtrans,2,miss)

iden_y<-mergeData[,2]
iden_coln<-var_class(mergeData[,395:length(mergeData)])

#identity data
#continuous
library(ltm)
iden_ttest = array()
iden_Xcon<-mergeData[,iden_coln[[3]]+394]
iden_ycon_coln<-NULL
for(con in 1:ncol(iden_Xcon)){
  iden_ycon_coln<-which(!is.na(iden_Xcon[,con]))
  n<-length(iden_ycon_coln)
  co = biserial.cor(iden_Xcon[iden_ycon_coln,con], iden_y[iden_ycon_coln])
  iden_ttest[con] = co*sqrt((n-2)/(1-co^2))
}
iden_con_use<-which(abs(iden_ttest) <= 1.96)
iden_newcon=iden_Xcon[,iden_con_use]

# For test of binary and binary, odds ratio
library(questionr)
iden_bintest = array()
iden_Xbin<-mergeData[,iden_coln[[2]]+394]
# iden_Xbin<-iden_Xbin[,-1]
iden_ybin_coln<-NULL
for(bin in 1:ncol(iden_Xbin)){
  iden_ybin_coln<-which(!is.na(iden_Xbin[,bin]))
  print(chisq.test(iden_Xbin[iden_ybin_coln,bin],iden_y[iden_ybin_coln]))
}
iden_newbin=iden_Xbin


# For test of binary and categorical
iden_Xcat<-mergeData[,iden_coln[[4]]+394]
iden_ybin_coln<-NULL
for(cat in 1:ncol(iden_Xcat)){
  iden_ycat_coln<-which(!is.na(iden_Xcat[,cat]))
  print(chisq.test(iden_Xcat[iden_ycat_coln,cat],iden_y[iden_ycat_coln]))
}
iden_newcat=iden_Xcat[,-4]

# saveRDS(newtrans,"newtrans")
saveRDS(iden_newcon,"idennewcon")
saveRDS(iden_newbin,"idennewbin")
saveRDS(iden_newcat,"idennewcat")


+++++++++++++++++++++++++++++++++++++++++++
  install.packages("xgboost")
install.packages("caret")
install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

load("1.RData")
productcd<-function(x){
  obs_no<-length(x)
  C<-rep(0,length(x))
  H<-rep(0,length(x))
  R<-rep(0,length(x))
  S<-rep(0,length(x))
  W<-rep(0,length(x))
  C[grep("C",x)]<-1
  H[grep("H",x)]<-1
  R[grep("R",x)]<-1
  S[grep("S",x)]<-1
  W[grep("W",x)]<-1
  cat_product<-cbind(C,H,R,S,W)
  return(cat_product)
}
card4<-function(x){
  obs_no<-length(x)
  american_express<-rep(0,length(x))
  discover<-rep(0,length(x))
  mastercard<-rep(0,length(x))
  visa<-rep(0,length(x))
  american_express[grep("american express",x)]<-1
  discover[grep("discover",x)]<-1
  mastercard[grep("mastercard",x)]<-1
  visa[grep("visa",x)]<-1
  cat_card4<-cbind(american_express,discover,mastercard,visa)
  return(cat_card4)
}
card6<-function(x){
  obs_no<-length(x)
  charge_card<-rep(0,length(x))
  credit<-rep(0,length(x))
  debit<-rep(0,length(x))
  debit_or_credit<-rep(0,length(x))
  card6_NA<-rep(0,length(x))
  charge_card[grep("charge card",x)]<-1
  credit[grep("credit",x)]<-1
  debit[grep("debit",x)]<-1
  debit_or_credit[grep("debit or credit",x)]<-1
  #card6_NA<-
  cat_product<-cbind(charge_card,credit,debit,debit_or_credit)
  return(cat_product)
}
M4<-function(x){
  obs_no<-length(x)
  MM0<-rep(0,length(x))
  MM1<-rep(0,length(x))
  MM2<-rep(0,length(x))
  #M4_NA<-rep(0,length(x))
  MM0[grep("M0",x)]<-1
  MM1[grep("M1",x)]<-1
  MM2[grep("M2",x)]<-1
  cat_M4<-cbind(MM0,MM1,MM2)
  return(cat_M4)
}
#one out one hot encoding
# cat_class<-function(x){
#   obs_no<-length(x)
#   allclass<-na.omit(unique(x))
#   class_no<-length(allclass)
#   cat_var<-matrix(0,nrow = obs_no,ncol = class_no)
#   colnames(cat_var)<-allclass
#   for (i in 1:class_no) { # check every column for every class
#     cat_var[grep(allclass[i],x),i]<-1
#   }
#   return(cat_var)
# }

allcat<-cbind(productcd(Xcat[,1]),card4(Xcat[,2]),card6(Xcat[,3]),M4(Xcat[,6]))
# names(Xcat)
# # product<-cat_class(Xcat[,1])
# # cat_matrix_name<-names(Xcat)
# allcat<-NULL
# for (i in (c(1,2,3,6))) {
#   onecat<-cat_class(Xcat[,i])
#   allcat<-cbind(allcat,onecat)
# }
# #train categorical data after one hot encoding
# saveRDS(allcat,"traincat.RDS")

Xbin<-trans[,coln[[2]]]
Xbin<-Xbin[,-1]
y<-trans[,2]
Xcon<-trans[,coln[[3]]]
Xcat<-trans[,coln[[4]]]
data_train_trans<-cbind(y,Xcon,Xbin,allcat)
saveRDS(data_train_trans,"traintrans.RDS")
data_train_trans<-readRDS("traintrans.RDS")
train_trans<-data_train_trans[,-1]
y<-data_train_trans[,1]
+++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #test data
  test_trans<-readRDS("test.RDS")
test_coln<-var_class(test_trans)
test_Xbin<-test_trans[,test_coln[[2]]]
test_Xcon<-test_trans[,test_coln[[3]]]
test_Xcat<-test_trans[,test_coln[[4]]]
# test_allcat<-NULL
# for (i in (c(1,2,3,6))) {
#   test_onecat<-cat_class(test_Xcat[,i])
#   test_allcat<-cbind(test_allcat,test_onecat)
# }
test_allcat<-cbind(productcd(test_Xcat[,1]),card4(test_Xcat[,2]),card6(test_Xcat[,3]),M4(test_Xcat[,6]))

#train categorical data after one hot encoding

data_test_trans<-cbind(test_Xcon,test_Xbin,test_allcat)
saveRDS(data_test_trans,"testtrans.RDS")
data_test_trans<-readRDS("testtrans.RDS")
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #modelling
  install.packages(ROCR)
install.packages("pROC")
library(ROCR)
library(pROC)
saveRDS(train_trans,"alltraintrans.RDS")
saveRDS(data_test_trans,"alltesttrans.RDS")

y1<-y[(300000:590000)]
train_trans1<-train_trans[(300000:590000),]
mall1 <- xgboost(data = as.matrix(train_trans1), eval_metric="auc",max_depth=20, nrounds =8,  label = y1, eta=0.2, booster='gbtree', colsample_bytree=0.9, gamma=0.2)

yall=predict(mall,as.matrix(train_trans[(300000:400000),]))
yall1=predict(mall1,as.matrix(train_trans[(1:200000),]))
ygd=predict(mgd,as.matrix(train_trans[goodfeatureinx][(500000:600000),]))

mgd <- xgboost(data = as.matrix(train_trans[goodfeatureinx]), eval_metric="auc",max_depth=20, nrounds =15,  label = y, eta=0.2, booster='gbtree', colsample_bytree=0.9, gamma=0.2)

roc(y[(500000:600000)],ygd)
roc(y[(300000:400000)],yall)
roc(y[(1:200000)],yall1)



# y1=predict(m1,as.matrix(train_trans[(400000:430000),]))
# roc(y[(400000:430000)],y1)
yhat=predict(mall,as.matrix(data_test_trans))
write.csv(yhat,"sub")

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #parameters
  best_param=list()
best_seednumber=4011
best_logloss= Inf
best_logloss_index = 0

for (iter in 1:10) {
  param <- list(objective = "binary:logistic",     # ????????????:logistic??????????????????,??????Y???????????????
                eval_metric = c("auc"),                # ????????????:logloss
                max_depth = sample(6:15, 1),               # ???????????????????????????:1??? 6-10 ????????????
                eta = runif(1, .01, .3),                   # eta????????????????????????:1??? 0.01-0.3????????????
                gamma = runif(1, 0.0, 0.2),                # gamma????????????????????????:1??? 0-0.2????????????
                subsample = runif(1, .6, .9),             
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 20                                   # ????????????:50
  cv.nfold = 5                                     # 5???????????????
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=as.matrix(train_trans[1:300000]), label = y, params = param, nthread=6, metrics="auc",
                 nfold=cv.nfold, nrounds=cv.nround, watchlist = list(),
                 verbose = F, early_stop_round=8, maximize=FALSE)
  
  min_auc = min(mdcv$evaluation_log[,test_auc_mean])
  min_auc_index = which.min(mdcv$evaluation_log[,test_auc_mean])
  
  if (min_auc < best_auc) {
    best_auc = min_auc
    best_auc_index = min_auc_index
    best_seednumber = seed.number
    best_param = param
  }
}

(nround = best_auc_index)
set.seed(best_seednumber)
best_seednumber
(best_param)                
# ????????????????????????,??????????????????????????????



#mdcv$evaluation_log

xgb_plot=function(input,output){
  history=input
  train_history=history[,1:8]%>%mutate(id=row.names(history),class="train")
  test_history=history[,9:16]%>%mutate(id=row.names(history),class="test")
  colnames(train_history)=c("logloss.mean","logloss.std","auc.mean","auc.std","rmse.mean","rmse.std","error.mean","error.std","id","class")
  colnames(test_history)=c("logloss.mean","logloss.std","auc.mean","auc.std","rmse.mean","rmse.std","error.mean","error.std","id","class")
  
  his=rbind(train_history,test_history)
  his$id=his$id%>%as.numeric
  his$class=his$class%>%factor
  
  if(output=="auc"){ 
    auc=ggplot(data=his,aes(x=id, y=auc.mean,ymin=auc.mean-auc.std,ymax=auc.mean+auc.std,fill=class),linetype=class)+
      geom_line()+
      geom_ribbon(alpha=0.5)+
      labs(x="nround",y=NULL,title = "XGB Cross Validation AUC")+
      theme(title=element_text(size=15))+
      theme_bw()
    return(auc)
  }
  
  
  if(output=="rmse"){
    rmse=ggplot(data=his,aes(x=id, y=rmse.mean,ymin=rmse.mean-rmse.std,ymax=rmse.mean+rmse.std,fill=class),linetype=class)+
      geom_line()+
      geom_ribbon(alpha=0.5)+
      labs(x="nround",y=NULL,title = "XGB Cross Validation RMSE")+
      theme(title=element_text(size=15))+
      theme_bw()
    return(rmse)
  }
  
  if(output=="error"){
    error=ggplot(data=his,aes(x=id,y=error.mean,ymin=error.mean-error.std,ymax=error.mean+error.std,fill=class),linetype=class)+
      geom_line()+
      geom_ribbon(alpha=0.5)+
      labs(x="nround",y=NULL,title = "XGB Cross Validation ERROR")+
      theme(title=element_text(size=15))+
      theme_bw()
    return(error)
  }
  
}


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  names(data_test_trans)[names(data_test_trans)=="M1"]="M1.1"

#feature importance
importanceRaw<- xgb.importance(feature_names=colnames(train_trans),model = mgd)
xgb.ggplot.importance(importanceRaw)
importanceRaw[1]
ygdhat=predict(mgd,as.matrix(data_test_trans[goodfeatureinx]))
sub$isFraud<-ygdhat
write.csv(sub,"gdvarxgboots.csv",row.names = F)
save(mgd,file = "mgd_xgb.rda")

load("mgd_xgb.rda")
yhat=predict(mall,as.matrix(data_test_trans[goodfeatureinx]))

sub<-read.csv("sample_submission.csv")
sub$isFraud<-yhat
write.csv(sub,"allvarxgboots.csv",row.names = F)
save(mall,file = "mall_xgb.rda")

load("mall_xgb.rda")



