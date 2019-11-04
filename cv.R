install.packages("xgboost")
install.packages("pROC")
library(pROC)
library(xgboost)
library(readr)

data_test_trans<-readRDS("testtrans.RDS")
data_train_trans<-readRDS("traintrans.RDS")
train_trans<-data_train_trans[,-1]
y<-data_train_trans[,1]

hr = floor( (data_test_trans[,2] / 3600) %% 24 )
         
weekday = (floor (data_test_trans[,2] / 3600 / 24) %%7)
data_test_trans=cbind(data_test_trans,hr,weekday)

data_test_trans = as.data.frame(data_test_trans)

cvn=nrow(train_trans)/5
idall=1:nrow(train_trans)
cv1_inx=sample(idall,cvn,replace = F)
cv2_inx=sample(idall[-cv1_inx],cvn,replace = F)
cv3_inx=sample(idall[-c(cv1_inx,cv2_inx)],cvn,replace = F)
cv4_inx=sample(idall[-c(cv1_inx,cv2_inx,cv3_inx)],cvn,replace = F)  
cv5_inx=idall[-c(cv1_inx,cv2_inx,cv3_inx,cv4_inx)]

cv1=train_trans[cv1_inx,]
cv2=train_trans[cv2_inx,]
cv3=train_trans[cv3_inx,]
cv4=train_trans[cv4_inx,]
cv5=train_trans[cv5_inx,]

ycv1=y[cv1_inx]
ycv2=y[cv2_inx]
ycv3=y[cv3_inx]
ycv4=y[cv4_inx]
ycv5=y[cv5_inx]

auccv<-NULL
cvm1 <- xgboost(data = as.matrix(rbind(cv2,cv3,cv4,cv5)),objective = "binary:logistic",  eval_metric="auc",max_depth=10, nrounds = 50, label = c(ycv2,ycv3,ycv4,ycv5), eta=0.2, booster='gbtree', colsample_bytree=0.9, gamma=0.2)
yhcv1=predict(cvm1,as.matrix(cv1))
roc(ycv1,yhcv1)

cvm2 <- xgboost(data = as.matrix(rbind(cv1,cv3,cv4,cv5)), objective = "binary:logistic", eval_metric="auc",max_depth=10, nrounds = 50, label = c(ycv1,ycv3,ycv4,ycv5), eta=0.2, booster='gbtree', colsample_bytree=0.9, gamma=0.2)
yhcv2=predict(cvm2,as.matrix(cv2))
roc(ycv2,yhcv2)

cvm3 <- xgboost(data = as.matrix(rbind(cv1,cv2,cv4,cv5)),objective = "binary:logistic",  eval_metric="auc",max_depth=10, nrounds = 50, label = c(ycv1,ycv2,ycv4,ycv5), eta=0.2, booster='gbtree', colsample_bytree=0.9, gamma=0.2)
yhcv3=predict(cvm3,as.matrix(cv3))
roc(ycv3,yhcv3)

cvm4 <- xgboost(data = as.matrix(rbind(cv1,cv2,cv3,cv5)),objective = "binary:logistic",  eval_metric="auc",max_depth=10, nrounds =50, label = c(ycv1,ycv2,ycv3,ycv5), eta=0.2, booster='gbtree', colsample_bytree=0.9, gamma=0.2)
yhcv4=predict(cvm4,as.matrix(cv4))
roc(ycv4,yhcv4)

cvm5 <- xgboost(data = as.matrix(rbind(cv1,cv2,cv3,cv4)), objective = "binary:logistic", eval_metric="auc",max_depth=10, nrounds = 50, label = c(ycv1,ycv2,ycv3,ycv4), eta=0.2, booster='gbtree', colsample_bytree=0.9, gamma=0.2)
yhcv5=predict(cvm5,as.matrix(cv5))
roc(ycv5,yhcv5)

yyhcv4=predict(cvm4,as.matrix(data_test_trans))

##############################################################################


#sub<-read.csv("sample_submission.csv")
sub$isFraud<-yyhcv4
write.csv(sub,"transvarxgboots1.csv",row.names = F)


################################################################
xg<-read.csv("0.9255transvarxgboots1.csv")
bes<-read.csv("0.9477bestsubmission.csv")
ble=read.csv("0.9516blendsub.csv")
gme=read.csv("0.9525stack_gmean.csv")
eas=read.csv("0.9526easy_blend4.csv")
su=read.csv("0.9522submission.csv")
# gme=read.csv("0.9525stack_gmean.csv")


w=c(0.9255,0.9477,0.9516,0.9527,0.9526,0.9522)
allsum=sum(w)

allble=w[1]/allsum*xg$isFraud+w[2]/allsum*bes$isFraud+w[3]/allsum*ble$isFraud
+w[4]/allsum*gme$isFraud+w[5]/allsum*eas$isFraud+w[6]/allsum*su$isFraud

sub<-read.csv("sample_submission.csv")
sub$isFraud=allble
write.csv(sub,"ble2.csv",row.names = F)

