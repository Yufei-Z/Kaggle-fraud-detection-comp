
#data input
setwd('C:/Users/DELL/Desktop/STAT4011/Term/ieee-fraud-detection')
library(cattonum)
library(pROC)
library(readr)
library(moments)
library(dplyr)
library(rpart)
library(plyr)
library(tidyverse)
library(R6)
library(lightgbm)
library(MLmetrics)
library(lubridate)
library(ggthemes)
library(rattle)
library(ggmosaic)
library(gridExtra)
library(repr)
library(data.table)
library(fastDummies)
library(tictoc)
library(MLmetrics)
options(warn=-1)
options(scipen = 99)


train_iden<- read_csv("train_identity.csv")
train_trans <- read_csv("train_transaction.csv")
test_iden <- read_csv("test_identity.csv")
test_trans <- read_csv("test_transaction.csv")

# %% [code]
y <- train_trans$isFraud 
train_trans$isFraud <- NULL
train <- train_trans %>% left_join(train_iden) %>% mutate(set = "Train")
test <- test_trans %>% left_join(test_iden) %>% mutate(set = "Test")
base <- train %>% bind_rows(test)
# using k-fold validation,k=4 (20%)
set.seed(4011)
tr_idx_cv1 <- which(train$TransactionDT < quantile(train$TransactionDT, 0.8))
tr_idx_cv2 <- sample(c(1:nrow(train)),size =0.8*nrow(train),replace = F)
set.seed(40112)
tr_idx_cv3 <- sample(c(1:nrow(train)),size =0.8*nrow(train),replace = F)
set.seed(40113)
tr_idx_cv4 <- sample(c(1:nrow(train)),size =0.8*nrow(train),replace = F)
rm(train_iden,train_trans,test_iden,test_trans, train, test) ; invisible(gc())

#Auxiliary variables
base$date <- ymd_hms("2017-12-01 00:00:00")
base$date <- base$date + seconds(base$TransactionDT-86400)
base$date<-base$date-days(base$D1)
base$fec <- as.Date(base$date)
base$idn <- 1:nrow(base)
base$month <- month(base$date)
base$year <- year(base$date)
base$day <- day(base$date)

#first transaction day (same card)
base$D1n<-base$date-days(base$D1)
base$D1n<-as.Date(base$D1n)

#first transaction day (same account)
base$D4n<-base$date-days(base$D4)
base$D4n<-as.Date(base$D4n)

#latest domestic transaction day
base$D10n<-base$date-days(base$D10)
base$D10n<-as.Date(base$D10n)

#combine same card & same account, card=my uid
base$card <- with(base, paste0(card1, card2,card3,addr1, D1n,P_emaildomain))

base$decimal = nchar(base$TransactionAmt - floor(base$TransactionAmt))

drop_col <- c('V300','V309','V111','C3','V124','V106','V125','V315','V134','V102','V123','V316','V113',
              'V136','V305','V110','V299','V289','V286','V318','V103','V304','V116','V29','V284','V293',
              'V137','V295','V301','V104','V311','V115','V109','V119','V321','V114','V133','V122','V319',
              'V105','V112','V118','V117','V121','V108','V135','V320','V303','V297','V120')

base[,drop_col]<-NULL

# FE part1: # Latest browser
fe_part1 <- base[, "id_31"]
fe_part1$latest_browser = 0

new_browsers <- c("samsung browser 7.0", "opera 53.0", "mobile safari 10.0", "google search application 49.0",
                  "firefox 60.0", "edge 17.0", "chrome 69.0", "chrome 67.0", "chrome 63.0", "chrome 63.0", 
                  "chrome 64.0", "chrome 64.0 for android", "chrome 64.0 for ios", "chrome 65.0", "chrome 65.0 for android",
                  "chrome 65.0 for ios", "chrome 66.0", "chrome 66.0 for android", "chrome 66.0 for ios")

fe_part1[fe_part1$id_31 %in% new_browsers,] <- 1

paste0(nrow(fe_part1[fe_part1$latest_browser == 1 ,])*100/nrow(fe_part1), " % total rows with latest browsers")
paste0(nrow(fe_part1[fe_part1$latest_browser == 0 ,])*100/nrow(fe_part1), " % total rows with old browsers")

fe_part1 <- fe_part1[, -c(1)]


#FE part2:(aggregation) mean and sd transaction amount to card 
fe_part2 <- base[, c("card","TransactionAmt","D4n","D10n","dist1","C1","C2","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","id_02","M1","M2","M4","M5","M6","M7","M8","M9")]

fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_Trans= mean(TransactionAmt), sd_card_Trans = sd(TransactionAmt)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, max_card_Trans= max(TransactionAmt), min_card__Trans = min(TransactionAmt)))

fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_dist1= mean(dist1), sd_card_dist1 = sd(dist1)))

fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_D10n_Trans= mean(D10n), sd_D10n_Trans = sd(D10n)))

fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_D4n_Trans= mean(D4n), sd_D4n_Trans = sd(D4n)))


#aggregation for c1:14
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c1= mean(C1), sd_card_C1 = sd(C1)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c2= mean(C2), sd_card_C2 = sd(C2)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c4= mean(C4), sd_card_C4 = sd(C4)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c5= mean(C5), sd_card_C5 = sd(C5)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c6= mean(C6), sd_card_C6 = sd(C6)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c7= mean(C7), sd_card_C7 = sd(C7)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c8= mean(C8), sd_card_C8 = sd(C8)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c9= mean(C9), sd_card_C9 = sd(C9)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c10= mean(C10), sd_card_C10 = sd(C10)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c11= mean(C11), sd_card_C11 = sd(C11)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c12= mean(C12), sd_card_C12 = sd(C12)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c13= mean(C13), sd_card_C13 = sd(C13)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_c14= mean(C14), sd_card_C14 = sd(C14)))

#aggregation for M1:9
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_M1= mean(M1), sd_card_M1 = sd(M1)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_M2= mean(M2), sd_card_M2 = sd(M2)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_M4= mean(M4), sd_card_M4 = sd(M4)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_M5= mean(M5), sd_card_M5 = sd(M5)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_M6= mean(M6), sd_card_M6 = sd(M6)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_M7= mean(M7), sd_card_M7 = sd(M7)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_M8= mean(M8), sd_card_M8 = sd(M8)))
fe_part2 <- fe_part2 %>% left_join(plyr::ddply(fe_part2, ~card, summarise, mean_card_M9= mean(M9), sd_card_M9 = sd(M9)))

fe_part2<-fe_part2[,-c(1:27)]

# FE part3: email binning for purchaser and Recipiant

#proton mail is sketch
fe_part3 <- base[, c("P_emaildomain", "R_emaildomain")]

#email bin function
bin_email <- function(df, grouped, P_colname, R_colname){
  
  typeof(df)
  df$P_placeholder <- 0
  df$R_placeholder <- 0
  
  names(df)[names(df) == "P_placeholder"] <- P_colname
  names(df)[names(df) == "R_placeholder"] <- R_colname
  
  df[df$P_emaildomain %in% grouped, P_colname] <- 1
  df[df$R_emaildomain %in% grouped, R_colname] <- 1
  
  print(paste0(nrow(df[df[, P_colname] == 1,])*100/nrow(df), " % total transactions are ", P_colname, " for Purchaser"))
  print(paste0(nrow(df[df[, R_colname] == 1,])*100/nrow(df), " % total transactions are ", R_colname, " for Recipiant"))
  
  return(df)
}

#is Yahoo
a<- c("yahoo.fr", "yahoo.de", "yahoo.es", "yahoo.co.uk", "yahoo.com", "yahoo.com.mx", "ymail.com", "rocketmail.com", "frontiernet.net")

fe_part3 <- bin_email(fe_part3,a, "P_isyahoo", "R_isyahoo")

#is Microsoft
b<- c("hotmail.com", "live.com.mx", "live.com", "msn.com", "hotmail.es", "outlook.es", "hotmail.fr", "hotmail.de", "hotmail.co.uk")
fe_part3 <- bin_email(fe_part3,b, "P_ismfst", "R_ismfst")

#is apple icloud / mac / me -> apple
c<- c("icloud.com", "mac.com", "me.com")
fe_part3 <- bin_email(fe_part3,c, "P_ismac", "R_ismac")

#is att
d <- c("prodigy.net.mx", "att.net", "sbxglobal.net")
fe_part3 <- bin_email(fe_part3,d, "P_isatt", "R_isatt")

#iscenturylink
e <- c("centurylink.net", "embarqmail.com", "q.com")
fe_part3 <- bin_email(fe_part3,e, "P_iscenturylink", "R_iscenturylink")

#isaol
f <- c("aim.com", "aol.com")
fe_part3 <- bin_email(fe_part3,f, "P_isaol", "R_isaol")

#isspectrum
g <- c("twc.com", "charter.com")
fe_part3 <- bin_email(fe_part3,g, "P_isspectrum", "R_isspectrum")

#isproton
h <- c("protonmail.com")
fe_part3 <- bin_email(fe_part3,h, "P_isproton", "R_isproton")

#iscomcast
i <- c("comcast.net")
fe_part3 <- bin_email(fe_part3,i, "P_iscomcast", "R_iscomcast")

#isgoogle
j <- c("gmail.com")
fe_part3 <- bin_email(fe_part3,j, "P_isgoogle", "R_isgoogle")

#isanonynous
k <- c("anonymous.com")
fe_part3 <- bin_email(fe_part3,k, "P_isanon", "R_isanon")

#isNA
l <- NA
fe_part3 <- bin_email(fe_part3,l, "P_isNA", "R_isNA")

rm(a,b,c,d,e,f,g,h,i, j, k, l) 

fe_part3 <- fe_part3[, -c(1,2)]



#FE part4: count encoding of base features
char_features <- base[,colnames(base) %in% c("ProductCD","card1","card2","card3","card4","card5","card6","addr1","addr2","P_emaildomain","R_emaildomain","M4","DeviceType","DeviceInfo","id_12","id_13","id_14","id_15","id_16","id_17","id_18","id_19","id_20","id_21","id_22","id_23","id_24","id_25","id_26","id_27","id_28","id_29","id_30","id_31","id_32","id_33","id_34","card")]

fe_part4<-catto_freq(char_features)
colnames(fe_part4)<-paste(colnames(fe_part4),"__count_encoding",sep="")


fe_part4 <- fe_part4[,-1]
rm(char_features) ; invisible(gc())
cat("fe_part4 ncol :" , ncol(fe_part4) ,"\n" )




base <- data.frame(base,fe_part1,fe_part2,fe_part3,fe_part4)

rm(fe_part1, fe_part3, fe_part4); invisible(gc())




base <- base %>% group_by(card) %>%mutate(TransactionAmt_to_mean_card = TransactionAmt/mean_card_Trans,TransactionAmt_to_sd_card = TransactionAmt/sd_card_Trans,TransactionAmt_subs_card = TransactionAmt - mean_card_Trans)
base <- base %>% group_by(card) %>%mutate(dist1_to_mean_card = dist1/mean_card_dist1,dist1_to_sd_card = dist1/sd_card_dist1,dist1_subs_card = dist1- mean_card_dist1)

base<- base%>% mutate(TransactionAmt = log(1 + TransactionAmt))

base<-base[!duplicated(as.list(base))]

see<-base[1:10,]


base$new3 = base$TransactionAmt * base$C1
base$new5 = base$TransactionAmt * base$C13
base$new7 = base$TransactionAmt * base$C14

drop_char<-c("ProductCD","card1","card2","card3","card4","card5","card6","addr1","addr2","P_emaildomain","R_emaildomain","M4","DeviceType","DeviceInfo","id_12","id_13","id_14","id_15","id_16","id_17","id_18","id_19","id_20","id_21","id_22","id_23","id_24","id_25","id_26","id_27","id_28","id_29","id_30","id_31","id_32","id_33","id_34","card")

base[,drop_char]<-NULL
base$TransactionDT<-NULL
base$idn<-NULL
base$TransactionID<-NULL
base$set<-NULL

base <- setDT(base)
base [, dec := gsub("0*\\.", "", round(TransactionAmt - floor(TransactionAmt), 4))]


train <- base[1:590540,]
test <- base[-c(1:590540),]

y_train <- y

train$X0<-NULL
test$X0<-NULL

#############################################################################################################


rm(base)

d0 <- lgb.Dataset(data.matrix(train[tr_idx_cv1,]), label = y[tr_idx_cv1] )
dval <- lgb.Dataset(data.matrix(train[-tr_idx_cv1,]), label = y[-tr_idx_cv1] ) 

lgb_param <- list(boosting_type = 'gbdt',
                  objective = "binary" ,
                  metric = "AUC",
                  boost_from_average = "false",
                  tree_learner = 'serial',
                  learning_rate = 0.0068,
                  num_leaves = 197,
                  feature_fraction = 0.3,          
                  bagging_freq = 1,
                  bagging_fraction = 0.7,
                  min_data_in_leaf = 100,
                  bagging_seed = 11,
                  lambda_l1=0,
                  lambda_l2=0,
                  verbose = -1)

valids <- list(valid = dval)
lgb <- lgb.train(params = lgb_param,  data = d0, nrounds = 2000, eval_freq = 200, valids = valids, early_stopping_rounds = 400, verbose = 1)

oof_pred <- predict(lgb, data.matrix(train[-tr_idx_cv1,]))
iter <- lgb$best_iter


# full data
d0 <- lgb.Dataset(data.matrix(train), label = y )
lgb <- lgb.train(params = lgb_param, data = d0, nrounds = iter * 1.05, verbose = -1)
pred <- predict(lgb, data.matrix(test))
sub <- data.frame(read_csv("sample_submission.csv"))
sub[,2] <- pred
write.csv(sub,"uid.csv",row.names = F)
