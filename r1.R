compare_fraud=function(trans){fraud=trans[which(trans$isFraud==1),]
nofraud=trans[which(trans$isFraud==0),]
mis_fraud=apply(fraud,2,FUN=function(x){sum(is.na(x)/length(x))})
mis_nofraud=apply(nofraud,2,FUN=function(x){sum(is.na(x)/length(x))})
return(mis_fraud-mis_nofraud)}
