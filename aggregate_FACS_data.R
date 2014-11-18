install.packages('ggplots')
require(ggplot2)
dir<-'~/Desktop/comp_data/'
setwd(dir)
load('2014-11-12.RData')

#Hour10<-Hour10_proc
i<-1
Hour10_avg<-c()
while(i<nrow(Hour10))
{
  row1<-Hour10[i,]
  row2<-Hour10[i+1,]
  row3<-Hour10[i+2,]
  
  drop<-c('Sample','rows','cols')
  all<-rbind(row1,row2,row3)
  all<-all[,!names(all) %in% drop]
  

  
  mCherryPermL<-mean(all$mCherryPermL)
  mCherryPermL_StdDev<-sd(all$mCherryPermL)
  mCherryPermL_CV<-mCherryPermL_StdDev/mCherryPermL
  BFPPermL<-mean(all$BFPPermL)
  BFPPermL_StdDev<-sd(all$BFPPermL)
  BFPPermL_CV<-BFPPermL_StdDev/BFPPermL
  mOrangePermL<-mean(all$mOrangePermL)
  mOrangePermL_StdDev<-sd(all$mOrangePermL)
  mOrangePermL_CV<-mOrangePermL_StdDev/mOrangePermL
  
  Hour10_avg<-rbind(Hour10_avg,c(mCherryPermL,mCherryPermL_StdDev,mCherryPermL_CV*100,BFPPermL,BFPPermL_StdDev,BFPPermL_CV*100,mOrangePermL,mOrangePermL_StdDev,mOrangePermL_CV*100))
  

  
  
  
  i<-i+3
  #print(i)
  
  
  
  
}

colnames(Hour10_avg)<-c('mCherryPermL_AVG','mCherryPermL_SD','mCherryPermL_CV','BFPPermL_AVG','BFPPermL_SD','BFPPermL_CV','mOrangePermL_AVG','mOrangePermL_SD','mOrangePermL_CV')
rownames(Hour10_avg)<-c('A','B','C','D','E','F','G','H','I','J','L','M','N','O')

All_Data<-rbind(Hour0_avg,Hour2_avg,Hour4_avg,Hour6_avg,Hour8_avg,Hour10_avg)
All_Data<-as.data.frame(All_Data)
mCherryPerBFP<-(All_Data$mCherryPermL_AVG/All_Data$BFPPermL_AVG)
