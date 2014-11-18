###load data files and move to working directory
install.packages('ggplots')
require(ggplot2)
dir<-'~/Desktop/comp_data/'
setwd(dir)
#load('2014-11-12.RData')

{
Hour0<-Hour0_proc
i<-1
Hour0_avg<-c()
while(i<nrow(Hour0))
{
  row1<-Hour0[i,]
  row2<-Hour0[i+1,]
  row3<-Hour0[i+2,]
  
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
  
  Hour0_avg<-rbind(Hour0_avg,c(mCherryPermL,mCherryPermL_StdDev,mCherryPermL_CV*100,BFPPermL,BFPPermL_StdDev,BFPPermL_CV*100,mOrangePermL,mOrangePermL_StdDev,mOrangePermL_CV*100))

  
  i<-i+3
  
  
}

colnames(Hour0_avg)<-c('mCherryPermL_AVG','mCherryPermL_SD','mCherryPermL_CV','BFPPermL_AVG','BFPPermL_SD','BFPPermL_CV','mOrangePermL_AVG','mOrangePermL_SD','mOrangePermL_CV')
rownames(Hour0_avg)<-c('A','B','C','D','E','F','G','H','I','J','L','M','N','O')
Hour<-rep(0,nrow(Hour0_avg))
Hour0_avg<-cbind(Hour,Hour0_avg)

i<-1
Hour2_avg<-c()
while(i<nrow(Hour2))
{
  row1<-Hour2[i,]
  row2<-Hour2[i+1,]
  row3<-Hour2[i+2,]
  
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
  Hour2_avg<-rbind(Hour2_avg,c(mCherryPermL,mCherryPermL_StdDev,mCherryPermL_CV*100,BFPPermL,BFPPermL_StdDev,BFPPermL_CV*100,mOrangePermL,mOrangePermL_StdDev,mOrangePermL_CV*100))
  i<-i+3
}
colnames(Hour2_avg)<-c('mCherryPermL_AVG','mCherryPermL_SD','mCherryPermL_CV','BFPPermL_AVG','BFPPermL_SD','BFPPermL_CV','mOrangePermL_AVG','mOrangePermL_SD','mOrangePermL_CV')
rownames(Hour2_avg)<-c('A','B','C','D','E','F','G','H','I','J','L','M','N','O')
Hour<-rep(2,nrow(Hour2_avg))
Hour2_avg<-cbind(Hour,Hour2_avg)

i<-1
Hour4_avg<-c()
while(i<nrow(Hour4))
{
  row1<-Hour4[i,]
  row2<-Hour4[i+1,]
  row3<-Hour4[i+2,]
  
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
  Hour4_avg<-rbind(Hour4_avg,c(mCherryPermL,mCherryPermL_StdDev,mCherryPermL_CV*100,BFPPermL,BFPPermL_StdDev,BFPPermL_CV*100,mOrangePermL,mOrangePermL_StdDev,mOrangePermL_CV*100))
  i<-i+3
}
colnames(Hour4_avg)<-c('mCherryPermL_AVG','mCherryPermL_SD','mCherryPermL_CV','BFPPermL_AVG','BFPPermL_SD','BFPPermL_CV','mOrangePermL_AVG','mOrangePermL_SD','mOrangePermL_CV')
rownames(Hour4_avg)<-c('A','B','C','D','E','F','G','H','I','J','L','M','N','O')
Hour<-rep(4,nrow(Hour4_avg))
Hour4_avg<-cbind(Hour,Hour4_avg)

i<-1
Hour6_avg<-c()
while(i<nrow(Hour6))
{
  row1<-Hour6[i,]
  row2<-Hour6[i+1,]
  row3<-Hour6[i+2,]
  
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
  Hour6_avg<-rbind(Hour6_avg,c(mCherryPermL,mCherryPermL_StdDev,mCherryPermL_CV*100,BFPPermL,BFPPermL_StdDev,BFPPermL_CV*100,mOrangePermL,mOrangePermL_StdDev,mOrangePermL_CV*100))
  i<-i+3
}
colnames(Hour6_avg)<-c('mCherryPermL_AVG','mCherryPermL_SD','mCherryPermL_CV','BFPPermL_AVG','BFPPermL_SD','BFPPermL_CV','mOrangePermL_AVG','mOrangePermL_SD','mOrangePermL_CV')
rownames(Hour6_avg)<-c('A','B','C','D','E','F','G','H','I','J','L','M','N','O')
Hour<-rep(6,nrow(Hour6_avg))
Hour6_avg<-cbind(Hour,Hour6_avg)

i<-1
Hour8_avg<-c()
while(i<nrow(Hour8))
{
  row1<-Hour8[i,]
  row2<-Hour8[i+1,]
  row3<-Hour8[i+2,]
  
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
  Hour8_avg<-rbind(Hour8_avg,c(mCherryPermL,mCherryPermL_StdDev,mCherryPermL_CV*100,BFPPermL,BFPPermL_StdDev,BFPPermL_CV*100,mOrangePermL,mOrangePermL_StdDev,mOrangePermL_CV*100))
  i<-i+3
}
colnames(Hour8_avg)<-c('mCherryPermL_AVG','mCherryPermL_SD','mCherryPermL_CV','BFPPermL_AVG','BFPPermL_SD','BFPPermL_CV','mOrangePermL_AVG','mOrangePermL_SD','mOrangePermL_CV')
rownames(Hour8_avg)<-c('A','B','C','D','E','F','G','H','I','J','L','M','N','O')
Hour<-rep(8,nrow(Hour8_avg))
Hour8_avg<-cbind(Hour,Hour8_avg)

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
}
colnames(Hour10_avg)<-c('mCherryPermL_AVG','mCherryPermL_SD','mCherryPermL_CV','BFPPermL_AVG','BFPPermL_SD','BFPPermL_CV','mOrangePermL_AVG','mOrangePermL_SD','mOrangePermL_CV')
rownames(Hour10_avg)<-c('A','B','C','D','E','F','G','H','I','J','L','M','N','O')
Hour<-rep(10,nrow(Hour10_avg))
Hour10_avg<-cbind(Hour,Hour10_avg)

All_Data<-rbind(Hour0_avg,Hour2_avg,Hour4_avg,Hour6_avg,Hour8_avg,Hour10_avg)
All_Data<-as.data.frame(All_Data)
mCherryPerBFP<-(All_Data$mCherryPermL_AVG/All_Data$BFPPermL_AVG)
All_Data<-cbind(All_Data,mCherryPerBFP)
mOrangePerBFP<-(All_Data$mOrangePermL_AVG/All_Data$BFPPermL_AVG)
All_Data<-cbind(All_Data,mOrangePerBFP)
}


mytheme =   list(
  geom_line(),
  geom_point(shape=1,size=3.5),
  xlab("time in hours"),
  ylab("Met-/Met+"),
  scale_colour_manual(values = c("red",'red',"blue",'blue', "green",'green',"orange",'orange',"black",'black', "purple",'purple','darkgreen','darkgreen'))
)	

f<-read.csv('Dilutions_Gen.csv',header=T)
All_Data<-cbind(All_Data,f)

strains<-rep(c('WY1773.1','WY1773.2','WY1774.1','WY1774.2','WY1775.1','WY1775.2','WY1922.1','WY1922.2','WY1924.1','WY1924.2','WY1344.1','WY1344.2','WY1348.1','WY1348.2'),6)

All_Data<-cbind(All_Data,strains)




png(file='2014-11-6_MetAuxoRatiovsGen.png')
a<-ggplot(data=All_Data,aes(x=All_Data$Gen,y=log((All_Data$mCherryPerBFP)),col=((All_Data$strains))))+mytheme+xlab("time in Gen")+ylim(-0.5,0.5)+ylab('ln Met-/Met+')
print(a)
dev.off()

png(file='2014-11-6_ControlRatiovsGen.png')
a<-ggplot(data=All_Data,aes(x=All_Data$Gen,y=log(All_Data$mOrangePerBFP),col=((All_Data$strains))))+mytheme+xlab("time in Gen")+ylab('ln mOrangetoBFP_ControlRatio')+ylim(-0.5,0.5)
print(a)
dev.off()

All_Data_copy<-All_Data

png(file='2014-11-6_mCherryVsGen.png')
a<-ggplot(data=All_Data,aes(x=All_Data$Gen,y=log(All_Data$mCherryPermL_AVG*(All_Data$Cumulative.Dil)),col=((All_Data$strains))))+mytheme+xlab("time in Gen")+ylab('ln mCherry Density')+ylim(12,15)
print(a)
dev.off()

png(file='2014-11-6_BFPVsGen.png')
a<-ggplot(data=All_Data,aes(x=All_Data$Gen,y=log(All_Data$BFPPermL_AVG*(All_Data$Cumulative.Dil)),col=((All_Data$strains))))+mytheme+xlab("time in Gen")+ylab('ln BFP Density')
print(a)
dev.off()

nams<-c('A','B','C','D','E','F','I','J')


png(file='2014-11-6_mCherryVsGen_Wrapped.png')
a<-ggplot(data=All_Data,aes(x=Gen,y=log10((mCherryPermL_AVG)*(Cumulative.Dil)),col=strains))+xlab("time in Gen")+mytheme+ylab('log10 mCherry Density')+facet_wrap(~strains)+geom_point()+geom_line()+ylim(5,8)
print(a)
dev.off()

png(file='2014-11-6_BFPVsGen_Wrapped.png')
a<-ggplot(data=All_Data,aes(x=Gen,y=log10((BFPPermL_AVG)*(Cumulative.Dil)),col=strains))+xlab("time in Gen")+mytheme+ylab('log10 BFP Density')+facet_wrap(~strains)+geom_point()+geom_line()+ylim(5,8)
print(a)
dev.off()

png(file='2014-11-6_mOrangeVsGen_Wrapped.png')
a<-ggplot(data=All_Data,aes(x=Gen,y=log10((mOrangePermL_AVG)*(Cumulative.Dil)),col=strains))+xlab("time in Gen")+mytheme+ylab('log10 mOrange Density')+facet_wrap(~strains)+geom_point()+geom_line()+ylim(5,8)+mytheme
print(a)
dev.off()

fit_advs<-c()
for(i in 1:length(nams))
{
  nam<-nams[i]
  print(nam)
  a<-All_Data[which(All_Data$Sample==nam),]
  a<-a[-1,]
  for(j in 1:(nrow(a)))
  {
    print(j)
    print(a$Gen[j])
    d<-lm(log(a$mCherryPerBFP[c(j,j+1)])~a$Gen[c(j,j+1)])
    print(a$Gen[c(j,j+1)])
    adv<-(d$coefficients[2]/log(2))*100
    fit_advs<-rbind(fit_advs,c(nam,a$Gen[j+1],adv))
    
    
  }
}

colnames(fit_advs)<-c('Sample','Gen','Fit_Adv')

fit_advs<-as.data.frame(fit_advs)

fit_advs$Gen<-floor(as.numeric(as.character(fit_advs$Gen)))
fit_advs$Fit_Adv<-as.numeric(as.character(fit_advs$Fit_Adv))

Gen<-rep(c(2,3,4,5,NA),length(nams))
#Sample<-rep(c('WY1773','WY1773','WY1774','WY1774','WY1775','WY1775','WY1924','WY1924'),5)

fit_advs$Gen<-Gen

strains<-c('WY1773','WY1773','WY1773','WY1773','WY1773','WY1773','WY1773','WY1773','WY1774','WY1774','WY1774','WY1774','WY1774','WY1774','WY1774','WY1774',
           'WY1775','WY1775','WY1775','WY1775','WY1775','WY1775','WY1775','WY1775','WY1922','WY1922','WY1922','WY1922','WY1922','WY1922','WY1922','WY1922',
           'WY1924','WY1924','WY1924','WY1924','WY1924','WY1924','WY1924','WY1924')

fit_advs<-cbind(fit_advs,strains)

png('FitnessVSGenEach.png')
a<-ggplot(data=fit_advs,aes(x=Gen,y=Fit_Adv,col=strains))+geom_point(size=(2))+geom_line(aes(y=0))+facet_wrap(~strains)+ylab('%Fitness Advantage Relative to BFP Ruler')
print(a)


dev.off()

fit_advs_comp<-fit_advs

nams<-c('L','M','N','O')

fit_advs<-c()
for(i in 1:length(nams))
{
  nam<-nams[i]
  print(nam)
  a<-All_Data[which(All_Data$Sample==nam),]
  a<-a[-1,]
  for(j in 1:(nrow(a)))
  {
    print(j)
    print(a$Gen[j])
    d<-lm(log(a$mOrangePerBFP[c(j,j+1)])~a$Gen[c(j,j+1)])
    print(a$Gen[c(j,j+1)])
    adv<-(d$coefficients[2]/log(2))*100
    fit_advs<-rbind(fit_advs,c(nam,a$Gen[j+1],adv))
    
    
  }
}

colnames(fit_advs)<-c('Sample','Gen','Fit_Adv')

fit_advs<-as.data.frame(fit_advs)

fit_advs$Gen<-floor(as.numeric(as.character(fit_advs$Gen)))
fit_advs$Fit_Adv<-as.numeric(as.character(fit_advs$Fit_Adv))

Gen<-rep(c(2,3,4,5,NA),length(nams))

fit_advs$Gen<-Gen

strains<-c('WY1344','WY1344','WY1344','WY1344','WY1344','WY1344','WY1344','WY1344','WY1344','WY1344',
           'WY1348','WY1348','WY1348','WY1348','WY1348','WY1348','WY1348','WY1348','WY1348','WY1348')
fit_advs<-cbind(fit_advs,strains)


png('FitnessVSGenEach_Control.png')
a<-ggplot(data=fit_advs,aes(x=Gen,y=Fit_Adv,col=strains))+geom_point(size=(5))+geom_line(aes(y=0))+facet_wrap(~strains)
print(a)


fit_advs_all<-rbind(fit_advs_comp,fit_advs)

png('FitnessVSGen_ALL.png')
a<-ggplot(data=fit_advs_all,aes(x=Gen,y=Fit_Adv,col=strains))+geom_point(size=(2))+geom_line(aes(y=0))+facet_wrap(~strains)+ylab('%Fitness Advantage Relative to Ruler Strain')
print(a)


dev.off()



drop=c('L','M','N','O')

b<-All_Data[,c('Gen','Sample','strains','mCherryPerBFP')]
b<-b[which(!(b$Sample==drop)),]

c<-All_Data[,c('Gen','Sample','strains','mOrangePerBFP')]
c<-c[which((c$Sample==drop)),]

colnames(b)<-c('Gen','Sample','strains','ratio')
colnames(c)<-c('Gen','Sample','strains','ratio')

all<-c()
#all<-rbind(b,c)
all<-b

#all<-all[-(which(all$Sample=='L'),]
#all<-all[-(which(all$Sample=='L'),]



nums<-c(.7,.8,.85,.9,.925,.95,.975,.99,1,1.01,1.02,1.03,1.05)
nums<-log(nums)
labs<-c('.7','.8','.85','.9','.925','.95','.975','.99','1','1.01','1.02','1.03','1.05')

png(file='2014-11-6_AllRatioVsGen.png')
a<-ggplot(data=all,aes(x=all$Gen,y=log(all$ratio),col=((all$strains))))+mytheme+xlab("time in Gen")+ylab('ln Ratio to Ruler Strain')+
    geom_point(data=c,aes(x=c$Gen,y=log(c$ratio),col=c$strains))+geom_line(data=c,aes(x=c$Gen,y=log(c$ratio),col=c$strains))+scale_y_continuous(limits=c(log(0.7),log(1.1)),breaks=log(c(.7,.8,.85,.9,.925,.95,.975,.99,1,1.01,1.02,1.03,1.05)),labels=labs)#+ylim(log(0.7),log(1.1))
print(a)

dev.off()

save.image('2014-11-16.RData')