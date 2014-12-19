###########################
#load data files and move to working directory
{
#install.packages('ggplots')
require(ggplot2)
dir<-'X:/fast/shou_w/shougroup/lab_users/Robin/Notebook/Hamilton Competetion/2014-12-11_CoSMOCheatCoopBenchmarkTest/'
setwd(dir)
}
###########################
#load RData file where each time point R object is stored (may need to load more than one)
load('2014-12-18.RData')

#############################
#aggregate all data into master object to be used for plotting/analysis
{
 All_Data<-rbind(Hour0,Hour6,Hour17,Hour25)
 All_Data<-as.data.frame(All_Data)
}

#############################
# theme for ggplot to standardize color scheme and how to plot data
{
  mytheme =   list(
  geom_line(),
  geom_point(shape=1,size=3.5),
  #xlab("time in hours"),
  #ylab("Met-/Met+"),
  scale_colour_manual(values = c("red","blue", "green","orange","black",'tomato','navyblue', "purple",'darkgreen','darkred','darkblue','yellowgreen'))
)	
}

##############################
#collect dilutions(cumulative over experiment) and generation for each timepoint per sample (based on OD)
{
f<-read.csv('Dilutions_Gen.csv',header=T)
All_Data<-cbind(All_Data,f)
}



#############################
# plot population density vs time and ratios vs generation (mCherry vs BFP OR mOrange vs BFP)
{
png(file='2014-12-18_CoopCheatRatiovsGen.png')
a<-ggplot(data=All_Data,aes(x=Gen,y=log(DSRedPerCFP),col=names))+mytheme+facet_wrap(~names)+xlab("Gen")+mytheme+ylab('ln Coop/Cheat')
print(a)
dev.off()



png(file='2014-12-18_DSRedVsGen_Wrapped.png')
a<-ggplot(data=All_Data,aes(x=Gen,y=log10((DSRedPermL)*(Cumulative.Dil)),col=names))+xlab("Gen")+mytheme+ylab('log10 DSRed Density')+facet_wrap(~names)+ylim(4,12)
print(a)
dev.off()

png(file='2014-12-18_CFPVsGen_Wrapped.png')
a<-ggplot(data=All_Data,aes(x=Gen,y=log10((CFPPermL)*(Cumulative.Dil)),col=names))+xlab("Gen")+mytheme+ylab('log10 CFP Density')+facet_wrap(~names)+ylim(4,12)
print(a)
dev.off()


}


save.image('2014-12-18.RData')