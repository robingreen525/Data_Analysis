###########################################
# Rscript to analyze FACS data collected during competetion experiments
# Robin Green
# Shou Lab, Basic Sciences, Fred Hutchinson Cancer Research Center
# Version 1.1 
# 2014-11-11

###################'
#set save file parameters and working directory
dir<-'X:/fast/shou_w/shougroup/lab_users/Robin/Notebook/Hamilton Competetion/2014-12-11_CoSMOCheatCoopBenchmarkTest/'
save_file<-'Hour17.RData'
output_csv<-'Hour17_Processed.csv'

options( scipen = -200000 )
###########################################
#install nessecary packages

#install.packages('XLConnect')
#install.packages('ggplots')
require(XLConnect)
require(ggplot2)

#############################################
#move to dir where all data is stored
setwd(dir)


####################################
# read input excel file from FloJo Analysis of timepoint

#rr<-loadWorkbook(filename='Hour17/Hour17_results.xls')
#r<-readWorksheet(rr,sheet='Sheet0',header=T)
r<-read.csv('Hour17/Results.csv',header=T)

#######################################
#Set up reference 'dictionary' to match well positions with strain pairs
strain_pairs<-vector(mode='list',length=12)
#Hournames(strain_pairs)<-




#####################################
# set up table by renaming colums and removing unneeded information
colnames(r)<-c('Sample','BeadCount','CellDustCount','CellCount','DeadCell','LiveCell','CFP','DSRed','Dust')
r<-r[-nrow(r),]
r<-r[-nrow(r),] # removes mean and SD row entries


# get cell name (A01,B01...)
names<-c()
for(i in 1:nrow(r))
{
  s<-r[i,1]
  s<-substr(s,start=18,stop=19)
  #print(s)
  names<-rbind(names,s)
}
r<-cbind(r,names)

# get row and col information for each sample
rows<-c()
cols<-c()
for(i in 1:nrow(r))
{
  s<-r[i,11]
  #print(s)
  row<-substring(s,1,1)
  #print(row)
  col<-as.numeric(substring(s,2,4))
  #print(col)
  rows<-rbind(rows,row)
  cols<-rbind(cols,col)
  
}
r<-cbind(r,rows,cols)


###############################
## identify any potentially probalmatic samples (those with counts <10000)
bad_beads<-which(r$BeadCount<10000)
bad_cells<-which(r$CellCount<10000)

print(bad_beads)
print(bad_cells)

###############
# define parameters for calculations
BeadsPeruL<-2.67e3
BeaduL<-10
#CelluL<-90
CelluL<-150

################
#sample dilution factor is 1, as plate goes straight to FACS from robot

dilution_factor<-rep(1,times=nrow(r))

r<-cbind(r,dilution_factor)

###########################
# calculate cell densities of each population
CellPerBead<-r$CellCount/r$BeadCount
r<-cbind(r,CellPerBead)
CellPermL<-r$CellPerBead*BeadsPeruL*BeaduL*1000/CelluL
r<-cbind(r,CellPermL)
LivePermL<-((r$LiveCell/r$CellCount)*r$CellPermL)*r$dilution_factor
r<-cbind(r,LivePermL)
CFPPermL<-((r$CFP/r$LiveCell)*r$CellPermL)*r$dilution_factor
r<-cbind(r,CFPPermL)
DSRedPermL<-((r$DSRed/r$LiveCell)*r$CellPermL)*dilution_factor
r<-cbind(r,DSRedPermL)
DSRedPerCFP<-round((r$DSRedPermL/r$CFPPermL),digits=2)
r<-cbind(r,DSRedPerCFP)
PercentTotalEvents<-round(((r$DSRed+r$CFP)/r$LiveCell),digits=2)*100
r<-cbind(r,PercentTotalEvents)

write.csv(r,output_csv)

mytheme =   list(
  #geom_line(),
  geom_point(shape=1),
  scale_y_log10(),
  annotation_logticks(sides="l",size=0.2)
)		



#create data object that will be used to stich together with other timepoints and save work
Hour<-rep(17,nrow(r))
Hour17<-cbind(r,Hour)
save.image(save_file)



