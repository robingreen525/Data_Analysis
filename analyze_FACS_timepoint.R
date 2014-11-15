###########################################
# Rscript to analyze FACS data collected during competetion experiments
# Robin Green
# Shou Lab, Basic Sciences, Fred Hutchinson Cancer Research Center
# Version 1.0 
# 2014-11-11

###################
#set save file parameters and working directory
dir<-'X:/fast/shou_w/shougroup/lab_users/Robin/Notebook/CoSMO Auxotrophs/Competition/Supplemented Media/2014-10-6_AncestralMetMinusSuppMedia/'
save_file<-'Hour10.RData'
output_csv<-'Hour10_Processed.csv'



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

rr<-loadWorkbook(filename='Hour10/Hour10_results.xls')
r<-readWorksheet(rr,sheet='Sheet0',header=T)

#######################################
#Set up reference 'dictionary' to match well positions with strain pairs
strain_pairs<-vector(mode='list',length=16)
#Hournames(strain_pairs)<-




#####################################
# set up table by renaming colums and removing unneeded information
colnames(r)<-c('Sample','Total','BeadCount','CellDustCount','CellCount','LiveCell','DeadCell','BFP','mCherry','mOrange')
r<-r[-nrow(r),]
r<-r[-nrow(r),] # removes mean and SD row entries

# get cell name 
names<-c()
for(i in 1:nrow(r))
{
  s<-r[i,1]
  s<-substr(s,start=15,stop=17)
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
BeadsPeruL<-6e3
BeaduL<-10
#CelluL<-90
CelluL<-120

################
#read table of dilutions to get dilution factor for each sample
dd<-loadWorkbook(filename='Hour10/Hour10_Dilutions.xlsx')
d<-readWorksheet(dd,sheet='Sheet1',header=T)
dilution_factor<-d$uL.Cells/d$uL.Total

r<-cbind(r,dilution_factor)

###########################
# calculate cell densities of each population
CellPerBead<-r$CellCount/r$BeadCount
r<-cbind(r,CellPerBead)
CellPermL<-r$CellPerBead*BeadsPeruL*BeaduL*1000/CelluL
r<-cbind(r,CellPermL)
LivePermL<-((r$LiveCell/r$CellCount)*r$CellPermL)*r$dilution_factor
r<-cbind(r,LivePermL)
BFPPermL<-((r$BFP/r$LiveCell)*r$CellPermL)*r$dilution_factor
r<-cbind(r,BFPPermL)
mCherryPermL<-((r$mCherry/r$LiveCell)*r$CellPermL)*dilution_factor
r<-cbind(r,mCherryPermL)
mOrangePermL<-((r$mOrange/r$LiveCell)*r$CellPermL)*dilution_factor
r<-cbind(r,mOrangePermL)
mCherryPerBFP<-round((r$mCherryPermL/r$BFPPermL),digits=2)
r<-cbind(r,mCherryPerBFP)
mOrangePerBFP<-round((r$mOrangePermL/r$BFPPermL),digits=2)
r<-cbind(r,mOrangePerBFP)
PercentTotalEvents<-round(((r$mCherry+r$mOrange+r$BFP)/r$LiveCell),digits=2)*100
r<-cbind(r,PercentTotalEvents)

write.csv(r,output_csv)

mytheme =   list(
  #geom_line(),
  geom_point(shape=1),
  scale_y_log10(),
  annotation_logticks(sides="l",size=0.2)
)		


Hour<-rep(6,nrow(r))
Hour10<-cbind(r,Hour)
save.image(save_file)



