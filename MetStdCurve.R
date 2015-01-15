###########################################
# Rscript to analyze FACS data to calculate fmole of methionine needed to make new cell
# Robin Green
# Shou Lab, Basic Sciences, Fred Hutchinson Cancer Research Center
# Version 1.1 
# 2014-11-26

###################


###########################
#load data files and move to working directory
{
  require(ggplot2)
  #require(xtable)
  dir<-'~/Desktop/Break_Analysis/FACS/Hour48/'
  setwd(dir)
}

options(scipen=0)
############################
# read file and remove mean/SD columns (unneeded information)
r<-read.csv(file='Results.csv',header=T)
r<-r[-nrow(r),]
r<-r[-nrow(r),]

############################
#name columns and get row/column ID's
colnames(r)<-c('Sample','BeadCount','CellDustCount','CellCount')
# get cell name (A01,B01...)
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
  s<-r$names[i]
  row<-substring(s,1,1)
  col<-as.numeric(substring(s,2,4))
  rows<-rbind(rows,row)
  cols<-rbind(cols,col)
}
r<-cbind(r,rows,cols)


###############
# define parameters for calculations
BeadsPeruL<-2.67e3
BeaduL<-10
#CelluL<-90
CelluL<-150

###########################
# calculate cell densities of each population
CellPerBead<-r$CellCount/r$BeadCount
r<-cbind(r,CellPerBead)
CellPermL<-r$CellPerBead*BeadsPeruL*BeaduL*1000/CelluL
r<-cbind(r,CellPermL)

#########################
#incorporate met concentration data

MetConc<-vector(mode='list',length=10)
MetConc[[1]]<-0
MetConc[[2]]<-1
MetConc[[3]]<-1.25
MetConc[[4]]<-1.5
MetConc[[5]]<-2
MetConc[[6]]<-3
MetConc[[7]]<-4
MetConc[[8]]<-5
MetConc[[9]]<-7.5
MetConc[[10]]<-10

Conc<-c()

for(i in 1:nrow(r))
{
  col<-r$cols[i]
  Conc<-append(Conc,MetConc[[col]],after=length(Conc))
  #print(Conc)
  
  
}

r<-cbind(r,Conc)

#################
#plot conc data
mytheme =   list(
  geom_line(),
  geom_point(shape=1),
  facet_wrap(~rows),
  scale_y_log10(),
  annotation_logticks(sides="l"),
  xlab('uM Met'),
  ylab('Cells Per mL')
)    




png('All_FACS.png')
a<-ggplot(data=r,aes(x=Conc,y=CellPermL,col=rows))+mytheme
print(a)

dev.off()

r<-r[which(r$Conc<6),]
png('5uM_FACS.png')
a<-ggplot(data=r,aes(x=Conc,y=CellPermL,col=rows))+mytheme
print(a)

dev.off()







TotalCells<-r$CellPermL*.150
r<-cbind(r,TotalCells)

umol<-r$Conc*(150/1e6)
r<-cbind(r,umol)

fmole<-(r$umol/1e6)*1e15
r<-cbind(r,fmole)



##################
#calculate slopes and fmole/newCell  (assume linear scale)

r$TotalCells<-log(r$TotalCells)
A<-r[which(r$rows=='A'),]
B<-r[which(r$rows=='B'),]
C<-r[which(r$rows=='C'),]
D<-r[which(r$rows=='D'),]
E<-r[which(r$rows=='E'),]
F<-r[which(r$rows=='F'),]
G<-r[which(r$rows=='G'),]
H<-r[which(r$rows=='H'),]





A_model<-lm(A$TotalCells~A$fmole)
B_model<-lm(B$TotalCells~B$fmole)
C_model<-lm(C$TotalCells~C$fmole)
D_model<-lm(D$TotalCells~D$fmole)



mytheme =   list(
  geom_line(),
  geom_point(shape=1),
  facet_wrap(~rows),
  #scale_y_log10(),
  #annotation_logticks(sides="l"),
  xlab('uM Met'),
  ylab('Cells Per mL')
)    



r<-r[which(r$Conc<6),]
png('5uM_FACS_linear.png')
a<-ggplot(data=r,aes(x=Conc,y=CellPermL,col=rows))+mytheme
print(a)
dev.off()


r<-r[which(r$Conc<4),]
png('3uM_FACS_linear.png')
a<-ggplot(data=r,aes(x=Conc,y=CellPermL,col=rows))+mytheme
print(a)
dev.off()


save.image('2014-12-27.RData')
