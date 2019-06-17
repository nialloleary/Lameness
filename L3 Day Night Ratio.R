#This is script 3 of the scripts for the lameness studies. It produces that data required for a section of Paper 1, table 2 - ratio of day to night total summaries of behaviour.

#The script is the same as script 2 up until about line 140 
#Start-----
{ 
{ 
  home<- "C:/Users/olearyn2/OneDrive - Lincoln University/RW_Acceleration_and_Behavior" 
# locoation of Lameness files on your computer - data available from Nialloleary@gmail.com

library(dplyr); library(data.table);library(tibble);library("Hmisc")


#Meta Table ----
#This table contains the meta data for locomotion scoring event required to run the script. This facilitate same script being applied to each data set. Only these variables should change between locmotion scoring events. The Selected row corresponds to the locomotion scoring event (lse). In the script, then the item from that row is called. 
  
MVars<-c( #Meta variables "Feature path",
      "Folder",  'SerialStart','SerialEnd',"Date","LocoName", "Loco_Index","ExclCow","Excl2","Excl3","ExclCosws4","ExclCows5","ExclCows6","ExclCows7",'JoinBy', "LSE")
  
  #Jersey trial - First scoring (a) 
  ja<-c( './Jerseys','35','44', '02',  "Loco0106",2,40,40,40,40,40,40,40, 'UNITID')

  jb<-c('./Jerseys','35','44','13',   "Loco1506",3,40,40,40,40,40,40,40, 'UNITID')

# Dairygold 2017(Black & white herd)     
BW17<-c('./DGHF2017', '20','29', '17',  "LocoScore150617",4,
    40,40,40,40,40,40,40, 'UNITID') 
  
#DairyGold 2018 (Black & white herd)
BW18a<-c( './DGHF2018','20','29', '10', "Loco080818",2,
           11, # data doesn't contain walking, SN00017FFD
           13, #SN00018D41
           13, #SN0001932C # also faulty at commercial farm
           15,#SN00018DD5 #4 cows Also didn't work at commercial farm
           40,40,40, 'Ped'  )
  
BW18b<-c('./DGHF2018','20','29','13', "Loco130818", 4,
           11, # SN00017FFD
           13, #SN00018D41
           13, #SN0001932C
           15,#SN00018DD5 
           40,40,40, 'Ped'   )
  
  #Commerical farm 
Farma <-c( './Commercial_Farm','20','29','17', "Loco160818",2, 
           1, #SN00018E33 no loco score, not attached
           40, # SN0001932C
           40, #SN000192D9
           40,40,40,
           40, 'Ped')# out by a day SN00018D79
               
  Farmb <-c('./Commercial_Farm','20','29', '20',"Loco200818",4,  
            1, #SN00018E33 no loco score, not attached
            40, # SN0001932C
            40, #SN000192D9
            40,40,40,
            40, 'Ped')# out by a day SN00018D79
               
#LSE selection (Locomotion scoring event)
Meta<-rbind.data.frame(ja, jb,BW17, BW18a,BW18b,Farma, Farmb)
Meta$LSE<-c("1.aJerseys", "1.bJerseys","2.BW17", "3.aBW18a","3.bBW18b","4.aFarma", "4.bFarmb")
colnames(Meta)<-MVars
  
#Initialise lists to store results from each cohort
#All needed?
CorList<-vector(mode ="list" ,(nrow(Meta)+3))
numList<-vector(mode ="list" ,(nrow(Meta)+3))
pList<-vector(mode ="list" ,(nrow(Meta)+3))
SumDataList<-vector(mode ="list" ,(nrow(Meta)+3))
MRatioList<-vector(mode ="list" ,(nrow(Meta)+3))
VRatioList<-vector(mode ="list" ,(nrow(Meta)+3))
#Loop start----

    for (lse in 1:nrow(Meta)) { # Load selected locomotion scoring events
    print(c("Locomotion Scoring Event",lse))
    setwd(home) 
    setwd(as.character(Meta$Folder[[lse]]))
    setwd('./1Hourly')
    inde<-dir() # Index of file names 
    inde2<-substr(inde,start = paste(Meta$SerialStart[[lse]]),stop= paste(Meta$SerialEnd[[lse]])) # Pedometer Serial numbers
    Results3<-cbind.data.frame(inde,inde2)
    colnames(Results3)<-c("inde","UNITID")
#Excluded Records----
# If you want to exclude 12 &13 you exclude 12 twice (13 goes to 12th position). if 12 and 14, 12 and 13 (14 goes to 13).
#names(Meta)      
for (m in 7:13){ 
      Exclude<- as.numeric(paste0(Meta[lse,m]))
      Results3<-Results3[-Exclude,]}

#Locomotion Scores and Reference table----
    setwd("../")
    Ref1<-read.csv(file = 'PedRef.csv' ,sep = ",",header = T)
    Score<-read.csv(file =  'Score.csv'  ,sep = ",",header = T)# 
    NUM <-as.numeric(paste0(Meta$Loco_Index[[lse]])) # Column with relevant loco score for this 
    Score$loco2<-Score[,NUM] 
    Ref<- left_join(Score,Ref1,paste0(Meta$JoinBy[[lse]]))
    Results2<-left_join(Results3,Ref,"UNITID")#
    Results2$inc<-is.na(Results2$loco2) 
    Results<- Results2 %>% filter(inc== F) # don't load
    Results<-Results[,-(ncol(Results))] #remove inc
    Results$loco<-Results$loco2
    Results<-Results[,c(1,2,ncol(Results))] 

  {    ## 1 hour data-----
      setwd('./1Hourly')
      
      #load first & Initialise
RWconvert <- fread(input = paste(Results[1,1]),sep2 = ";", header=T)
      #Append the rest
  for (i in 2:nrow(Results)) { # loads all the day records
feat <- fread(input = paste(Results[i,1]),sep2 = ";",  header=T)
RWconvert<-rbind.data.frame(RWconvert, feat,fill=TRUE)
}

# Pull out characters for date
RWconvert$Date<-substr(x = RWconvert$WATCHSTART,start = 1,stop = 2)
RWconvert <- RWconvert %>% filter(Date==paste(Meta$Date[[lse]])) #Chosen day

RWconvert$Hour<-as.numeric(substr(x = RWconvert$WATCHSTART,start = 12,stop = 13))

#RWconvert[RWconvert==0]<-NA
#Between milking----
DayTime <- c(10:15)
Day <- RWconvert %>% filter(Hour %in% DayTime)


NightTime <- c(0:5,19:23)
Night <- RWconvert %>% filter(Hour %in% NightTime)


DayResM<- Day %>% group_by(UNITID) %>% summarise_all(mean)
DayResM<-DayResM[,c(1,4:8,12:20)]
DayResV<- Day %>% group_by(UNITID) %>% summarise_all(var)
DayResV<-DayResV[,c(1,4:8,12:20)]

NightResM<- Night %>% group_by(UNITID) %>% summarise_all(mean)
NightResM<-NightResM[,c(1,4:8,12:20)]

NightResV<- Night %>% group_by(UNITID) %>% summarise_all(var)
NightResV<-NightResV[,c(1,4:8,12:20)]

#Diverge from script 2 here ----
MRatio<- DayResM[,2:ncol(DayResM)]/NightResM[,2:ncol(DayResM)]
MRatio<-cbind.data.frame(DayResM[,1],MRatio)
MRatio<- left_join(Results, MRatio, "UNITID")
MRatio<-MRatio[,c(-1,-2)]
MRatioList[lse]<-MRatio

VRatio<- DayResV[,2:ncol(DayResV)]/NightResV[,2:ncol(DayResV)]
VRatio<-cbind.data.frame(DayResV[,1],VRatio)
VRatio<- left_join(Results, VRatio, "UNITID")
VRatio<-VRatio[,c(-1,-2)]
VRatioList[lse]<-VRatio

#Variable Select----

#### Correlation within trial ----
# rcorr creates a list of 3 with 1 - Correlation matrix r, 2 n and 3 p values

#Variance or Mean ----
#CorMat <- rcorr(as.matrix(MRatio),type = 'spearman')
CorMat <- rcorr(as.matrix(VRatio),type = 'spearman')

#r
DFCor<-rownames_to_column(as.data.frame(CorMat[1]))
CorList[[lse]]<-as.data.frame(DFCor[,1:2])

#n - values
DFCor<-rownames_to_column(as.data.frame(CorMat[2]))
numList[[lse]]<-as.data.frame(DFCor[,1:2])

#p-values
DFCor<-rownames_to_column(as.data.frame(CorMat[3]))
pList[[lse]]<-as.data.frame(DFCor[,1:2])

print("LSE")
print(lse)
  }
    }

#Results begin ----
#Combine key columns from the lists created above into summary table
CorListA<-as.data.frame(CorList[1])
for (i in 2:7){
CorListA<-left_join(CorListA,as.data.frame(CorList[[i]]),"rowname")
#colnames(CorListA)[ncol(CorListA)]<-ncol(CorListA)-1 
}
#P-value summary table ----
pListA<-as.data.frame(pList[1])

for (i in 2:7){pListA<-left_join(pListA,as.data.frame(pList[[i]]),"rowname")

colnames(pListA)[ncol(pListA)]<-ncol(pListA)-1 }

pListA$AveragePVal <- rowMeans(pListA[,2:8])
   
CorListA$AverageAbsoluteCorrelation <- rowMeans(CorListA[,2:8])
CorListA$SqrAverageAbsoluteCorrelation<-sqrt(CorListA$AverageAbsoluteCorrelation*CorListA$AverageAbsoluteCorrelation)
is.num <- sapply(CorListA, is.numeric)

CorListB<-cbind(CorListA[,1],as.data.frame(lapply(CorListA[is.num],round,2)))

#Blank values of negligible size
CorListC<-replace(CorListB,CorListB<0.2 & CorListB > -0.2,NA)

colnames(CorListC)<-c("Variable", "JerseysA","JerseysB", 
"BW17", "BW18A","BW18B","FarmA", "FarmB", "Jersey Change","BW18 Change")

plstSig<-pListA
plstSig[pListA>0.2]<-NA
plstSig$rowname<- pListA$rowname
}
#Write Behaviour correlation Table
setwd('../')
setwd('../')
}
# Outermost

write.csv(x = CorListC,file = "Tablex1Not in Paper.csv")
write.csv(x = plstSig,file = "Table1xPvals.csv")
#P Values - Manually add in stars for the few that are significant.
