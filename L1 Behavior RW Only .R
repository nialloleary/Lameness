# To do 
#Finalize Correlation table for 24hr 
# Write it up
#Look at 1hrly variables to be calculated
#Decide same or new script?
#Create correlation table for these variables
# Write up Discussion and Conclusions
# See if summary table is required to pad it out / adds value?

#So in the jersey hourly summaries- stride distance appears reasonable
#start----
h<-1 #r
h<-2 #n
h<-3 #p
{ { 
  options(scipen=99,digits=2)
  home<- "C:/Lameness/LamenessPublicData/RW_Acceleration_and_Behavior" # locoation of Lameness file on your computer - data available from Nialloleary@gmail.com
  library(dplyr); library(data.table);library(tibble);
#library(magrittr);  library(RcppRoll);  library(purrr);library(zoo);


#This Meta table contains the meta data for locomotion scoring event required to run the script. This facilitate same script being applied to each data set. Only these variables should change between locmotion scoring events. The Selected row corresponds to the locomotion scoring event (lse). In the script, then the item from that row is called. 
  
MVars<-c( #Meta variables 
      "Folder",      "WATCHSTART","Feature path","Loco_Index","ExclCow","Excl2","Excl3","ExclCosws4","ExclCows5","ExclCows6","ExclCows7",'JoinBy', "LSE"
  )
  #Meta ----
  #Jersey trial - First scoring (a) 
  ja<-c( './Jerseys', '02.06.2017 00:00',  "Loco0106",2,40,40,40,40,40,40,40, 'UNITID')

  jb<-c('./Jerseys','13.06.2017 00:00',   "Loco1506",3,40,40,40,40,40,40,40, 'UNITID')
  
  BW17<-c(# Dairygold 2017(Black & white herd) 
   './DGHF2017', '17.06.2017 00:00',  "LocoScore150617",4,
    40,40,40,40,40,40,40, 'UNITID') 
  
  #DairyGold 2018 (Black & white herd)
  BW18a<-c( './DGHF2018', '10.08.2018 00:00', "Loco080818",2,
           11, # data doesn't contain walking? SN00017FFD
           13, #SN00018D41
           13, #SN0001932C # also faulty at commercial farm
           15,#SN00018DD5 #4 cows Also didn't work at commercial farm
           40,40,40, 'Ped'  )
  
  BW18b<-c('./DGHF2018','13.08.2018 00:00', "Loco130818", 4,
           11, # data doesn't contain walking? SN00017FFD
           13, #SN00018D41
           13, #SN0001932C
           15,#SN00018DD5 
           40,40,40, 'Ped'   )
  
  #Commerical farm 
  Farma <-c( './Commercial_Farm','17.08.2018 00:00', "Loco160818",2, 1, #SN00018E33 no loco score, not attached
            1, # SN0001932C
            3, #SN000192D9
            7,7,7,
            12, 'Ped')# out by a day SN00018D79
               
  Farmb <-c('./Commercial_Farm', '20.08.2018 00:00',"Loco200818",4,  
            1, #SN00018E33 no loco score, not attached
            1, # SN0001932C - doesn't record all the way through
            3,#SN000192D9
            7,#SN00018DD5 - all lying
            7, 7, 12, 'Ped')# out by a day SN00018D79
               
  #LSE selection (Locomotion scoring event)
  Meta<-rbind.data.frame(ja, jb,BW17, BW18a,BW18b,Farma, Farmb)
  Meta$LSE<-c("1.aJerseys", "1.bJerseys","2.BW17", "3.aBW18a","3.bBW18b","4.aFarma", "4.bFarmb")
  colnames(Meta)<-MVars
  
  #initialise lists to store results from each cohort
  #LongList<-vector(mode ="list" ,nrow(Meta))
  CorList<-vector(mode ="list" ,(nrow(Meta)+3))
  SumDataList<-vector(mode ="list" ,(nrow(Meta)+3))

#Loop start----
lse<-3
    for (lse in 1:nrow(Meta)) { # Load selected locomotion scoring events
    print(c("Locomotion Scoring Event",lse))
    setwd(home) 
    # Raw xyz data folder
    setwd(as.character(Meta$Folder[[lse]]))
    setwd('./24Hourly')
    inde<-dir() # Index of file names 
    inde2<-substr(inde,start = 35,stop=44 ) # Pedometer Serial numbers
    Results3<-cbind.data.frame(inde,inde2)
    colnames(Results3)<-c("inde","UNITID")
    #Files that didn't store data  check # for making meta table 
    #Data that does not work - exclude
    for (m in 6:11){# why this range? These are the exlcude columns
      # If you want to exclude 12 &13 you exclude 12 twice (13 goes to 12th position). if 12 and 14, 12 and 13 (14 goes to 13). 
      Exclude<- as.numeric(paste0(Meta[lse,m]))
      Results3<-Results3[-Exclude,]
    }
   # Exclude<- as.numeric(paste0(Meta[lse,18])) #Necessary? 
    Results3<-Results3[-Exclude,]
    ##### Event Ref table knit together 
    #goal here is to have the unitid and score for this LSE
    setwd("../")
    Ref1<-read.csv(file = 'PedRef.csv' ,sep = ",",header = T)
    Score<-read.csv(file =  'Score.csv'  ,sep = ",",header = T)# 
    NUM <-as.numeric(paste0(Meta$Loco_Index[[lse]])) # Column with relevant loco score for this 
    Score$loco2<-Score[,NUM] #  to fix bug with Jereseys
    Ref<- left_join(Score,Ref1,paste0(Meta$JoinBy[[lse]]))
    Results2<-left_join(Results3,Ref,"UNITID")#
    Results2$inc<-is.na(Results2$loco2) 
    Results<- Results2 %>% filter(inc== F) # don't load
    Results<-Results[,-(ncol(Results))] #remove inc
    Results$loco<-Results$loco2
    Results<-Results[,c(1,2,ncol(Results))] 
    
# Used to break the Long tables with Jerseys. Using Loco2 and loco ensures the right columns get through to the next step
    {
      ## 24 hour data-----
      setwd('./24Hourly')
      inde
      #load first
      RWconvert <- fread(input = paste(Results[1,1]),sep2 = ";", header=T)
      #Append the rest
      for (i in 2:nrow(Results)) { # loads all the day records
        feat <- fread(input = paste(Results[i,1]),sep2 = ";",  header=T)
        RWconvert<-rbind.data.frame(RWconvert, feat)
      }
     RWconvert[RWconvert==0]<-NA
RWconvert<-RWconvert %>% filter(WATCHSTART==paste(Meta$WATCHSTART[[lse]])) # relevant day
names(RWconvert)
RWconvert<-RWconvert[,c(1,4:8,12:20)] 
#assign to a list
MODDF<-left_join(Results,RWconvert,'UNITID')   
MODDF<-MODDF[,-1]
MODDF<-MODDF[,-1]
#### LSE Correlation ----
       DFCor<- as.data.frame(as.matrix(cor(MODDF,use = "pairwise.complete.obs",method="spearman")))
      DFCor$Scale<-as.numeric(sqrt((DFCor[,1])^2))
      DFCor2<- DFCor[,c(1,ncol(DFCor))]
      DFCor3<-rownames_to_column(DFCor2)
      DFCor3<-DFCor3 %>% arrange(desc(Scale))
      CorList[[lse]]<- DFCor3
      library("Hmisc")
      ?rcorr
      res2 <- rcorr(as.matrix(MODDF))
      res2[1]#r
      res2[2]#n
      res2[3]# p
      res2[res2>0.1]<-NA
      SumDataList[[lse]]<-MODDF
      print("LSE")
      print(lse)
    }
    #LongList
    #SumDataList[1]
    #Model 
}

#Change variables ----
#Need to remove extra jersey from 2nd scoring
JerseyB<-as.data.frame(SumDataList[[2]]) 
  JerseyB<-JerseyB[-4,] # wasn't scored first time
  SumDataList[[8]] <- SumDataList[[1]]-JerseyB
  SumDataList[[9]]<-(as.data.frame(SumDataList[4])-as.data.frame(SumDataList[5]))  
  
  SumDataList[[10]]<-as.data.frame(SumDataList[6])-as.data.frame(SumDataList[7])

  #Change correlations ----
  for (change in 8:10) {
    MODDF<-SumDataList[[change]]
    DFCor<- as.data.frame(as.matrix(cor(MODDF,use="pairwise.complete.obs"),method="spearman")) 
    #Spearman ----
    DFCor$Scale<-as.numeric(sqrt((DFCor[,1])^2))
    DFCor2<- DFCor[,c(1,ncol(DFCor))]
    DFCor3<-rownames_to_column(DFCor2)
    CorList[[change]]<- DFCor3
  }
  
}# end of loop - 

#Results begin ----

#Summary Stats If needed ----
#Summary table - mean, IQ, RMS or SD, Highest group, lowest. 
#variables
  CorListA<-as.data.frame(CorList[1])
  for (i in 2:10){
  CorListA<-left_join(CorListA,as.data.frame(CorList[i]),"rowname")
  }

  CorListA$AbsCor <- (CorListA$loco.x+CorListA$loco.y+CorListA$loco.x.x+CorListA$loco.y.y +CorListA$loco.x.x.x+CorListA$loco.y.y.y +CorListA$loco.x.x.x.x)/7
  
# To do implement weighted average - length sumdata list 
CorListA$ChangeCor<- (CorListA$loco.y.y.y.y+ CorListA$loco.x.x.x.x.x+CorListA$loco.y.y.y.y.y)/3
  
CorListA<-CorListA[-1,-(c(2,4,6,8,10,12,14,16,18,20)+1)]
is.num <- sapply(CorListA, is.numeric)
CorListB<-cbind(CorListA[,1],as.data.frame(lapply(CorListA[is.num],round,2)))
  
colnames(CorListB)<-c("Variable", "JerseysA","JerseysB", 
"BW17", "BW18A","BW18B","FarmA", "FarmB", "Jersey Change","BW18 Change", "Farm Change","Average Cor","Av Change Cor")
#Write Behaviour correlation Table
setwd('../')
setwd('../')


                     
}

write.csv(x = CorListB,file = "Table1.csv")
#P Values - Add in stars for the few that are significant?

# Behaviours Results ---- 
inspCorJ$Variable

ListBehav<-c( "PACTIVITYCHANGE","STRIDES", "WALKTIME", "STANDINGCOUNTER", "WALKINGINDEX" ,  "LAYINGINDEX" ,   "STANDINGINDEX" ,    "STANDUP",  "WALKINGCOUNTER","LIMBEVENTS","STRIDES", "WALKINGCOUNTER","STANDINGCOUNTER", "Speed","LAYTIME","STANDTIME","LAYDOWN"  
)
BehavCorLSE<- arrange(inspCorJ[inspCorJ$Variable %in% ListBehav,],Variable)

setwd(home)
setwd("./Tables" )
write.csv(BehavCorLSE,file = "BehavCorLSE.csv" )

cor.test(dfResults$loco,dfResults$STRIDEMOVERATIO)
cor.test(dfResults$loco,dfResults$PACTIVITYCHANGE)
cor.test(dfResults$STANDINGCOUNTER,dfResults$PACTIVITYCHANGE)

cor.test(dfResults$loco,dfResults$WALKTIME)

cor.test(dfResults$loco,dfResults$Mangweth1)


#Acceleration Table ----
#selected correlations
ListSel<-c("ACTIVITY", 
           "TASumcow" ,
           "Zmean" ,   "Ymean" ,"Xmean" ,"V_ta",
           "TAmeancow",
           "Ymean","V_totalAct"
)
AccelCorLSE<- arrange(inspCorJ[inspCorJ$Variable %in% ListSel,],Variable)

setwd(home)
setwd("./Tables")
write.csv(AccelCorLSE,file = "AccelCorLSE.csv")

cor.test(dfResults$loco,dfResults$ACTIVITY)

cor.test(dfResults$loco,dfResults$AdjActivity)


# ####Table 2----
#Variables to be assessed
#Pooled mean, range in LSE means, Score 0 & Score 2
#Range in Sample (extremes) , range in LSE's mean
#How to do this? - Sumtab list - 1-7. select vars

SumTab<-SumTabList[[1]]
names(SumTab)
row.names(SumTab)

SumTab[c(1,#loco
         4:8,#Activity measure
         51:54, #Gait cycle duration #Swing duration#Swing %
         138),] #Speed 

#Mangweth - left right of back RMS indicative of lameness.
#Next run see what it is correlated with

#Alsaood Pedogram ----


cor.test(dfResults$loco,dfResults$STANDUP, method='spearman')
cor.test(dfResults$loco,dfResults$LAYINGCOUNTER, method='spearman')
cor.test(dfResults$loco,dfResults$STANDINGINDEX, method='spearman')
cor.test(dfResults$loco,dfResults$LAYTIME, method='spearman')

t.test(dfResults0$STANDUP,dfResults2$STANDUP)
t.test(dfResults0$STANDINGINDEX,dfResults2$STANDINGINDEX)

df1<-SumDataList[[8]]
cor.test(df1$loco,df1$STANDUP)


#Logistic regression----

dfAlsBinary<-rbind.data.frame(dfResults0,dfResults2)
dfAlsBinary$loco[dfAlsBinary$loco=="2"]<-"1"
dfAlsBinary$loco<-as.numeric(dfAlsBinary$loco)

L1<-glm(loco~Speed+STANDUP,data=dfAlsBinary,family='binomial')
summary(L1)
library(pscl)
pR2(L1)

