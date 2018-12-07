#Tasks
#Load required data correctly - 2017 HF, 2018 HF 
#cow eleven different- 2018 HF, add list of cows to skip?
#Check cor sumtable - seems to populate all score columns even with only one cow with one score

#Check loco_index - which column score is in
#Results all the same data

# goal now - is to create a model. 
# After Lincoln finish DE run - NO
#This script loads required packages, populates a meta table with information about the multiple locomotion scoring event to be pulled in, list are initialised to store the individual steps of each cow from each scoring event, the correlation matrix for each experiment, and summary table for each experiment, a loop then starts pulling in these experiments, (within this is a loop which pulls in both the raw data and RW converted data, processess them (detailed explaination starts there) and adds them to the long list (individual steps), the cor list (per experiment) and sumtable (list of summary tables)

#i = individual cows in an event
  # p = event/ experiment number
    
#Package loading & Define Functions - constants----------------
{
library(magrittr);library(dplyr); library(data.table); library(RcppRoll);  library(tibble);library(purrr);library(zoo);
 
mean.sd <- function(x) c(mean = mean(x), sd = sd(x))
Recs_per_Day<-24*60*60*10# records per day
Records <- 1000
side<-Records/2

home<- "D:/Lameness" # Change for your computer

#This Meta table contains the meta data for each data set required to run the script. This means the same script is applied to each data set and only these variables should change. The Selected row corresponds to the data set. In the script, then the item from that row is called. 
###Meta ---- 
MVars<-c("Raw_Path","Start","Stop", "Feature_Y_N", "Skip", "WATCHSTART","Feature path","Ref_File","Scores" ,"Score_Var","Loco_Index1" # Loco index is the column number to pull the score
)

#Jersey trial - First scoring (a) 
ja<-c("./Jerseys/Raw_XYZ",35,44, TRUE, 0.935,'02.06.2017 00:00', "./RW_Converter", "Ref.csv", "Score","Loco0106",2)

jb<-c("./Jerseys/Raw_XYZ",35,44, TRUE, 11.965,'13.06.2017 00:00', "./RW_Converter", "Ref.csv", "Score","Loco1506",3)
DE<-c("./HaladajanData/Raw_XYZ",35,44,FALSE, 0,'NArwConvert', "./RW_Converter", NA,"Scores",NA,NA)

BW17<-c("./DGHF2017/Raw_XYZ",35,44, TRUE, .69,'16.06.2017 00:00', "./RW_Converter", "BlueRef16.csv", "RWBlue16th17th","LocoScore150617",4) # Dairygold 2017

#DairyGold 2018 (start and finish)
BW18a<-c("./DGHF2018/Raw_XYZ",35,44, TRUE, .74,'09.08.2018 00:00
', "./RW_Converter","PedRef.csv","Scores130818.csv","Loco080818",2)

BW18b<-c("./DGHF2018/Raw_XYZ",35,44, TRUE, 4.42,'13.08.2018 00:00',"./RW_Converter","PedRef.csv", "Scores130818.csv","Loco130818", 4)  # loco index?

#Commerical farm - start and end. 
Farma <-c("./Commercial_Farm/Raw_XYZ",35,44, TRUE, 0.855,'16.08.2018 00:00',"./RW_Converter","PedRef.csv", "ScoresPed", "Loco160818",20)
Farmb <-c("./Commercial_Farm/Raw_XYZ",35,44, TRUE, 5.41,'20.08.2018 00:00',"./RW_Converter","PedRef", "ScoresPed","Loco200818",21)
#Farm data shared by German researcher
#Event select ----
Meta<-rbind.data.frame(#ja,jb,
                       #BW17,
                       BW18a
                       #,BW18b,Farma, Farmb
              )
colnames(Meta)<-MVars

#initialise lists to store results from each cohort
LongList<-vector(mode ="list" ,nrow(Meta))
CorList<-vector(mode ="list" ,nrow(Meta))
SumTabList<-vector(mode ="list" ,nrow(Meta))
}
##### Long loop start ~ 11 minutes----
for (p in 1:nrow(Meta)) { # Loads selected experiments/ events
  print("start")
setwd(home) 
# Raw xyz data folder
  Event<-p
setwd(as.character(Meta$Raw_Path[[Event]]))
inde<-dir() # List of file names 
inde2<-substr(inde,start = paste(Meta$Start[[Event]]),stop= paste(Meta$Stop[[Event]])) # Ped Serial numbers
Results<-cbind.data.frame(inde,inde2)
colnames(Results)<-c("inde","UNITID")
##### Event Ref table  -------
  setwd("../")
  Ref1<-read.csv(file =  paste(Meta$Ref_File[Event]) ,sep = ",",header = T)
  Score<-read.csv(file =  paste(Meta$Scores[Event]) ,sep = ",",header = T)# 
NUM <-as.numeric(paste0(Meta$Loco_Index1[[Event]])) # Column with relevant loco score for this 
  Score$loco<-Score[,NUM]
  Ref<-left_join(Score,Ref1,"Ped") 
    
Results2<-left_join(Results,Ref,"UNITID")#-----
Results2$inc<-is.na(Results2$loco) # Score missing

Results<- Results2 %>% filter(inc== F) # remove from loading
Results<-Results[,-(ncol(Results))]
Results<-Results[,c(1,ncol(Results))]
# Load XYZ data---------------------------------------

for (i in 1:nrow(Results)) {
  setwd(home)
  setwd(as.character(Meta$Raw_Path[[Event]])) # select data set 
    cow <- fread(input = paste(Results[i,1]),sep2 = ";", skip = (Recs_per_Day*as.numeric(paste(Meta$Skip[[Event]]))), nrows = (Recs_per_Day*0.05), header=F,select = c(1,3,4,5,8),showProgress = T) 
#head(cow)
#tail(cow)
#plot(cow$V8[2:70000]) 
    cow <- fread(input = paste(Results[10,1]),sep2 = ";", skip = (Recs_per_Day*as.numeric(paste(Meta$Skip[[Event]]))), nrows = (Recs_per_Day*0.15), header=F,select = c(1,3,4,5,8),showProgress = T) 
    
    
    setnames(cow, c("Tstamp","x","y","z","state"))
#i
# Process raw data ----
cow$totalAct<- sqrt(cow$x^2)+ sqrt(cow$y^2)+sqrt(cow$z^2)
stand<-cow %>% filter (state=="1") # After Actvity

cow$ta <- 1:(nrow(cow)) #index
#Consistent Walking ----
cow<-cow[(as.numeric(side)):(nrow(cow)-side),] 
cow$cw<- roll_sum(cow$state,n = Records,align = "center")
walk<-cow %>% filter (cow$cw==(Records*2))

#Invert check----
#Make sure x is forward (can be facing backwards depending on leg or if upside down)
invert<-mean(walk$x)<0
# if (!is.na(invert)) #?
if (invert){ # if x is less than 0 - facing backwards
 walk$x<-((walk$x)*(-1)) # So invert values 
} else {} # # If correct - invertion not performed # Most values in first study needed inverting so set as default

# *?Rotated check? ----

#Thresholds----
walk$x[walk$x<15&walk$x>(-15)] <-0 # Setting low values to zero - likely noise
walk<-walk[,c(1:4,6,7)]
#   plot(walk$x[17200:17600])
 #  plot(walk$x[1:10])
low<-quantile(walk$x,probs = 0.012) # 10th percentile of x values
high<-quantile(walk$x,probs = 0.99) #90th percentile of x value
walk$strike<-walk$x<(low)
#plot(walk$strike[1:6000])
#group by steps by cow, group etc 
# indicates if value is below 10th percentile and so a candidate as being the strike point.
#(sum(walk$strike)/nrow(walk))*100
#The strike point is the value of greatest deceleration as the foot strikes the ground. Multiple values may be below the threshold so need to compare strike candidate values with those around them. 

#Widen step data----
veca<-walk$x 
veca<- c(1:10,veca,1:10) # add some values to the tails for matching vector lengths - makes the first and last steps useless. Remove tail later.

walk$dm1<-veca[10:(nrow(walk)+9)] # x 1 before (m= minus)
walk$dm2<-veca[9:(nrow(walk)+8)] # 2 before (m= minus)
walk$dm3<-veca[8:(nrow(walk)+7)] # minus
walk$dm4<-veca[7:(nrow(walk)+6)] # minus
walk$dm5<-veca[6:(nrow(walk)+5)] # minus
walk$dm6<-veca[5:(nrow(walk)+4)]
walk$dm7<-veca[4:(nrow(walk)+3)]
walk$dm8<-veca[3:(nrow(walk)+2)]
walk$dm9<-veca[2:(nrow(walk)+1)]
walk$dm10<-veca[1:(nrow(walk))]

walk$d1<-veca[12:(nrow(walk)+11)] # records after 
walk$d2<-veca[13:(nrow(walk)+12)]
walk$d3<-veca[14:(nrow(walk)+13)]
walk$d4<-veca[15:(nrow(walk)+14)]

walk<-walk[10:(nrow(walk)-10),]

steps <- walk %>% filter (walk$strike==T) # below threshold with surrounding records. Candidate strike points

steps$lowest<- (steps$x<steps$dm1& steps$x<steps$dm1) # Check it is the local nadir
steps2a <- steps %>% filter(lowest==T)
#repeat necessary? Only finds a few records?
steps2a$lowest<- (steps2a$x<steps2a$dm2& steps2a$x<steps2a$dm2) 
steps2 <- steps2a %>% filter(lowest==T)

### Duration ----
tplus1<- c(steps2$ta[2:(nrow(steps2)-1)],1,1) # shift
steps2$duration<-tplus1-steps2$ta 
steps3 <- steps2 %>% filter(steps2$duration > 5 & steps2$duration < 20 ) # Filter steps with durations outside normal (about 40% cut)
# 50% of steps make it to steps3
#Swing PC---- 
#Select variables for swing analysis for swing percentage
names(steps3)
swingdf<-steps3[,c(1,14:8,2,18:23)]

#Strategy - count NA's from 7 records back from Strike
#Sometimes 1,3,4,5 will have values and 2 will be zeroed or
#3,4,5 and 7 will have values but 6 is zeroed even though 6 was in the swing phase
#Show example in reported or link to it in google
#https://drive.google.com/drive/folders/16kO-ssIwYv1nLP8_lYQ_3Q7oDMcaLOmu?usp=sharing 
#in the first line dm3 is part of the swing phase and so should be counted as part of it
swingdf$dm5[swingdf$dm6 != "0"] <-25 # if value before, have a value
swingdf$dm4[swingdf$dm5 != "0"] <-25 # if value before, have a value
swingdf$dm3[swingdf$dm4 != "0"] <-25 # if value before, have a value
swingdf$dm2[swingdf$dm3 != "0"] <-25
swingdf$dm1[swingdf$dm2 != "0"] <-25
swingdf[swingdf=="0"] <-NA # easier to count NA's?

for (h in 1:nrow(swingdf)){ # count NA's in range- 
 steps3$swing_dur[h] <- 7-sum(is.na(swingdf[h,4:10]))
}

steps3 <- steps3 %>% mutate(swingPC = swing_dur/duration )

#send step3 data to be aggregated for machine learning???
#Or walk data sample
i
# Results XYZ ------------
Results$nrow_steps[i] <- nrow(steps) 
#steps3
Results$n_steps3[i] <- nrow(steps3) 
Results$prop_steps<-Results$n_steps3[i]/Results$nrow_steps[i]
Results$M_X_steps3[i] <- mean(steps3$x)
Results$V_X_steps3[i] <- var(steps3$x)

Results$M_Duration_steps3[i] <- mean(steps3$duration)
Results$V_Duration_steps3[i] <- var(steps3$duration)

Results$M_Activity_steps3[i] <- mean(steps3$totalAct) 
Results$V_Activity_steps3[i] <- var(steps3$totalAct) 

Results$M_SwingPC[i] <- mean(steps3$swingPC)
Results$V_SwingPC[i] <- var(steps3$swingPC)

Results$M_Swing[i] <- mean(steps3$swing_dur)
Results$V_Swing[i] <- var(steps3$swing_dur)

Results$D1_M[i] <- mean(steps3$d1)
Results$D2_M[i] <- mean(steps3$d2) 
Results$D3_M[i] <- mean(steps3$d3) 
Results$D4_M[i] <- mean(steps3$d4)
Results$D1_V[i] <- var(steps3$d1)
Results$D2_V[i] <- var(steps3$d2) 
Results$D3_V[i] <- var(steps3$d3) 
Results$D4_V[i] <- var(steps3$d4)

Results$DM1_M[i] <- mean(steps3$dm1)
Results$DM2_M[i] <- mean(steps3$dm2) 
Results$DM3_M[i] <- mean(steps3$dm3) 
Results$DM4_M[i] <- mean(steps3$dm4)
Results$DM1_V[i] <- var(steps3$dm1)
Results$DM2_V[i] <- var(steps3$dm2) 
Results$DM3_V[i] <- var(steps3$dm3) 
Results$DM4_V[i] <- var(steps3$dm4)

#Walk
Results$N_walk[i] <-nrow(walk) 

Results$M_Activity_Walk[i] <- mean(walk$totalAct)
Results$V_Activity_Walk[i] <- var(walk$totalAct) #8
 
#Stand
Results$N_Stand[i] <-nrow(stand)
Results$M_Activity_Stand[i] <- mean(stand$totalAct) 
Results$V_Activity_Stand[i] <-var(stand$totalAct) #9

#Values
Results$Low [i] <- low
Results$High[i] <- high

#names(Results)
steps3$Loco <- Results$Loco[i]
steps3$UNITID <- Results$UNITID[i]
# long data
if (exists("long")) {
  long = rbind(long, steps3)
} else {
  long = steps3

print(i)
timestamp()
}
} # End of XYZ data loop
wa##Rw Converter-----
setwd("../")
setwd(paste(Meta$`Feature path`[Event])) # features
inde3<-dir()

#load first
RWconvert <- fread(input = inde3[1],sep2 = ";", header=T)
#Append the rest

for (i in 2:length(inde3)) { # loads all the day records
  feat <- fread(input = inde3[i],sep2 = ";",  header=T)
  RWconvert<-rbind.data.frame(RWconvert, feat)
}
#selects the relevant days in the 
RWconvert<-RWconvert %>% filter(WATCHSTART==paste(Meta$WATCHSTART[[Event]])) # relevant day
RWconvert<-RWconvert[,c(1,4:8,12:24,28)]    

Results4<-left_join(Results,RWconvert,"UNITID")

#Mutate Summary Data ----
#names(Results)
SumData <- Results4#[,c(2,25: #Summary data 
SumData$D1_4 <- (SumData$D1_M+SumData$D2_M+SumData$D3_M+SumData$D4_M)
healthyDF<- SumData[(SumData$Loco== "0"),]
healthy_Swing_PC<-mean(healthyDF$M_SwingPC)
SumData$SwingDiff<- sqrt((healthy_Swing_PC-SumData$M_SwingPC)^2)
SumData$Speed<-SumData$STRIDEDISTANCE/SumData$STRIDEDURATION
print("Mutate done")

#### Correlation ----
names(SumData) 
#SumData[,c(34,4:8,12:24,37:ncol(SumData))]

MODDF<- SumData[,c(3:ncol(SumData))] # Numeric only?
# Change here  selection of columns and loco score # maybe just cor to loco

DFCor<- as.data.frame(as.matrix(cor(MODDF)))
DFCor$Scale<-as.numeric(sqrt((DFCor[,1])^2))

DFCor2<- DFCor[,c(1,ncol(DFCor))]
DFCor3<-rownames_to_column(DFCor2)
#DFCor3<-DFCor3 %>% arrange(desc(Scale))
print("Correlation done")

#### Summary Table----

MODDF$Loco<-MODDF[,1]
MODDF0<- MODDF %>% filter (Loco==0)
options(scipen=99)
options(digits=1)
Table0<-t(as.data.frame(map(MODDF0,.f = (mean.sd))))

MODDF1<- MODDF %>% filter (Loco==1)
Table1<-t(as.data.frame(map(MODDF1,.f = (mean.sd))))
MODDF2<- MODDF %>% filter (Loco==2)
Table2<-t(as.data.frame(map(MODDF2,.f = (mean.sd))))

SumTab<-cbind.data.frame(Table0,Table1,Table2)
print ("Table ready")
#Adding to experimetents / events / p
LongList[[p]]<-long
CorList[[p]]<- DFCor3
SumTabList[[p]]<-SumTab
# Models generated and stored 
#R^2 stored
print(p)
}


LongList
