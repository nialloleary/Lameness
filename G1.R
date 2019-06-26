<<<<<<< HEAD
#This script summarises Acceleration only factors from the 10 hz data
#Goal Mean and Variance of each variable for all seven events Like summary table 2 of paper 1

#Package loading & Define Functions & set constants-----------
  { # 467
  home<- "C:/Users/olearyn2/OneDrive - Lincoln University/Lameness/RW_Acceleration_and_Behavior"  # location of Lameness file on your computer - data available from Nialloleary@gmail.com
  library(magrittr);library(dplyr); library(data.table); library(RcppRoll);  library(tibble);library(purrr);library(zoo);
  
  #Function mean and Standard deviation combined 
#  mean.sd <- function(x) c(mean = mean(x), sd = sd(x))
=======
#Tasks to do
# Normal step - graph 
# Ggplot - so we want the steps df from the list
#currently only a small window
#Long table - 45248 steps from every cow and lse
#change from LSE to trial a & B

#Summary table
#Stride duration - has zeros - need to assign NA's to zeros biasing down - After RW convert brought in - assign NA's to 0's and follow the errors

#So in the jersey hourly summaries- stride distance appears reasonable 
#Look for other hourly summaries


# OVERVIEW
#This script: ##populates a meta table with information about the multiple locomotion scoring events 
##lists are initialised to store 
###records for individual steps (long), 
### the correlation matrix for each experiment, and 
###summary table for each experiment, 
#A loop (duration ~13 minutes) then starts loading data from these experiments
##Within this is a loop both the raw data and Rumi Watch software converted data is loaded, processed (detailed explaination there) and ##Adds them to the long list (individual steps), the cor list (per locomotin scoring event) and sumtable (list of summary tables from each locomotin scoring event)

#For loop characters
#i = individual cows in a locomotin scoring event
# lse = locomotin scoring event/ experiment number
#h is for cycling through records around strike point in gait analysis

#Package loading & Define Functions & set constants-----------
{{ # 467
  home<- "D:/RW_Acceleration_And_Behavior_Converted" # locoation of Lameness file on your computer - data available from Nialloleary@gmail.com
  library(magrittr);library(dplyr); library(data.table); library(RcppRoll);  library(tibble);library(purrr);library(zoo);
  
  #Function mean and Standard deviation combined 
  mean.sd <- function(x) c(mean = mean(x), sd = sd(x))
  SEM <- function(x) sd(x)/sqrt(length(x))
  options(scipen=99,digits=3)
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
  #Constants
  Recs_per_Day<-24*60*60*10# records per day
  Records <- 600 # To be examined in the consistent walking analysis 60 seconds, 1 minute
  side<-Records/2 # when centred time before and after
  
#This Meta table contains the meta data for locomotion scoring event required to run the script. This facilitate same script being applied to each data set. Only these variables should change between locmotion scoring events. The Selected row corresponds to the locomotion scoring event (lse). In the script, then the item from that row is called. 
  
  MVars<-c("Raw_Path", #Meta variables
<<<<<<< HEAD
           "Start","Stop",  "Skip", "WATCHSTART","Feature path","Ref_File","Scores" ,"Score_Var","Loco_Index1","ExcludedCows","ExcludedCows2","ExcludedCows3","ExcludedCows4","ExcludedCows5","ExcludedCows6","ExcludedCows7","JoinBy","LSE")
  #Meta ----
  #Jersey trial - First scoring (a) 
  ja<-c("./Jerseys",35,44,  0.935,'02.06.2017 00:00', "./RW_Converter", "PedRef.csv", "Score.csv","Loco0106",2,40,40,40,40,40,40,40,"UNITID")
  jb<-c("./Jerseys",35,44,  12.3,'13.06.2017 00:00', "./RW_Converter", "PedRef.csv", "Score.csv","Loco1506",3,40,40,40,40,40,40,40,"UNITID")
  
  BW17<-c(# Dairygold 2017(Black & white herd) 
    "./DGHF2017",35,44, 1.67,'17.06.2017 00:00', "./RW_Converter", "PedRef.csv", "Score.csv","LocoScore150617",4,
    40,40,40,40,40,40,40,"UNITID") 
  
  #DairyGold 2018 (Black & white herd)
  BW18a<-c("./DGHF2018",35,44,  0.39,'10.08.2018 00:00', "./RW_Converter","PedRef.csv","Score.csv","Loco080818",2,
     11, # data doesn't contain walking? SN00017FFD
     13, #SN00018D41
   13, #SN0001932C # also faulty at commercial farm
 15,#SN00018DD5 #4 cows Also didn't work at commercial farm
     40,40,40,"Ped" )

BW18b<-c("./DGHF2018",35,44,  4.42,'13.08.2018 00:00',"./RW_Converter","PedRef.csv", "Score.csv","Loco130818", 4,
         11, # data doesn't contain walking? SN00017FFD
         13, #SN00018D41
         13, #SN0001932C
         15,#SN00018DD5 
         40,40,40,"Ped"
  )
  
  #Commerical farm 
Farma <-c("./Commercial_Farm",35,44,  0.8,'17.08.2018 00:00',"./RW_Converter","PedRef.csv", "Score.csv", "Loco160818",2, 1, #SN00018E33 no loco score, not attached
            1, # SN0001932C
            3, #SN000192D9
            7,7,7,
            12,"Ped")# out by a day SN00018D79
  
  Farmb <-c("./Commercial_Farm",35,44,  4.405,'20.08.2018 00:00',"./RW_Converter","PedRef.csv", "Score.csv","Loco200818",4,  
=======
           "Start","Stop", "Feature_Y_N", "Skip", "WATCHSTART","Feature path","Ref_File","Scores" ,"Score_Var","Loco_Index1","ExcludedCows","ExcludedCows2","ExcludedCows3","ExcludedCows4","ExcludedCows5","ExcludedCows6","ExcludedCows7","24hr1hr"
  )
  #Meta ----
  #Jersey trial - First scoring (a) 
  ja<-c("./Jerseys/Raw_XYZ",35,44, TRUE, 0.935,'02.06.2017 00:00', "./RW_Converter", "Ref.csv", "Score.csv","Loco0106",2,40,40,40,40,40,40,40,"Hourly")
  jb<-c("./Jerseys/Raw_XYZ",35,44, TRUE, 12.3,'13.06.2017 00:00', "./RW_Converter", "Ref.csv", "Score.csv","Loco1506",3,40,40,40,40,40,40,40,"Hourly")
  
  BW17<-c(# Dairygold 2017(Black & white herd) 
    "./DGHF2017/Raw_XYZ",35,44, TRUE, 1.67,'17.06.2017 00:00', "./RW_Converter", "PedRef.csv", "BlueRef16to190617.csv","LocoScore150617",4,
    40,40,40,40,40,40,40,"NoHourly") 
  
  #DairyGold 2018 (Black & white herd)
  BW18a<-c("./DGHF2018/Raw_XYZ",35,44, TRUE, 0.39,'10.08.2018 00:00', "./RW_Converter","PedRef.csv","Scores130818.csv","Loco080818",2,
           11, # data doesn't contain walking? SN00017FFD
           13, #SN00018D41
           13, #SN0001932C # also faulty at commercial farm
           15,#SN00018DD5 #4 cows Also didn't work at commercial farm
           40,40,40,"NoHourly"
  )
  BW18b<-c("./DGHF2018/Raw_XYZ",35,44, TRUE, 4.42,'13.08.2018 00:00',"./RW_Converter","PedRef.csv", "Scores130818.csv","Loco130818", 4,
           11, # data doesn't contain walking? SN00017FFD
           13, #SN00018D41
           13, #SN0001932C
           15,#SN00018DD5 
           40,40,40,"NoHourly"
  )
  
  #Commerical farm 
  Farma <-c("./Commercial_Farm/Raw_XYZ",35,44, TRUE, 0.8,'17.08.2018 00:00',"./RW_Converter","PedRef.csv", "ScoresPed.csv", "Loco160818",2, 1, #SN00018E33 no loco score, not attached
            1, # SN0001932C
            3, #SN000192D9
            7,7,7,
            12,# out by a day SN00018D79
            "NoHourly"
            ) #
  Farmb <-c("./Commercial_Farm/Raw_XYZ",35,44, TRUE, 4.405,'20.08.2018 00:00',"./RW_Converter","PedRef.csv", "ScoresPed.csv","Loco200818",4,  
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
            1, #SN00018E33 no loco score, not attached
            1, # SN0001932C - doesn't record all the way through
            3,#SN000192D9
            7,#SN00018DD5 - all lying
            7,
            7,
            12# out by a day SN00018D79
<<<<<<< HEAD
            ,"Ped"
=======
            ,"NoHourly"
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
            ) 
  
  #LSE selection (Locomotion scoring event)
  Meta<-rbind.data.frame(ja, jb,BW17, BW18a,BW18b,Farma, Farmb)
  Meta$LSE<-c("1.JerseysA", "2.JerseysB","3.BW17", "4.BW18a","5.BW18b","6.Farma", "7.Farmb")
  colnames(Meta)<-MVars
  
  #initialise lists to store results from each cohort
  #LongList<-vector(mode ="list" ,nrow(Meta))
<<<<<<< HEAD
    SumTabList<-vector(mode ="list" ,(nrow(Meta)+3))
=======
  CorList<-vector(mode ="list" ,(nrow(Meta)+3))
  SumTabList<-vector(mode ="list" ,(nrow(Meta)+3))
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
  SumDataList<-vector(mode ="list" ,(nrow(Meta)+3))
  
  
  ##### Long loop start 11+ minutes----
  for (lse in 1:nrow(Meta)) { # Load selected locomotion scoring events
    print(c("Locomotion Scoring Event",lse))
<<<<<<< HEAD
   
    setwd(home) 
    # Raw xyz data folder
    setwd(as.character(Meta$Raw_Path[[lse]]))
    setwd("./10Hz")
=======
    print(Meta$Score_Var[lse])
    setwd(home) 
    # Raw xyz data folder
    setwd(as.character(Meta$Raw_Path[[lse]]))
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
    inde<-dir() # Index of file names 
    inde2<-substr(inde,start = paste(Meta$Start[[lse]]),stop= paste(Meta$Stop[[lse]])) # Pedometer number& Serial numbers
    
    Results<-cbind.data.frame(inde,inde2)
    colnames(Results)<-c("inde","UNITID")
<<<<<<< HEAD
    names(Meta)
    for (m in 11:17){ 
      Exclude<- as.numeric(paste0(Meta[lse,m]))
      Results<-Results[-Exclude,]
    }
=======
    #Files that didn't store data  check # for making meta table 
    #Data that does not work - exclude
    for (m in 12:18){# why 12 to 18? These are the exlcude columns
      # If you want to exclude 12 &13 you exclude 12 twice (13 goes to 12th position). if 12 and 14, 12 and 13 (14 goes to 13). 
      Exclude<- as.numeric(paste0(Meta[lse,m]))
      Results<-Results[-Exclude,]
    }
    Exclude<- as.numeric(paste0(Meta[lse,18])) #Necessary? 
    Results<-Results[-Exclude,]
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
    ##### Event Ref table knit together 
    setwd("../")
    Ref1<-read.csv(file =  paste(Meta$Ref_File[lse]) ,sep = ",",header = T)
    Score<-read.csv(file =  paste(Meta$Scores[lse]) ,sep = ",",header = T)# 
    NUM <-as.numeric(paste0(Meta$Loco_Index1[[lse]])) # Column with relevant loco score for this 
    Score$loco2<-Score[,NUM] #  to fix bug with Jereseys
<<<<<<< HEAD
    Ref<-left_join(Score,Ref1,paste0(Meta$JoinBy[[lse]])) #breaks at 4
=======
    Ref<-left_join(Score,Ref1,"Ped") 
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
    Results2<-left_join(Results,Ref,"UNITID")#
    Results2$inc<-is.na(Results2$loco2) 
    
    Results<- Results2 %>% filter(inc== F) # don't load
    Results<-Results[,-(ncol(Results))] #remove inc
    Results$loco<-Results$loco2
    Results<-Results[,c(1,2,ncol(Results))] # Used to break the Long tables with Jerseys. Using Loco2 and loco ensures the right columns get through to the next step
    
    # Load XYZ data--------------------
    for (i in 1:nrow(Results)) {
      print(lse)
      print(c("cow",i))
      setwd(home)
<<<<<<< HEAD
      setwd(as.character(Meta$Raw_Path[[lse]]))
      setwd("./10Hz")
      # select data set 
=======
      setwd(as.character(Meta$Raw_Path[[lse]])) # select data set 
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
      
      cow <- fread(input = paste(Results[i,1]),sep2 = ";", skip = (Recs_per_Day*as.numeric(paste(Meta$Skip[[lse]]))), 
                   nrows = (Recs_per_Day*0.25),#amount of data to be 
                   header=F, select = c(1,3,4,5,8)) 
      
      setnames(cow, c("Tstamp","x","y","z","state"))
      
      
      #Redundancy - pick one
      cow$x<-scale(x = cow$x,center = F,scale = T)
      cow$y<-scale(x = cow$y,center = F,scale = T)
      cow$z<-scale(x = cow$z,center = F,scale = T)
      # Leaving center f for invert check
      
      cow$totalAct<- sqrt(cow$x^2)+ sqrt(cow$y^2)+sqrt(cow$z^2)
      stand<-cow %>% filter (state=="1") # 0= Lying, 1 standing, 2 = walking
      cow$ta <- 1:(nrow(cow)) # numeric index used later
      
<<<<<<< HEAD
      
      cow<-cow[(as.numeric(side)):(nrow(cow)-side),] 
     
      #Make sure x is forward 
      invert<-mean(cow$x)<0
      print(invert)
      
      if (invert){ # if x is less than 0 - facing backwards
        cow$x<-((cow$x)*(-1)) # So invert values 
      }
    
      #mean & variance----
      #All the mean values
    
=======
      #Consistent Walking RW state 2 walking not consistent----
      cow<-cow[(as.numeric(side)):(nrow(cow)-side),] 
      cow$cw<- roll_sum(cow$state,n = Records,align = "center")
      walk<-cow %>% filter (cow$cw>(Records*1.6))
      
      #Make sure x is forward 
      invert<-mean(walk$x)<0
      print(invert)
      
      if (invert){ # if x is less than 0 - facing backwards
        walk$x<-((walk$x)*(-1)) # So invert values 
      } 
      
      low<-quantile(walk$x,probs = 0.075) # 1/13 records percentile of x values
      
      walk$strike<-walk$x<(low) # indicates if value is below the chosen percentile 
      #Likely The value of greatest deceleration as the foot strikes the ground. 
      
      high<-quantile(walk$x,probs = 0.99) #90th percentile of x value
      
      walk<-walk[,c(1:4,6,7,9)]
      
      #Widen step data----
      {
        veca<-roll_mean(x=walk$x,n = 3,align = "center") 
        veca<- c(1:11, #padding for matching vector lengths
                 veca,
                 1:11) # These are trimmed later.
        
        walk$xm1<-veca[10:(nrow(walk)+9)]# x 1 before (m= minus)
        walk$xm2<-veca[9:(nrow(walk)+8)] # 2 before (m= minus)
        walk$xm3<-veca[8:(nrow(walk)+7)] # minus
        walk$xm4<-veca[7:(nrow(walk)+6)] # minus
        walk$xm5<-veca[6:(nrow(walk)+5)] # minus
        walk$xm6<-veca[5:(nrow(walk)+4)]
        walk$xm7<-veca[4:(nrow(walk)+3)]
        walk$xm8<-veca[3:(nrow(walk)+2)]
        walk$xm9<-veca[2:(nrow(walk)+1)]
        walk$xm10<-veca[1:(nrow(walk))]
        
        walk$d1<-veca[12:(nrow(walk)+11)] # records after 
        walk$d2<-veca[13:(nrow(walk)+12)]
        walk$d3<-veca[14:(nrow(walk)+13)]
        walk$d4<-veca[15:(nrow(walk)+14)]
        
        walk<-walk[10:(nrow(walk)-10),]
        
        #also for y
        veca<-walk$y 
        veca<- c(1:10,veca,1:10) # add some values to the tails for matching vector lengths - makes the first and last steps useless. Remove tail later.
        
        walk$dym1<-veca[10:(nrow(walk)+9)] # x 1 before (m= minus)
        walk$dym2<-veca[9:(nrow(walk)+8)] # 2 before (m= minus)
        walk$dym3<-veca[8:(nrow(walk)+7)] # minus
        walk$dym4<-veca[7:(nrow(walk)+6)] # minus
        walk$dym5<-veca[6:(nrow(walk)+5)] # minus
        walk$dym6<-veca[5:(nrow(walk)+4)]
        walk$dym7<-veca[4:(nrow(walk)+3)]
        walk$dym8<-veca[3:(nrow(walk)+2)]
        walk$dym9<-veca[2:(nrow(walk)+1)]
        walk$dym10<-veca[1:(nrow(walk))]
        
        walk$dy1<-veca[12:(nrow(walk)+11)] # records after 
        walk$dy2<-veca[13:(nrow(walk)+12)]
        walk$dy3<-veca[14:(nrow(walk)+13)]
        walk$dy4<-veca[15:(nrow(walk)+14)]
        
        walk<-walk[10:(nrow(walk)-10),]
        
        #also for z
        veca<-walk$z 
        veca<- c(1:10,veca,1:10) # add some values to the tails for matching vector lengths - makes the first and last steps useless. Remove tail later.
        
        walk$dzm1<-veca[10:(nrow(walk)+9)] # x 1 before (m= minus)
        walk$dzm2<-veca[9:(nrow(walk)+8)] # 2 before (m= minus)
        walk$dzm3<-veca[8:(nrow(walk)+7)] # minus
        walk$dzm4<-veca[7:(nrow(walk)+6)] # minus
        walk$dzm5<-veca[6:(nrow(walk)+5)] # minus
        walk$dzm6<-veca[5:(nrow(walk)+4)]
        walk$dzm7<-veca[4:(nrow(walk)+3)]
        walk$dzm8<-veca[3:(nrow(walk)+2)]
        walk$dzm9<-veca[2:(nrow(walk)+1)]
        walk$dzm10<-veca[1:(nrow(walk))]
        
        walk$dz1<-veca[12:(nrow(walk)+11)] # records after 
        walk$dz2<-veca[13:(nrow(walk)+12)]
        walk$dz3<-veca[14:(nrow(walk)+13)]
        walk$dz4<-veca[15:(nrow(walk)+14)]
        
        walk<-walk[10:(nrow(walk)-10),] #trim records
        
      }
      steps <- walk %>% filter (walk$strike==T) # below threshold with surrounding records. Candidate strike points
      Results$nrow_steps[i] <-  nrow(steps) 
      
      steps$lowest<- (steps$x<steps$xm1& steps$x<steps$d1) # Check it is the local nadir
      steps2a <- steps %>% filter(lowest==T)
      #repeat necessary? Only finds a few records?
      steps2a$lowest<- (steps2a$x<steps2a$xm2& steps2a$x<steps2a$d2) 
      steps2a <- steps2a %>% filter(lowest==T)
      steps2<-steps2a[,-(ncol(steps2a))]
      
### Duration & swing  ----
      tplus1<- c(1,1,steps2$ta[2:(nrow(steps2)-1)]) # shift
      steps2$duration<-steps2$ta -tplus1
      #hist(steps2$duration)
      steps3 <- steps2 %>% filter(steps2$duration > 6 & steps2$duration < 19 ) # Filter steps with durations outside normal
      
      #Select variables for swing analysis for swing percentage
      swingdf<-sqrt((steps3[,14:9])^2)
      swingdf[swingdf<(0.5)]<-0 # threshold
      #Alternative strategy = row mins - find zero accel point
      for (d in 1:nrow(swingdf)){
        swingdf$min[d] <-min(swingdf[d,1:6])
      }
      
      swingdf[,1:6]<-swingdf[,1:6]==swingdf$min # true /false
      swingdf[(nrow(swingdf)+1), #extra row
              1:(ncol(swingdf))] <- # each column 
        c(6:1, #assign list  of 6 to 1
          1) #  1 row 
      # 1 or 0 and refernce in last row
      #r = row
      
      for (r in 1:(nrow(swingdf)-1)){# for every row
        for (Col in 1:6){ #then for each column
          if  (swingdf[r,Col]==T){ # if one - if equal to minimum, if stance phase
            steps3$SwingDur[r]<-(swingdf[nrow(swingdf),Col])
            #overrights until stance phase ends - .
          } 
        }
      }
      
      
      
      
      steps3<- steps3 %>% mutate(swingPC=SwingDur/duration)
      
      steps3<-steps3[steps3$swingPC>0.2&steps3$swingPC<0.55,]
      #reasonable range
      mean(steps3$swingPC)
      steps3$sPCdiff<- (steps3$swingPC-0.419)^2 # source of this figure and date? 
      steps3$loco <- Results$loco[i]
      steps3$UNITID <- Results$UNITID[i]
      steps3$lse <- lse
      print(i)
      # Results XYZ ------------
      steps3<-steps3[,-7] # remove strike
      Results$n_steps3[i] <- nrow(steps3) 
      steps4<-steps3[,2:(ncol(steps3)-3)]
      
      #mean & variance----
      #All the mean values
      Results[i,6:(5+ncol(steps4))]<-as.vector(colMeans(steps4)) #All the variances values
      Results[i,(6+ncol(steps4)):(5+(ncol(steps4)*2))]<-as.vector(apply(steps4,2,FUN = var))
      
      Results$SEM_SwingPC[i]<-SEM(steps3$swingPC)
      Results$Low[i]<-low
      Results$High[i]<-high
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
      Results$Xmean[i]<-mean(cow$x)
      Results$Ymean[i]<-mean(cow$y)
      Results$Zmean[i]<-mean(cow$z)
      
      Results$XSum[i]<-sum(cow$x)
      Results$YSum[i]<-sum(cow$y)
      Results$ZSum[i]<-sum(cow$z)
<<<<<<< HEAD

    SumDataList[lse]<-Results
      
    }
  }
  }
  #  names(Results)<-c("inde","UNITID","loco","nsteps","nsteps3","Xmean","Ymean", "Zmean","Xsum","Ysum", "Zsum")

    # End of XYZ data loop
 

   {

            #### LSE Summary Table----
=======
      
      Results$TASumWalk[i]<-sum(walk$totalAct)
      Results$TAmeancow[i]<-mean(walk$totalAct)
      Results$TASumcow[i]<-sum(steps4$totalAct)
      Results$Mangweth1[i]<-sqrt(mean(walk$y)^2)
      Results$Mangweth2[i]<-sqrt(mean(cow$y)^2)
      #variance? Standardised effect? ----
      #names(Results)
      
      
      # long data
      if (exists("long")) {
        long = rbind(long, steps3)
      } else {
        long = steps3
      }
      print(nrow(steps))
      timestamp()
    }
    
    names(Results)<-c("inde","UNITID","loco","nsteps","nsteps3", paste("M", names(steps4), sep="_"),paste("V", names(steps4), sep="_"),"SEM_SwingPC","Low","High","Xmean","Ymean", "Zmean","Xsum","Ysum", "Zsum","TASumWalk","TAmeancow","TASumcow","Mangweth1","Mangweth2")
    
    # End of XYZ data loop
    {
      ##Rw Converter-----
    #  setwd(as.character(Meta$Raw_Path[[lse]]))
      setwd("../")
      #lse<-4
      setwd(paste(Meta$`Feature path`[lse])) # features
      inde3<-dir()
      
      #load first
      RWconvert <- fread(input = inde3[1],sep2 = ";", header=T)
      #Append the rest
      
      for (i in 2:length(inde3)) { # loads all the day records
        feat <- fread(input = inde3[i],sep2 = ";",  header=T)
        RWconvert<-rbind.data.frame(RWconvert, feat)
      }
     RWconvert[RWconvert==0]<-NA
     # hist(RWconvert$STRIDEDISTANCE)
      #hist(RWconvert$STRIDEDURATION)
      #selects the relevant days 
      RWconvert<-RWconvert %>% filter(WATCHSTART==paste(Meta$WATCHSTART[[lse]])) # relevant day
      RWconvert<-RWconvert[,c(1,4:8,12:24,28)]  
      
      
      #hourly -----
      if (Meta[lse,19]=="Hourly") {
          setwd("../")
        setwd("./C36_hourly")
      hrdir<-dir()
      HRconvert <- fread(input = hrdir[1],sep2 = ";", header=T)
        #Append the rest
        #problem is this loop
        for (i in 2:length(hrdir)) { 
      feat2 <- fread(input = hrdir[i],sep2 = ";",  header=T)
          HRconvert<-rbind.data.frame(HRconvert, feat2)
        }
HRconvert$date<-substr(HRconvert$WATCHSTART,start=1,stop=10)
  HRconvert<-HRconvert[HRconvert$date==substr(Meta$WATCHSTART[lse],start=1,stop=10)] 
  HRconvert$hour<-substr(HRconvert$WATCHSTART,start=12,stop=13)
  Night1<-HRconvert[HRconvert$hour>19,] 
  Night2<-HRconvert[HRconvert$hour<4,] 
  Night<-rbind(Night1,Night2)    
  Day<-HRconvert[HRconvert$hour>3,]
  Day<-HRconvert[Day$hour<20,]
  Day<-Day %>% group_by(UNITID)%>% summarise(DayM= mean(ACTIVITY))
  
  Night<-Night %>% group_by(UNITID)%>% summarise(NightM= mean(ACTIVITY))
  DayNightRatio<-Day$DayM/Night$NightM
  
  DayNightRatio<-(mean(Day$ACTIVITY)/mean(Night$ACTIVITY))
  plot(HRconvert$hour,HRconvert$STRIDEDISTANCE)
  hist(HRconvert$STRIDEDISTANCE)
  
 HRconvert<-HRconvert[HRconvert$STRIDEDISTANCE>70,]
 HRconvert<-HRconvert[HRconvert$STRIDEDISTANCE<200,]
 
 hist(HRconvert$STRIDEDURATION)  
 HRconvert<-HRconvert[HRconvert$STRIDEDURATION>1300,]
 HRconvert<-HRconvert[HRconvert$STRIDEDURATION<2600,]
 
 STRDIST<-HRconvert %>% group_by(UNITID)%>% summarise(StrideDistance2= mean(STRIDEDISTANCE),StrideDuration2= mean(STRIDEDURATION))
 RWconvert <-left_join(RWconvert,STRDIST,"UNITID")
 RWconvert$STRIDEDISTANCE<-RWconvert$StrideDistance2
 RWconvert$STRIDEDURATION<-RWconvert$StrideDuration2
 RWconvert<-RWconvert[,1:(ncol(RWconvert)-2)]
       } else {}
      SumData <-left_join(Results,RWconvert,"UNITID")
      
      #Mutate Summary Data ----
      #names(Results)
      healthyDF<- SumData[(SumData$loco== "0"),]
      healthy_Swing_PC<-mean(healthyDF$M_sPCdiff)
      SumData$SDiffSampleHealthy<- sqrt((healthy_Swing_PC-SumData$M_swingPC)^2)
      SumData$Speed<-((SumData$STRIDEDISTANCE/SumData$STRIDEDURATION*10))
      print("Mutate done")
      
      #### LSE Correlation ----
      # Merge with later on
      MODDF<- SumData[,3:ncol(SumData)] # Numeric only
      DFCor<- as.data.frame(as.matrix(cor(MODDF,use = "pairwise.complete.obs")))
      DFCor$Scale<-as.numeric(sqrt((DFCor[,1])^2))
      
      DFCor2<- DFCor[,c(1,ncol(DFCor))]
      DFCor3<-rownames_to_column(DFCor2)
      DFCor3<-DFCor3 %>% arrange(desc(Scale))
      print("Correlation done")
      
      #### LSE Summary Table----
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
      MODDF0<- MODDF %>% filter (loco==0)
      
      Table0<-t(as.data.frame(map(MODDF0,.f = (mean.sd))))
      MODDF1<- MODDF %>% filter (loco==1)
      Table1<-t(as.data.frame(map(MODDF1,.f = (mean.sd))))
      MODDF2<- MODDF %>% filter (loco==2)
      Table2<-t(as.data.frame(map(MODDF2,.f = (mean.sd))))
      
      SumTab<-cbind.data.frame(Table0,Table1,Table2)
      print ("Table ready")
      #Adding to experimetents / events / p
      #LongList[[lse]]<-long # this contains all the steps identified
<<<<<<< HEAD
      SumDataList[[lse]]<-MODDF
      SumTabList[[lse]]<-SumTab
      # END LSE loop ----
    }
=======
      CorList[[lse]]<- DFCor3
      SumDataList[[lse]]<-MODDF
      SumTabList[[lse]]<-SumTab
      # Models generated and stored 
      #R^2 stored
      # END LSE loop ----
      print("LSE")
      print(lse)
    }
    #LongList
    #SumDataList[1]
    #Model 
  }
  getOption("max.print")
  tail(SumDataList[[2]])
  #Change variables ----
  #Need to remove extra jersey from 2nd scoring
  seven<-as.data.frame(SumDataList[[2]]) 
  seven<-seven[-4,] # wasn't scored first time
  SumDataList[[8]] <- SumDataList[[1]]-seven
  
  SumDataList[[9]]<-(as.data.frame(SumDataList[4])-as.data.frame(SumDataList[5]))  
  
  SumDataList[[10]]<-as.data.frame(SumDataList[6])-as.data.frame(SumDataList[7])
  
  #Correlation for changed data
  
  
}# end of loop - Results begin ----
  
  #Summary table - mean, IQ, RMS or SD, Highest group, lowest. 
  #variables
  # Include correlations here - or at the end? Include at all? no should be in exploratory study. 
  # Activity - most like past published variable
  #Step Duration 
  
  {dfResults<-SumDataList[[1]]
  dfResults$LSE<-1
  for (i in 2:7) {
    SumData<-SumDataList[[i]]
    #names(SumData)
    SumData$LSE<-i
    dfResults<-rbind(dfResults,SumData)
  }
  
  dfResults$SwingPCdiffRef<- (dfResults$M_swingPC-0.353)^2
  #source and date?
  
  mact<-dfResults %>% group_by(LSE)%>% summarise(Act=mean(ACTIVITY)) # Column `LSE` can't be modified because it's a grouping variable
  
  dfResults<-left_join(dfResults,mact,by = "LSE")
  dfResults$AdjActivity<-dfResults$ACTIVITY-dfResults$Act
  dfResults0<-dfResults[dfResults$loco=="0",]
  dfResults1<-dfResults[dfResults$loco=="1",]
  dfResults2<-dfResults[dfResults$loco=="2",]
  }
  
  #pool changes in locomotion - doesn't make sense - differing environments  - different management?
  
  changePool<-SumDataList[[8]]
  changePool<-rbind(changePool,SumDataList[[9]])
  changePool<-rbind(changePool,SumDataList[[10]])
  
  
  long$uni<-paste(long$lse,long$UNITID)
  unique(long$uni)# 105 records
  
  #Change correlations ----
  for (change in 8:10) {
    MODDF<-SumDataList[[change]]
    DFCor<- as.data.frame(as.matrix(cor(MODDF,use="pairwise.complete.obs"),method="spearmann")) 
    #Spearman ----
    DFCor$Scale<-as.numeric(sqrt((DFCor[,1])^2))
    DFCor2<- DFCor[,c(1,ncol(DFCor))]
    DFCor3<-rownames_to_column(DFCor2)
    CorList[[change]]<- DFCor3
  }
  
  
  inspCor<-as.data.frame(CorList[1])
  inspCor2<-as.data.frame(CorList[2])
  inspCor3<-as.data.frame(CorList[3])
  inspCor4<-as.data.frame(CorList[4])
  inspCor5<-as.data.frame(CorList[5])
  inspCor6<-as.data.frame(CorList[6])
  inspCor7<-as.data.frame(CorList[7])
  inspCor8<-as.data.frame(CorList[8])
  inspCor9<-as.data.frame(CorList[9])
  inspCor10<-as.data.frame(CorList[10])
  
  inspCorJ<-left_join(inspCor,inspCor2,"rowname")
  inspCorJ<-left_join(inspCorJ,inspCor3,"rowname")
  inspCorJ<-left_join(inspCorJ,inspCor4,"rowname")
  inspCorJ<-left_join(inspCorJ,inspCor5,"rowname")#error
  inspCorJ<-left_join(inspCorJ,inspCor6,"rowname")
  inspCorJ<-left_join(inspCorJ,inspCor7,"rowname")
  inspCorJ<-left_join(inspCorJ,inspCor8,"rowname")
  inspCorJ<-left_join(inspCorJ,inspCor9,"rowname")
  inspCorJ<-left_join(inspCorJ,inspCor10,"rowname")
  
  # inspCorJ$Average <-(inspCorJ$loco.x+inspCorJ$loco.y+inspCorJ$loco.x.x+inspCorJ$loco.y.y#2
  # +inspCorJ$loco.x.x.x+inspCorJ$loco.y.y.y #3
  # +inspCorJ$loco.x.x.x.x+inspCorJ$loco.y.y.y.y#4
  # +inspCorJ$loco.x.x.x.x.x+inspCorJ$loco.y.y.y.y.y#5
  # )/10
  
  
  inspCorJ$AbsCor <- (inspCorJ$loco.x+inspCorJ$loco.y+inspCorJ$loco.x.x+inspCorJ$loco.y.y +inspCorJ$loco.x.x.x+inspCorJ$loco.y.y.y +inspCorJ$loco.x.x.x.x)/7
  
  # To do implement weighted average - length sumdata list 
  inspCorJ$ChangeCor<- (inspCorJ$loco.y.y.y.y+ inspCorJ$loco.x.x.x.x.x+inspCorJ$loco.y.y.y.y.y)/3
  
  #inspCorJ<-inspCorJ[,-c(2,4,6,8,10,12,14,16,18,20)] with / without sign
  #names(inspCorJ[,-(c(2,4,6,8,10,12,14,16,18,20)+1)])
  inspCorJ<-inspCorJ[,-(c(2,4,6,8,10,12,14,16,18,20)+1)]
  
  
  #inspCorJ$AbCor2<-sqrt(inspCorJ$AbsCor^2)
  #inspCorJ$ChangeCor2 <-sqrt(inspCorJ$ChangeCor^2)
  colnames(inspCorJ)<-c("Variable",
                        "Jerseysa","Jerseysb", 
                        "BW17", "BW18a","BW18b",
                        "Farma", "Farmb",
                        "Jersey Change","BW18 Change", "Farm Change",
                        "Average Cor","Av Change Cor"
                        #,"Absol Cor2","Change Cor2"
  )

}

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

#Accell pooled absolute
t.test(dfResults0$V_totalAct,dfResults2$V_totalAct)
t.test(dfResults0$Xmean,dfResults2$Xmean)
#accell pooled change 

cor.test(changePool$loco, changePool$V_totalAct,method="spearman")

cor.test(changePool$loco, changePool$Ymean,method="spearman")

#Activity measures----
#pooled
t.test(dfResults0$TASumcow,dfResults2$TASumcow)
t.test(dfResults0$ACTIVITY,dfResults2$ACTIVITY)
t.test(dfResults0$AdjActivity,dfResults2$AdjActivity) # not significant

cor.test(dfResults$loco,dfResults$ACTIVITY)

cor.test(dfResults$loco,dfResults$AdjActivity)


#### - Duration variables----
RWStepDuration<-dfResults %>% group_by(loco,LSE) %>%  summarise(meanDur=mean(STRIDEDURATION))
write.csv(x = RWStepDuration,file = "RWStepDuration.csv")

StepDuration<-dfResults %>% group_by(loco,LSE) %>%  summarise(meanDur=mean(M_duration))
write.csv(x = StepDuration,file = "StepDuration.csv")

ListDur<-c("M_duration", "V_duration", "STRIDEDURATION" ,"STRIDEDISTANCE" , "STRIDEMOVERATIO", "nsteps" ,"Speed","STRIDES")
DurCorLSE<- arrange(inspCorJ[inspCorJ$Variable %in% ListDur,],Variable)

setwd(home)
setwd("./Tables" )
write.csv(DurCorLSE,file = "DurCorLSE.csv" )

mean(long$duration)
mean(dfResults$M_duration)
mean(dfResults$STRIDEDURATION)
cor.test(dfResults$loco,dfResults$STRIDES)
cor.test(dfResults$loco,dfResults$STRIDEDURATION)
cor.test(dfResults$loco,dfResults$STRIDEDISTANCE)


#Swing ----

SwingSummary<-dfResults %>% group_by(loco,LSE) %>%  summarise(meanSwing=mean(M_SwingDur),
                                                              VarSwing=mean(V_SwingDur), 
                                                              meanSwingPC=mean(M_swingPC),
                                                              VarSwingPC=mean(V_swingPC))

setwd(home)
setwd("./Tables" )  

write.csv(x = SwingSummary,file = "SwingSummaryRW.csv")


inspCorJ$Variable
ListSwing<-c("V_SwingDur", "V_swingPC","M_SwingDur", "M_swingPC")
SwingCorLSE<- arrange(inspCorJ[inspCorJ$Variable %in% ListSwing,],Variable)

setwd(home)
setwd("./Tables" )
write.csv(SwingCorLSE,file = "SwingCorLSE.csv" )


#Absolute Novel ----
ListAbsNov<-c("M_dy1", "M_dy2", "M_y","M_dym1","V_dym8","M_dzm5","V_dy3")
AbsNovLSE<- arrange(inspCorJ[inspCorJ$Variable %in% ListAbsNov,],Variable)

setwd(home)
setwd("./Tables" )
write.csv(AbsNovLSE,file = "AbsNovLSE.csv" )

#Change Novel ----
cor.test(dfResults$loco,dfResults$V_dym5)
ListChaNov<-c("V_dym5","M_xm7","Ymean","V_totalAct")
ChanNovLSE<- arrange(inspCorJ[inspCorJ$Variable %in% ListChaNov,],Variable)

setwd(home)
setwd("./Tables" )
write.csv(ChanNovLSE,file = "ChanNovLSE.csv" )

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

dfResults %>% group_by(LSE) %>%  summarise(meanDur=mean(STRIDEDURATION)) ????
  
  dfResults %>% group_by(loco) %>%  summarise(meanDur=mean(STRIDEDURATION))
mean(dfResults$STRIDEDURATION)

t.test(dfResults[1:37,]$STRIDEDURATION,dfResults[38:105,]$STRIDEDURATION)

t.test(dfResults[1:37,]$M_duration,dfResults[38:105,]$M_duration)

t.test(dfResults[1:37,]$V_duration,dfResults[38:105,]$V_duration)

mean(dfResults$M_duration)
mean(dfResults0$M_duration)
mean(dfResults1$M_duration)
mean(dfResults2$M_duration)


#Swing Pc% ----

dfResults %>% group_by(LSE) %>%  summarise(Swing=mean(M_SwingDur))

dfResults %>% group_by(LSE) %>%  summarise(Swing=mean(M_swingPC))

dfResults %>% group_by(loco) %>%  summarise(Swing=mean(M_swingPC))




dfResults %>% group_by(LSE) %>%  summarise(Swing=mean(V_SwingDur))

dfResults %>% group_by(LSE) %>%  summarise(Swing=mean(V_swingPC))

dfResults %>% group_by(loco) %>%  summarise(Swing=mean(V_swingPC))

mean(dfResults$M_SwingDur)
mean(dfResults$M_swingPC)
mean(dfResults$SEM_SwingPC)
mean(dfResults$V_swingPC)

#Score 0
mean(dfResults0$M_duration)
mean(dfResults0$M_swingPC) 
SEM(dfResults0$M_swingPC)

#score 1
mean(dfResults1$M_swingPC)
SEM(dfResults1$M_swingPC)

#Lame
mean(dfResults2$M_duration)
mean(dfResults2$M_swingPC)
SEM(dfResults2$M_swingPC)

#Speed----
inspCorJ$Variable
ListSel<-c(
)
AccelCorLSE<- arrange(inspCorJ[inspCorJ$Variable %in% ListSel,],Variable)

setwd(home)
setwd("./Tables")
write.csv(AccelCorLSE,file = "AccelCorLSE.csv")


library(ggplot2)

dfResults$locoF<-as.factor(dfResults$loco)

b1<- ggplot(dfResults,aes(x=locoF,y=M_swingPC))+
  geom_boxplot()
b1

long$locoF<-as.factor(long$loco)
b1a<- ggplot(long,aes(x=locoF,y=swingPC))+   geom_boxplot()
b1a

#Check
b2<- ggplot(dfResults,aes(x=locoF,y=dfResults$SEMSwingPC))+   geom_boxplot()
b2

hist(dfResults$M_swingPC)
hist(long$swingPC) # bi modal
#Correlation - n assocation
cor.test(long$loco,long$swingPC)
cor.test(long$loco,long$sPCdiff) # signficant but small
plot(long$loco,long$swingPC)
cor.test(dfResults$loco,dfResults$M_swingPC)
cor.test(dfResults$loco,dfResults$M_sPCdiff)
cor.test(dfResults$loco,dfResults$V_swingPC)
cor.test(dfResults$loco,dfResults$SEM_SwingPC)

t.test(dfResults0$M_swingPC,dfResults2$M_swingPC)
t.test(dfResults0$V_swingPC,dfResults2$V_swingPC)
t.test(dfResults0$SwingPCdiffRef,dfResults2$SwingPCdiffRef)
t.test(dfResults0$SEM_SwingPC,dfResults2$SEM_SwingPC)


#Beer ----
#Beer reported that a logistic regression with speed and standing bouts could discern healthy and non healthy
#Used the average of 2 of 3 recorded days.

#They had 2 accelerometers per cow - chose to use one - randomly
#ROC

cor.test(dfResults$loco,dfResults$Speed#,method='spearman') # signficant with pearson  - not spearman
)
cor.test(dfResults$loco,dfResults$STRIDEDURATION,method='spearman'
) # signficant with pearson  - not spearman
cor.test(dfResults$loco,dfResults$STRIDEDISTANCE, method='spearman')

cor.test(dfResults$loco,dfResults$STANDUP, method='spearman')
cor.test(dfResults$loco,dfResults$LAYINGCOUNTER, method='spearman')
cor.test(dfResults$loco,dfResults$STANDINGINDEX, method='spearman')
cor.test(dfResults$loco,dfResults$LAYTIME, method='spearman')

t.test(dfResults0$Speed,dfResults2$Speed)
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


#Select POOL of Results----

#So here we can want to pool certain subsets
#Interestingly when We look at  the summary for each cow, - the average, we get stronger correlations than for all the pooled steps. This means the number of steps which is broadly correlated also. 
#Correlations are higher within group as the baseline is established
# Strongest correlations accross all 7 are
#V_dzm5 -0.20
#WALKTIME -0.19
#M_xm5 -0.18
#Average correlations within LSEs much higher. 

norepeats<-c(1,3,5,7)
norepeats2<-c(1,2,4,6) # better results?

#PoolResults<-SumDataList[[8]]
{
  PoolResults<-SumDataList[[1]]
  for (j in 2:7){ # with repeats
    PoolResults<-rbind.data.frame(PoolResults,SumDataList[[j]])
  }}

{
  PoolResults<-SumDataList[[8]]
  for (j in 9:10){ # changes
    # PoolResults<-SumDataList[[1]]
    
    PoolResults<-rbind.data.frame(PoolResults,SumDataList[[j]])
  }
  MODDF<- PoolResults # Numeric only?
  # Change here  selection of columns and loco score # maybe just cor to loco
  
  cor.test(PoolResults$loco, PoolResults$STRIDEDURATION)
  cor.test(PoolResults$loco, PoolResults$M_duration)
  cor.test(PoolResults$loco, PoolResults$V_duration)
  
  DFCor<- as.data.frame(as.matrix(cor(MODDF)))
  DFCor$Scale<-as.numeric(sqrt((DFCor[,1])^2))
  
  DFCor2<- DFCor[,c(1,ncol(DFCor))]
  PoolCor3<-rownames_to_column(DFCor2)
}
#V_dzm5 -0.20
#WALKTIME -0.19
#M_xm5 -0.18

#### Pool Model----
m1<-lm(loco~V_dzm5+PACTIVITYCHANGE,data = PoolResults)
summary(m1)


#Long Analysis ----
#names(long)
LongDF<-long[,c(-1,-55,-56)]
DFCor<- as.data.frame(as.matrix(cor(LongDF)))
DFCor$Scale<-as.numeric(sqrt((DFCor[,53])^2))
DFCor2<- DFCor[,c(53,ncol(DFCor))]
LongCor<-rownames_to_column(DFCor2)

#Typical step----
library(ggplot2); 
#So we have 11,000 row of nonlame, and 15000 lame steps.
# So I could do standard deviation for each row

Longm<-colMeans(LongNonLame[,c(16:7,2,17:20),]) # -10 to +4
Longindex<-c(-10:4)
Longv<-apply(LongNonLame[,c(16:7,2,17:20)],2,FUN = var)
Longsum<-cbind.data.frame(Longm,Longv,Longindex)
p <- ggplot(data = Longsum, aes(x=Longindex,
                          y= Longm)) +
   geom_line(colour = "gray") +
  geom_errorbar(aes(ymin=Longm-(Longv/2), 
                    ymax=Longm+(Longv/2)), colour = "gray")
p
?geom_line()
LongNonLame<-long[long$loco=="0",]
LNLm<-colMeans(LongNonLame[,c(16:7,2,17:20),]) # -10 to +4
LNLindex<-c(-10:4)
LNLv<-apply(LongNonLame[,c(16:7,2,17:20)],2,FUN = var)
LNL<-cbind.data.frame(LNLm,LNLv,LNLindex)

p<- p+  geom_line(data = LNL,show.legend = T,aes(x=LNLindex,y=LNLm),colour="black") +
         geom_errorbar(aes(ymin=LNLm-(LNLv/2), 
                   ymax=LNLm+(LNLv/2)))
p

#Lame ----
LongLame<-long[long$loco=="2",]

LLm<-colMeans(LongLame[,c(16:7,2,17:20),])
LLindex<-c(-10:4)
LLv<-apply(LongLame[,c(16:7,2,17:20)],2,FUN = var)

LL<-cbind.data.frame(LLm,LLv,LLindex)

p + geom_line() +
  geom_errorbar(aes(ymin=(LLm-(LLv/2)), 
                    ymax=(LLm+(LLv/2))))




#  changes ??? ----
# for (change in 8:10) {
#   MODDF<-SumDataList[[change]]
#   MODDF0<- MODDF %>% filter (loco==0)
#   MODDFm2<- MODDF %>% filter (loco==-2)
#   Tablem2<-t(as.data.frame(map(MODDF2,.f = (mean.sd))))
#   
#   MODDFm1<- MODDF %>% filter (loco==-1)
#   Tablem1<-t(as.data.frame(map(MODDF1,.f = (mean.sd))))
#  
#   Table0<-t(as.data.frame(map(MODDF0,.f = (mean.sd))))
#   
#   MODDF1<- MODDF %>% filter (loco==1)
#   Table1<-t(as.data.frame(map(MODDF1,.f = (mean.sd))))
#   MODDF2<- MODDF %>% filter (loco==2)
#   Table2<-t(as.data.frame(map(MODDF2,.f = (mean.sd))))
#   
#   
# SumTab<-cbind.data.frame(Tablem2,Tablem1,Table0,Table1,Table2) 
#   
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b
