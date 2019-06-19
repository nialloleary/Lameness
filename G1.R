#This script summarises Acceleration only factors from the 10 hz data
#Goal Mean and Variance of each variable for all seven events Like summary table 2 of paper 1

#Package loading & Define Functions & set constants-----------
  { # 467
  home<- "C:/Users/olearyn2/OneDrive - Lincoln University/Lameness/RW_Acceleration_and_Behavior"  # location of Lameness file on your computer - data available from Nialloleary@gmail.com
  library(magrittr);library(dplyr); library(data.table); library(RcppRoll);  library(tibble);library(purrr);library(zoo);
  
  #Function mean and Standard deviation combined 
#  mean.sd <- function(x) c(mean = mean(x), sd = sd(x))
  #Constants
  Recs_per_Day<-24*60*60*10# records per day
  Records <- 600 # To be examined in the consistent walking analysis 60 seconds, 1 minute
  side<-Records/2 # when centred time before and after
  
#This Meta table contains the meta data for locomotion scoring event required to run the script. This facilitate same script being applied to each data set. Only these variables should change between locmotion scoring events. The Selected row corresponds to the locomotion scoring event (lse). In the script, then the item from that row is called. 
  
  MVars<-c("Raw_Path", #Meta variables
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
            1, #SN00018E33 no loco score, not attached
            1, # SN0001932C - doesn't record all the way through
            3,#SN000192D9
            7,#SN00018DD5 - all lying
            7,
            7,
            12# out by a day SN00018D79
            ,"Ped"
            ) 
  
  #LSE selection (Locomotion scoring event)
  Meta<-rbind.data.frame(ja, jb,BW17, BW18a,BW18b,Farma, Farmb)
  Meta$LSE<-c("1.JerseysA", "2.JerseysB","3.BW17", "4.BW18a","5.BW18b","6.Farma", "7.Farmb")
  colnames(Meta)<-MVars
  
  #initialise lists to store results from each cohort
  #LongList<-vector(mode ="list" ,nrow(Meta))
    SumTabList<-vector(mode ="list" ,(nrow(Meta)+3))
  SumDataList<-vector(mode ="list" ,(nrow(Meta)+3))
  
  
  ##### Long loop start 11+ minutes----
  for (lse in 1:nrow(Meta)) { # Load selected locomotion scoring events
    print(c("Locomotion Scoring Event",lse))
   
    setwd(home) 
    # Raw xyz data folder
    setwd(as.character(Meta$Raw_Path[[lse]]))
    setwd("./10Hz")
    inde<-dir() # Index of file names 
    inde2<-substr(inde,start = paste(Meta$Start[[lse]]),stop= paste(Meta$Stop[[lse]])) # Pedometer number& Serial numbers
    
    Results<-cbind.data.frame(inde,inde2)
    colnames(Results)<-c("inde","UNITID")
    names(Meta)
    for (m in 11:17){ 
      Exclude<- as.numeric(paste0(Meta[lse,m]))
      Results<-Results[-Exclude,]
    }
    ##### Event Ref table knit together 
    setwd("../")
    Ref1<-read.csv(file =  paste(Meta$Ref_File[lse]) ,sep = ",",header = T)
    Score<-read.csv(file =  paste(Meta$Scores[lse]) ,sep = ",",header = T)# 
    NUM <-as.numeric(paste0(Meta$Loco_Index1[[lse]])) # Column with relevant loco score for this 
    Score$loco2<-Score[,NUM] #  to fix bug with Jereseys
    Ref<-left_join(Score,Ref1,paste0(Meta$JoinBy[[lse]])) #breaks at 4
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
      setwd(as.character(Meta$Raw_Path[[lse]]))
      setwd("./10Hz")
      # select data set 
      
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
      
      
      cow<-cow[(as.numeric(side)):(nrow(cow)-side),] 
     
      #Make sure x is forward 
      invert<-mean(cow$x)<0
      print(invert)
      
      if (invert){ # if x is less than 0 - facing backwards
        cow$x<-((cow$x)*(-1)) # So invert values 
      }
    
      #mean & variance----
      #All the mean values
    
      Results$Xmean[i]<-mean(cow$x)
      Results$Ymean[i]<-mean(cow$y)
      Results$Zmean[i]<-mean(cow$z)
      
      Results$XSum[i]<-sum(cow$x)
      Results$YSum[i]<-sum(cow$y)
      Results$ZSum[i]<-sum(cow$z)

    SumDataList[lse]<-Results
      
    }
  }
  }
  #  names(Results)<-c("inde","UNITID","loco","nsteps","nsteps3","Xmean","Ymean", "Zmean","Xsum","Ysum", "Zsum")

    # End of XYZ data loop
 

   {

            #### LSE Summary Table----
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
      SumDataList[[lse]]<-MODDF
      SumTabList[[lse]]<-SumTab
      # END LSE loop ----
    }
