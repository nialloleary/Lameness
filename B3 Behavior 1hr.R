#This is script 3/5 of the scripts for the lameness studies. It produces table summarising correlations with day and night total summaries of behaviour. This tableis not presented in the paper as the results are not significant.
#Start-----
{ 
<<<<<<< HEAD
{ 
 home<- "C:/Users/olearyn2/OneDrive - Lincoln University/Lameness/RW_Acceleration_and_Behavior" 
<<<<<<< HEAD:B3 Behavior 1hr.R
# locotion of Lameness files on your computer - data available from Nialloleary@gmail.com
=======
# locoation of Lameness files on your computer - data available from Nialloleary@gmail.com
>>>>>>> 339eb7c825395d748117a6ff4aaa64f90944e29b:L2 Behavior 1hr.R

library(dplyr); library(data.table);library(tibble);library("Hmisc")


#Meta Table ----
#This table contains the meta data for locomotion scoring event required to run the script. This facilitate same script being applied to each data set. Only these variables should change between locmotion scoring events. The Selected row corresponds to the locomotion scoring event (lse). In the script, then the item from that row is called. 
=======
  { 
    home<- "C:/Users/olearyn2/OneDrive - Lincoln University/Lameness/RW_Acceleration_and_Behavior"
    # Locoation of Lameness files on your computer - data available from Nialloleary@gmail.com

    library(dplyr); 
    library(data.table);
    library(tibble);
    library("Hmisc")


    # meta_data_frame Table ----
    # This table contains the meta_data_frame data for locomotion scoring event required to run the script. 
    # This facilitate same script being applied to each data set. 
    # Only these variables should change between locmotion scoring events. 
    # The Selected row corresponds to the locomotion scoring event (lse). 
    # In the script, then the item from that row is called. 
>>>>>>> code_clean_up
  
    meta_column_names <- c( #meta_data_frame variables "Feature path",
      "Folder",  
      "SerialStart",
      "SerialEnd",
      "Date",
      "LocoName", 
      "Loco_Index",
      "ExclCow",
      "ExclCows2",
      "ExclCows3",
      "ExclCosws4",
      "ExclCows5",
      "ExclCows6",
      "ExclCows7",
      "JoinBy", 
      "LSE"
    )
  
    #Jersey trial - First scoring (a) 
    ja <- c( 
      "./Jerseys",
      "35",
      "44", 
      "02",  
      "Loco0106",
      2,
      40,
      40,
      40,
      40,
      40,
      40,
      40, 
      "UNITID"
    )

    jb <- c(
      "./Jerseys",
      "35",
      "44",
      "13",   
      "Loco1506",
      3,
      40,
      40,
      40,
      40,
      40,
      40,
      40, 
      "UNITID"
    )

    # Dairygold 2017(Black & white herd)     
    BW17 <- c(
      "./DGHF2017", 
      "20",
      "29", 
      "17",  
      "LocoScore150617",
      4,
      40,
      40,
      40,
      40,
      40,
      40,
      40, 
      "UNITID"
    ) 
  
    #DairyGold 2018 (Black & white herd)
    BW18a <- c( 
      "./DGHF2018",
      "20",
      "29", 
      "10", 
      "Loco080818",
      2,
      11, # data doesn"t contain walking, SN00017FFD
      13, #SN00018D41
      13, #SN0001932C # also faulty at commercial farm
      15,#SN00018DD5 #4 cows Also didn"t work at commercial farm
      40,
      40,
      40, 
      "Ped"  
    )
  
    BW18b <- c(
      "./DGHF2018",
      "20",
      "29",
      "13", 
      "Loco130818", 
      4,
      11, # SN00017FFD
      13, #SN00018D41
      13, #SN0001932C
      15,#SN00018DD5 
      40,
      40,
      40, 
      "Ped"   
    )
  
    #Commerical farm 
    Farma <- c( 
      "./Commercial_Farm",
      "20",
      "29",
      "17", 
      "Loco160818",
      2, 
      40, #SN00018E33 no locomotion score, not attached
      40, # SN0001932C
      40, #SN000192D9
      40,
      40,
      40,
      40, 
      "Ped"
    )# out by a day SN00018D79
               
    Farmb <- c(
      "./Commercial_Farm",
      "20",
      "29", 
      "20",
      "Loco200818",
      4,  
      40, #SN00018E33 no loco score, not attached
      40, # SN0001932C
      40, #SN000192D9
      40,
      40,
      40,
      40, 
      "Ped"
    )# out by a day SN00018D79
               
    #LSE selection (Locomotion scoring event)
    meta_data_frame <- rbind.data.frame(
      ja, 
      jb,
      BW17, 
      BW18a,
      BW18b,
      Farma, 
      Farmb
    )
    meta_data_frame$locomotion_scoring_event <- c(
      "1.aJerseys",
      "1.bJerseys",
      "2.BW17", 
      "3.aBW18a",
      "3.bBW18b",
      "4.aFarma", 
      "4.bFarmb"
    )
    colnames(meta_data_frame) <- meta_column_names
  
    #Initialise lists to store results from each cohort
    CorrelationList <- numList <- pList <- SumDataList <- 
      DayDataList <- NightDataList <- vector(mode ="list" ,(nrow(meta_data_frame)+3))
    #Loop start----

    for (locomotion_scoring_event in 1:nrow(meta_data_frame)) {
      # Load selected locomotion scoring events
      print(c("Locomotion Scoring Event",locomotion_scoring_event))
      setwd(home) 
      setwd(as.character(meta_data_frame$Folder[[locomotion_scoring_event]]))
      setwd("./1Hourly")
      inde <- dir() # Index of file names 
      inde2 <- substr(
        inde,
        start = paste(meta_data_frame$SerialStart[[locomotion_scoring_event]]),
        stop= paste(meta_data_frame$SerialEnd[[locomotion_scoring_event]])
      ) # Pedometer Serial numbers
      Results3 <- cbind.data.frame(inde,inde2)
      colnames(Results3) <- c("inde","UNITID")
      #Files that didn"t store data  check # for making meta_data_frame table 
      #Data that does not work - exclude

      #Excluded Records----
      for (m in 7:13){ 
        Exclude<- as.numeric(paste0(meta_data_frame[locomotion_scoring_event,m]))
        Results3<-Results3[-Exclude,]
      }

      #Locomotion Scores and Reference table----
      setwd("../")
      Ref1 <- read.csv(
        file = "PedRef.csv",
        sep = ",",
        header = T
      )
      Score <- read.csv(
        file = "Score.csv",
        sep = ",",
        header = T
      )
      NUM <- as.numeric(paste0( # Column with relevant loco score for this 
        meta_data_frame$Loco_Index[[locomotion_scoring_event]])) 
           Score$loco2 <- Score[,NUM] 
      Ref <- left_join(
        Score,
        Ref1,
        paste0(meta_data_frame$JoinBy[[locomotion_scoring_event]])
      )
      Results2 <- left_join(Results3,Ref,"UNITID")#
      Results2$inc <- is.na(Results2$loco2) 
      Results <- Results2 %>% filter(inc== F) # don"t load
      Results <- Results[,-(ncol(Results))] #remove inc
      Results$loco <- Results$loco2
      Results <- Results[,c(1,2,ncol(Results))] 

      {    ## 1 hour data-----
        setwd("./1Hourly")
        #load first & Initialise
        RWconvert <- fread(
          input = paste(Results[1,1]),
          sep2 = ";", 
          header=T
        )
        #Append the rest
        for (i in 2:nrow(Results)) { # loads all the day records
          feat <- fread(
            input = paste(Results[i,1]),
            sep2 = ";",  
            header=T
          )
          RWconvert<-rbind.data.frame(
            RWconvert, 
            feat,
            fill=TRUE
          )
        }

        #Here is different from script 1, hourly data
        RWconvert$Date <- substr(
          x = RWconvert$WATCHSTART,
          start = 1,
          stop = 2
        )
        RWconvert <- RWconvert %>% 
          filter(Date==paste(
            meta_data_frame$Date[[locomotion_scoring_event]])) #Chosen day

        RWconvert$Hour <- as.numeric(
          substr(
            x = RWconvert$WATCHSTART,
            start = 12,
            stop = 13
          )
        )

        #RWconvert[RWconvert==0]<-NA
        #Between milking----
        DayTime <- c(10:15)
        Day <- RWconvert %>% filter(Hour %in% DayTime)
        DayDataList[locomotion_scoring_event]<-Day
  
        NightTime <- c(0:5,19:23)
        Night <- RWconvert %>% filter(Hour %in% NightTime)
        NightDataList[locomotion_scoring_event]<-Night
  
        DayResM <- Day %>% group_by(UNITID) %>% summarise_all(mean)
        DayResM <- DayResM[,c(1,4:8,12:20)]
        DayResV <- Day %>% group_by(UNITID) %>% summarise_all(var)
        DayResV <- DayResV[,c(1,4:8,12:20)]
  
        NightResM <- Night %>% group_by(UNITID) %>% summarise_all(mean)
        NightResM <- NightResM[,c(1,4:8,12:20)]
  
        NightResV <- Night %>% group_by(UNITID) %>% summarise_all(var)
        NightResV <- NightResV[,c(1,4:8,12:20)]

        #Variable Select----
  
        #assign to a list
        MODDF <- left_join(
          Results,
          DayResM,
          "UNITID"
        )   
        MODDF <- left_join(
          MODDF,
          DayResV,
          "UNITID"
        )   
        MODDF <- left_join(
          MODDF,
          NightResM,
          "UNITID"
        )  
        MODDF <- left_join(
          MODDF,
          NightResV,
          "UNITID"
        )   
  
        #x= day m, y= day variance, x.x = night mean, y.y = night variance
        #Split into hourly sub groups
        #Calculate 


        SumDataList[[locomotion_scoring_event]] <- MODDF
        MODDF <- MODDF[,c(-1,-2)]
  
        #### Correlation within trial ----
        # rcorr creates a list of 3 with 1 - Correlation matrix r, 2 n and 3 p values
  
        CorMat <- rcorr(as.matrix(MODDF),type = "spearman")
  
        #r
        DFCor <- rownames_to_column(as.data.frame(CorMat[1]))
        CorrelationList[[locomotion_scoring_event]] <- as.data.frame(DFCor[,1:2])
  
        #n - values
        DFCor <- rownames_to_column(as.data.frame(CorMat[2]))
        numList[[locomotion_scoring_event]] <- as.data.frame(DFCor[,1:2])
  
        #p-values
        DFCor <- rownames_to_column(as.data.frame(CorMat[3]))
        pList[[locomotion_scoring_event]] <- as.data.frame(DFCor[,1:2])
  
        print("LSE")
        print(locomotion_scoring_event)
      }
    }

    #Results begin ----
    #Combine key columns from the lists created above into summary table
    CorrelationListA <- as.data.frame(CorrelationList[1])
    for (i in 2:7){
      CorrelationListA <- left_join(CorrelationListA,as.data.frame(CorrelationList[[i]]),"rowname")
      #colnames(CorrelationListA)[ncol(CorrelationListA)]<-ncol(CorrelationListA)-1 
    }

    #P-value summary table ----
    pListA <- as.data.frame(pList[1])

    for (i in 2:7){
      pListA <- left_join(
        pListA,
        as.data.frame(pList[[i]]),
        "rowname"
      )

      colnames(pListA)[ncol(pListA)] <- ncol(pListA)-1 
    }
  
    pListA$AveragePVal <- rowMeans(pListA[,2:8])
    CorrelationListA$AverageAbsoluteCorrelation <- rowMeans(CorrelationListA[,2:8])
    CorrelationListA$SqrAverageAbsoluteCorrelation <- 
      sqrt(CorrelationListA$AverageAbsoluteCorrelation*
             CorrelationListA$AverageAbsoluteCorrelation)
    is.num <- sapply(CorrelationListA, is.numeric)
  
    CorrelationListB <- cbind(
      CorrelationListA[,1],
      as.data.frame(
        lapply(
          CorrelationListA[is.num],
          round,
          2
        )
      )
    )
  
    #Blank values of negligible size
    CorrelationListC <- replace(
      CorrelationListB,
      CorrelationListB<0.2 & CorrelationListB > -0.2,
      NA
    )
  
  
    colnames(CorrelationListC) <- c(
      "Variable", 
      "JerseysA",
      "JerseysB", 
      "BW17", 
      "BW18A",
      "BW18B",
      "FarmA", 
      "FarmB", 
      "Jersey Change",
      "BW18 Change"
    )
  
    plstSig<-pListA
    plstSig[pListA>0.2] <- NA
    plstSig$rowname <- pListA$rowname
  }
  #Write Behaviour correlation Table
  setwd("../")
  setwd("../")
}
# Outermost

write.csv(x = CorrelationListC,file = "TableNot in Paper1.csv")
write.csv(x = plstSig,file = "TablePvalsNot in Paper1.csv")
#P Values - Manually add in stars for the few that are significant.
