# This is script 2/5 the scripts for the behavior studies. 
# It produces that data required for table 4 of Paper 1 - 24 hour summaries of behaviour correlation to mobility score. 

# Start-----
{ 
  { 
    home<- "C:/Users/olearyn2/OneDrive - Lincoln University/Lameness/RW_Acceleration_and_Behavior" 
    # Location of Lameness files on your computer - data available from Nialloleary@gmail.com

    library(dplyr); 
    library(data.table);
    library(tibble);
    library("Hmisc")

    # Meta Table ----
    # This table contains the meta data for locomotion scoring event required to run the script. 
    # This facilitate same script being applied to each data set. 
    # Only these variables should change between locmotion scoring events. 
    # The Selected row corresponds to the locomotion scoring event (lse). 
    # In the script, then the item from that row is called. 
  
    meta_column_names <- c( #Meta variables 
      "Folder",  
      "WATCHSTART",
      "Feature path",
      "Loco_Index",
      "ExclCow",
      "Exclcows2",
      "Exclcows3",
      "ExclCosws4",
      "ExclCows5",
      "ExclCows6",
      "ExclCows7",
      'JoinBy', 
      "LSE"
    )
  
    # Jersey trial - First scoring (a) 
    ja <- c( 
      './Jerseys', 
      '02.06.2017 00:00',  
      "Loco0106",
      2,
      40,
      40,
      40,
      40,
      40,
      40,
      40, 
      'UNITID'
    )

    jb <- c(
      './Jerseys',
      '13.06.2017 00:00',   
      "Loco1506",
      3,
      40,
      40,
      40,
      40,
      40,
      40,
      40, 
      'UNITID'
    )

    # Dairygold 2017(Black & white herd)     
    BW17 <- c(
      './DGHF2017', 
      '17.06.2017 00:00',  
      "LocoScore150617",
      4,
      40,
      40,
      40,
      40,
      40,
      40,
      40, 
      'UNITID'
    ) 
  
    #DairyGold 2018 (Black & white herd)
    BW18a <- c( 
      './DGHF2018', 
      '10.08.2018 00:00', 
      "Loco080818",
      2,
      11, # data doesn't contain walking, SN00017FFD
      13, #SN00018D41 #
      13, #SN0001932C # Faulty
      15,#SN00018DD5 # Faulty
      40,
      40,
      40, 
      'Ped'  
    )
  
    BW18b <- c(
      './DGHF2018',
      '13.08.2018 00:00', 
      "Loco130818", 
      4,
      11, # SN00017FFD
      13, #SN00018D41
      13, #SN0001932C
      15,#SN00018DD5 
      40,
      40,
      40, 
      'Ped'   
    )
  
    # Commerical farm 
    Farma <-c ( 
      './Commercial_Farm',
      '17.08.2018 00:00', 
      "Loco160818",
      2, 
      1, #SN00018E33 no loco score, not attached
      1, # SN0001932C
      3, #SN000192D9
      7,
      7,
      7,
      12, 
      'Ped'
    )# out by a day SN00018D79
               
    Farmb <-c(
      './Commercial_Farm', 
      '20.08.2018 00:00',
      "Loco200818",
      4,  
      1, #SN00018E33 no loco score, not attached
      1, # SN0001932C - doesn't record all the way through
      3,#SN000192D9
      7,#SN00018DD5 - all lying
      7, 
      7, 
      12, 
      'Ped'
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
    CorrelationList <- numList <- pList <- SumDataList <- vector(
      mode ="list" ,
      (nrow(meta_data_frame)+3)
    )

    #Loop start----
    for (locomotion_scoring_event in 1:nrow(meta_data_frame)) { # Load selected locomotion scoring events
      print(c("Locomotion Scoring Event",locomotion_scoring_event))
      setwd(home) 
      setwd(as.character(meta_data_frame$Folder[[locomotion_scoring_event]]))
      setwd('./24Hourly')
      inde <- dir() # Index of file names 
      inde2 <- substr(inde,start = 35,stop=44 ) # Pedometer Serial numbers
      Results3 <- cbind.data.frame(inde,inde2)
      colnames(Results3) <-c ("inde","UNITID")
      #Files that didn't store data  check 
      # for making meta table 
      #Data that does not work - exclude

      #Excluded Records----
      for (m in 6:11){ 
            Exclude <- as.numeric(paste0(meta_data_frame[locomotion_scoring_event,m]))
            Results3 <- Results3[-Exclude,]
      }

      #Locomotion Scores and Reference table----
      setwd("../")
      Ref1 <- read.csv(
        file = 'PedRef.csv',
        sep = ",",
        header = T
      )
      Score <- read.csv(
        file =  'Score.csv',
        sep = ",",
        header = T
      )
      NUM <- as.numeric(paste0(meta_data_frame$Loco_Index[[locomotion_scoring_event]])) # Column with relevant loco score for this 
      Score$loco2 <- Score[,NUM] 
      Ref <- left_join(
        Score,
        Ref1,
        paste0(meta_data_frame$JoinBy[[locomotion_scoring_event]])
      )
      Results2 <- left_join(
        Results3,
        Ref,
        "UNITID"
      )
      Results2$inc <- is.na(Results2$loco2) 
      Results <- Results2 %>% filter(inc== F) # don't load
      Results <- Results[,-(ncol(Results))] #remove inc
      Results$loco <- Results$loco2
      Results <- Results[,c(1,2,ncol(Results))] 

      {    ## 24 hour data-----
        setwd('./24Hourly')
        inde
        #load first & Initialise
        RWconvert <- fread(input = paste(Results[1,1]),sep2 = ";", header=T)
        #Append the rest
        for (i in 2:nrow(Results)) { # loads all the day records
          feat <- fread(
            input = paste(Results[i,1]),
            sep2 = ";",  
            header=T
          )
          RWconvert <- rbind.data.frame(RWconvert, feat)}

          RWconvert <- RWconvert %>% 
            filter(WATCHSTART==paste(
              meta_data_frame$WATCHSTART[[locomotion_scoring_event]])) # relevant day
          RWconvert[RWconvert==0]<-NA

          #Variable Select----
          RWconvert <- RWconvert[,c(1,4:8,12:20)] 
          #assign to a list
          MODDF <- left_join(Results,RWconvert,'UNITID')   
          MODDF <- MODDF[,c(-1,-2)]
          SumDataList[[locomotion_scoring_event]]<-MODDF

          #### Correlation within trial ----
          # rcorr creates a list of 3 with 1 - Correlation matrix r, 2 n and 3 p values

          CorMat <- rcorr(as.matrix(MODDF),type = 'spearman')

          #r
          DFCor <- rownames_to_column(as.data.frame(CorMat[1]))
          CorrelationList[[locomotion_scoring_event]]<-as.data.frame(DFCor[,1:2])

          #n - values
          DFCor <- rownames_to_column(as.data.frame(CorMat[2]))
          numList[[locomotion_scoring_event]]<-as.data.frame(DFCor[,1:2])

          #p-values
          DFCor <- rownames_to_column(as.data.frame(CorMat[3]))
          pList[[locomotion_scoring_event]] <- as.data.frame(DFCor[,1:2])
          
          print("locomotion_scoring_event")
          print(locomotion_scoring_event)
      }
    }

    #Start & end of trial change variables ----
    #Need to remove extra jersey from 2nd scoring
    JerseyB <- as.data.frame(SumDataList[[2]]) 
    JerseyB <- JerseyB[-4,] # Cow wasn't scored first time
    SumDataList[[8]] <- SumDataList[[1]]-JerseyB  #Jersey change
    SumDataList[[9]] <- (
      as.data.frame(SumDataList[4])-as.data.frame(SumDataList[5])
    )  #BW2018 Change
  
    SumDataList[[10]] <- as.data.frame(SumDataList[6])-as.data.frame(SumDataList[7]) #Commercial farm change

    #Change correlations ----

    for (locomotion_scoring_event in 8:10) {
      #Duplication of code
      CorMat <- rcorr(x = as.matrix(SumDataList[[locomotion_scoring_event]]),type = 'spearman')
          
      DFCor <- rownames_to_column(as.data.frame(CorMat[1])) #r
      CorrelationList[[locomotion_scoring_event]] <- as.data.frame(DFCor[,1:2])
              
      DFCor <- rownames_to_column(as.data.frame(CorMat[2])) #n - values
      numList[[locomotion_scoring_event]] <- as.data.frame(DFCor[,1:2])
              
      DFCor <- rownames_to_column(as.data.frame(CorMat[3])) #p-values
      pList[[locomotion_scoring_event]] <- as.data.frame(DFCor[,1:2])
    }
  }

  #Results begin ----
  #Combine key columns from the lists created above into summary table
  CorrelationListA <- as.data.frame(CorrelationList[1])
  for (i in 2:10){
    CorrelationListA <- left_join(
      CorrelationListA,as.data.frame(CorrelationList[[i]]),
      "rowname"
    )
    colnames(CorrelationListA)[ncol(CorrelationListA)]<-ncol(CorrelationListA)-1 
  }

  numListA <- as.data.frame(numList[1]) # N per sample
  for (i in 2:10){
    numListA <- left_join(
      numListA,
      as.data.frame(numList[[i]]),
      "rowname"
    )

    colnames(numListA)[ncol(numListA)] <- ncol(numListA)-1 } #number the columns

    #P-value summary table ----
    pListA <- as.data.frame(pList[1])

    for (i in 2:10){
      pListA <- left_join(
        pListA,
        as.data.frame(pList[[i]]),
        "rowname"
      )

      colnames(pListA)[ncol(pListA)] <- ncol(pListA)-1 
    }
   
    CorrelationListA$AverageAbsoluteCorrelation <- rowMeans(CorrelationListA[,2:8])
    CorrelationListA$AverageChangeCorrelation <- rowMeans(CorrelationListA[,9:11])

    CorrelationListA[nrow(CorrelationListA)+1,1] <- 'n'
    CorrelationListA[nrow(CorrelationListA),2:11] <- sapply(X = numListA[2:11],FUN = max)
    #Round & Name  

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
    CorrelationListC <- replace(CorrelationListB,CorrelationListB<0.2 & 
                                  CorrelationListB > -0.2,NA)


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
      "BW18 Change", 
      "Farm Change",
      "Average Cor",
      "Av Change Cor"
    )
    summary_table_4 <- CorrelationListC[order(CorrelationListC$Variable,decreasing = F),]
    plstSig <- pListA
    plstSig[pListA>0.2] <- NA
    plstSig$rowname <- pListA$rowname

    #Write Behaviour correlation Table
    setwd('../')
    setwd('../')
}# Outermost

write.csv(x = summary_table_4,file = "Table4.csv")
write.csv(x = plstSig,file = "Table5Pvals.csv")
#P Values - Manually add in stars for the few that are significant.
