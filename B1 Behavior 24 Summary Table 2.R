#This is script 1 of the scripts for the lameness studies. It produces that data required for table 4 of Paper 1 - 24 hour summaries of behaviour. 

{ 
  { 
    home<- "C:/SourceCode/Lameness/Data/RW_Acceleration_and_Behavior" 
    # Location of Lameness files on your computer - data available from Nialloleary@gmail.com

    library(dplyr); 
    library(data.table);
    library(tibble);
    library("Hmisc")

    # meta_data_frame Table ----
    # This table contains the meta data for locomotion scoring event required to run the script. 
    # This facilitate same script being applied to each data set. 
    # Only these variables should change between locmotion scoring events. 
    # The Selected row corresponds to the locomotion scoring event (locomotion_scoring_event). 
    # In the script, then the item from that row is called. 
  
    meta_variables <- c(
      "Folder",  
      "WATCHSTART",
      "Feature path",
      "Loco_Index",
      "ExclCow",
      "Excl2",
      "Excl3",
      "ExclCosws4",
      "ExclCows5",
      "ExclCows6",
      "ExclCows7",
      "JoinBy", 
      "LSE"
     )
  
    # Jersey trial - First scoring (a) 
    jersey_a <- c(
      "./Jerseys", 
      "02.06.2017 00:00",  
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

    jersey_b <- c(
      "./Jerseys",
      "13.06.2017 00:00",   
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
    black_and_white_2017 <- c(
      "./DGHF2017", 
      "17.06.2017 00:00",  
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
  
    # DairyGold 2018 (Black & white herd)
    black_and_white_2018_a <- c(
      "./DGHF2018", 
      "10.08.2018 00:00", 
      "Loco080818",
      2,
      11, #  Data doesn"t contain walking, SN00017FFD
      13, #  SN00018D41 
      13, #  SN0001932C - Faulty
      15, #  SN00018DD5 - Faulty
      40,
      40,
      40,
      "Ped"
    )
      
    black_and_white_2018_b <- c(
      "./DGHF2018",
      "13.08.2018 00:00", 
      "Loco130818", 
      4,
      11, #  SN00017FFD
      13, #  SN00018D41
      13, #  SN0001932C
      15, #  SN00018DD5 
      40,
      40,
      40, 
      "Ped"  
    )
  
    # Commerical farm 
    farm_a <- c(
      "./Commercial_Farm",
      "17.08.2018 00:00", 
      "Loco160818",
      2, 
      1, #  SN00018E33 no loco score, not attached
      1, #  SN0001932C
      3, #  SN000192D9
      7,
      7,
      7,
      12, 
      "Ped"
     ) # Out by a day SN00018D79
               
    farm_b <- c(
      "./Commercial_Farm", 
      "20.08.2018 00:00",
      "Loco200818",
      4,  
      1, #  SN00018E33 no loco score, not attached
      1, #  SN0001932C - doesn"t record all the way through
      3, #  SN000192D9
      7, #  SN00018DD5 - all lying
      7, 
      7, 
      12, 
      "Ped"
    ) # Out by a day SN00018D79
               
    # LSE selection (Locomotion scoring event)
    
    meta_data_frame <- rbind.data.frame(
      jersey_a, 
      jersey_b,
      black_and_white_2017, 
      black_and_white_2018_a,
      black_and_white_2018_b,
      farm_a, 
      farm_b
    )

    meta_data_frame$LSE <-c (
      "1.aJerseys", 
      "1.bJerseys",
      "2.black_and_white_2017", 
      "3.aBW18a",
      "3.bBW18b",
      "4.aFarma", 
      "4.bFarmb"
    )
    
    colnames(meta_data_frame) <- meta_variables
  
    # Initialise lists to store results from each cohort
    numeric_list <- variable_List <- summary_data_list <- vector(
      mode ="list",
      (
        nrow(meta_data_frame)+3
      )
    )
    
  
    # Loop start----
    for (locomotion_scoring_event in 1: nrow(meta_data_frame) ) { # Load selected locomotion scoring events
      print(
        c(
          "Locomotion Scoring Event",
          locomotion_scoring_event
         )
      )
      
      setwd(home) 
      setwd(
        as.character(
          meta_data_frame$Folder[[locomotion_scoring_event]]
        )
      )
      setwd("./24Hourly")
      
      index <- dir() # Index of file names 
      sub_index<-substr(
        index,
        start = 35,
        stop = 44 
      ) # Pedometer Serial numbers
      
      results_3 <- cbind.data.frame(
        index,
        sub_index
      )
      colnames(results_3) <- c(
        "index",
        "UNITID"
      )
      
      # Files that didn"t store data  check 
      # For making meta table 
      # Data that does not work - exclude
  
      #Excluded Records----
      for (record_to_exclude in 6:11){ 
        Exclude <- as.numeric(
          paste0(
            meta_data_frame[locomotion_scoring_event,record_to_exclude]
          )
        )
        results_3 <- results_3[-Exclude,]
      }
  
      #Locomotion Scores and Reference table----
      setwd("../")
      
      reference_1 <- read.csv(
        file = "PedRef.csv",
        sep = ",",
        header = T
      )
      
      score <- read.csv(
        file = "score.csv",
        sep = ",",
        header = T
      )
      
      NUM <- as.numeric(
        paste0(
          meta_data_frame$Loco_Index[[locomotion_scoring_event]]
        )
      ) # Column with relevant loco score for this 
      
      score$loco2 <- score[,NUM] 
      reference <- left_join(
        score,
        reference_1,
        paste0(meta_data_frame$JoinBy[[locomotion_scoring_event]]
        )
      )
      
      results_2 <- left_join(
        results_3,
        reference,
        "UNITID"
      )
      results_2$inc <- is.na(results_2$loco2) 
      results <- results_2 %>% filter(inc== F) # don"t load
      results <- results[,-(ncol(results))] #remove inc
      results$loco <- results$loco2
      results <- results[,c(1,2,ncol(results))] 
  
      {    
        ## 24 hour data-----
        setwd("./24Hourly")
        index
        #load first & Initialise
        rw_convert <- fread(input = paste(results[1,1]),sep2 = ";", header=T)
        #Append the rest
        for (i in 2:nrow(results)) { # loads all the day records
          feat <- fread(
            input = paste(results[i,1]),
            sep2 = ";",  
            header=T
          )
          rw_convert <- rbind.data.frame(
            rw_convert, 
            feat
          )
        }
  
        rw_convert <- rw_convert %>% filter(
          WATCHSTART==paste(
            meta_data_frame$WATCHSTART[[locomotion_scoring_event]]
          )
        ) # relevant day
        rw_convert[rw_convert==0]<-NA
  
        #Variable Select----
        rw_convert <- rw_convert[,c(1,4:8,12:20)] 
        #assign to a list
        MODDF <- left_join(
          results,
          rw_convert,
          "UNITID"
        )   
        MODDF <- MODDF[,c(-1,-2)]
        summary_data_list[[locomotion_scoring_event]] <- MODDF
        
        # Correlation within trial ----
        # rcorr creates a list of 3 with 1 - Correlation matrix r, 2 n and 3 p values
        print("LSE")
        print(locomotion_scoring_event)
      }
    }
  }
  
  # results begin ----
  # Combine key columns from the lists created above into summary table
  # Want the mean and var of 24 hour summary of each variable
  
  mean.sd <- function(x) 
                    c(
                      mean =mean(x,na.rm = T), 
                      sd = sd(x,na.rm = T)
                    )  
  summary_table_2 <- as.data.frame(
    t(sapply
      (
        X = summary_data_list[[1]],
        FUN = mean.sd
      )
    )
  )
  
  for (i in 2:7){
    summary_table_2 <- cbind.data.frame(
                        summary_table_2,
                        as.data.frame(
                          t(
                            sapply(
                              X = summary_data_list[[i]],
                              FUN = mean.sd
                            )
                          )
                        )
                      )
  }
  
  summary_table_2 <- round(
    summary_table_2,
    digits = 0
  )
  
  summary_table_2 <- rownames_to_column(summary_table_2)
  colnames(summary_table_2) <- c(
    "Variable",
    "1.a Jerseys Mean",
    "1.a Jerseys Var", 
    "1.b Jerseys Mean",
    "1.b Jerseys Var",
    "2 black_and_white_2017 Mean",
    "2 BW17Var", 
    "3.a black_and_white_2018_a Mean",
    "3.a black_and_white_2018_a Var",
    "3.b black_and_white_2018_b Mean",
    "3.b black_and_white_2018_b Var",
    "4.a Farm Mean",
    "4.a FarmaVar", 
    "4.b Farm Mean", 
    "4.b Farm Var"
  )
  
  summary_table_2 <- summary_table_2[order(summary_table_2$Variable,decreasing = F),]
  # Write Behaviour correlation Table
  setwd("../")
  setwd("../")
}
# Outermost

write.csv(x = summary_table_2,file = "Table2_24hr_Summary.csv")

#P Values - Manually add in stars for the few that are significant.

