#This is script 5/5 of the scripts for the lameness studies. It produces that correlation table for the variance in behavior throughout the 24 hour period as an indicator of lameness. Table not presented in paper as these variables were not statistically significiant.
#Start-----
{ 
	{ 
		home<- "C:/Users/olearyn2/OneDrive - Lincoln University/Lameness/RW_Acceleration_and_Behavior" 
		# locoation of Lameness files on your computer - data available from Nialloleary@gmail.com

		library(dplyr); 
		library(data.table);
		library(tibble);
		library(Hmisc)


		#	Meta Table ----
		#	This table contains the meta data for locomotion scoring event required to run the script. 
		#	This facilitate same script being applied to each data set. 
		#	Only these variables should change between locmotion scoring events. 
		#	The Selected row corresponds to the locomotion scoring event (locomotion_scoring_event). 
		#	In the script, then the item from that row is called. 
  
		meta_column_names<-c( #Meta variables "Feature path",
			"Folder",  
			'SerialStart',
			'SerialEnd',
			"Date",
			"LocoName", 
			"Loco_Index",
			"ExclCow",
			"Exclcows2",
			"Exclcows3",
			"ExclCows4",
			"ExclCows5",
			"ExclCows6",
			"ExclCows7",
			'JoinBy', 
			"locomotion_scoring_event"
		)
  
		#Jersey trial - First scoring (a) 
		ja<-c( 
			'./Jerseys',
			'35',
			'44', 
			'02',  
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

		jb<-c(
			'./Jerseys',
			'35',
			'44',
			'13',   
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
		BW17<-c(
			'./DGHF2017', 
			'20',
			'29', 
			'17',  
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
		BW18a<-c( 
			'./DGHF2018',
			'20',
			'29', 
			'10', 
			"Loco080818",
			2,
			11, # data doesn't contain walking, SN00017FFD
			13, #SN00018D41
			13, #SN0001932C # also faulty at commercial farm
			15,#SN00018DD5 #4 cows Also didn't work at commercial farm
			40,
			40,
			40, 
			'Ped'  
		)
  
		BW18b<-c(
			'./DGHF2018',
			'20',
			'29',
			'13', 
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
  
		#Commerical farm 
		Farma <-c( './Commercial_Farm','20','29','17', "Loco160818",2, 
			40, #SN00018E33 no loco score, not attached
      40, # SN0001932C
      40, #SN000192D9
      40,
			40,
			40,
			40, 
			'Ped'
		)# out by a day SN00018D79
               
		Farmb <-c('./Commercial_Farm','20','29', '20',"Loco200818",4,  
            40, #SN00018E33 no loco score, not attached
            40, # SN0001932C
            40, #SN000192D9
            40,
			40,
			40,
            40, 
			'Ped'
		)# out by a day SN00018D79
               
		#locomotion_scoring_event selection (Locomotion scoring event)
		meta_data_frame<-rbind.data.frame(
			ja, 
			jb,
			BW17, 
			BW18a,
			BW18b,
			Farma, 
			Farmb
		)
		meta_data_frame$locomotion_scoring_event<-c(
			"1.aJerseys", 
			"1.bJerseys",
			"2.BW17", 
			"3.aBW18a",
			"3.bBW18b",
			"4.aFarma", 
			"4.bFarmb"
		)
		colnames(meta_data_frame)<-meta_column_names
  
		#Initialise lists to store results from each cohort
		correlation_list<-numList<-pList<-
		  SumDataList<-DayDataList<-NightDataList<-vector(
		mode ="list" ,
		(nrow(meta_data_frame)+3)
		)
		
		#Loop start----

		for (locomotion_scoring_event in 1:nrow(meta_data_frame)) { # Load selected locomotion scoring events
			print(c("Locomotion Scoring Event",locomotion_scoring_event))
			setwd(home) 
			setwd(as.character(meta_data_frame$Folder[[locomotion_scoring_event]]))
			setwd('./1Hourly')
			inde<-dir() # Index of file names 
			inde2<-substr(
				inde,
				start = paste(meta_data_frame$SerialStart[[locomotion_scoring_event]]),
				stop= paste(meta_data_frame$SerialEnd[[locomotion_scoring_event]])
			) # Pedometer Serial numbers
			Results3<-cbind.data.frame(inde,inde2)
			colnames(Results3)<-c("inde","UNITID")
			#Files that didn't store data  check # for making meta_data_frame table 
			#Data that does not work - exclude

			#Excluded Records----
			# If you want to exclude 12 &13 you exclude 12 twice (13 goes to 12th position). if 12 and 14, 12 and 13 (14 goes to 13).
			#names(meta_data_frame)      
			for (m in 7:13){ 
				Exclude<- as.numeric(paste0(meta_data_frame[locomotion_scoring_event,m]))
				Results3<-Results3[-Exclude,]
			}

			#Locomotion Scores and Reference table----
			setwd("../")
			Ref1<-read.csv(
				file = 'PedRef.csv' ,
				sep = ",",
				header = T
			)
			Score<-read.csv(
				file =  'Score.csv',
				sep = ",",
				header = T
			)# 
			NUM <-as.numeric(paste0(meta_data_frame$Loco_Index[[locomotion_scoring_event]])) # Column with relevant loco score for this 
			Score$loco2<-Score[,NUM] 
			Ref<- left_join(
				Score,
				Ref1,
				paste0(meta_data_frame$JoinBy[[locomotion_scoring_event]])
			)
			Results2<-left_join(
				Results3,
				Ref,
				"UNITID"
			)#
			Results2$inc<-is.na(Results2$loco2) 
			Results<- Results2 %>% filter(inc== F) # don't load
			Results<-Results[,-(ncol(Results))] #remove inc
			Results$loco<-Results$loco2
			Results<-Results[,c(1,2,ncol(Results))] 

			{    ## 1 hour data-----
				setwd('./1Hourly')
		  
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

			RWconvert$Date<-substr(
					x = RWconvert$WATCHSTART,
					start = 1,
					stop = 2
				)
				RWconvert <- RWconvert %>% 
				  filter(Date==paste(meta_data_frame$Date[[locomotion_scoring_event]])) #Chosen day

				#Variance within Day----
						MODDF<- RWconvert %>% group_by(UNITID)%>% 
				  summarise_all(var) #<------- Distinct Here

				MODDF<-left_join(Results,MODDF,'UNITID')
				names(MODDF)
				MODDF<-MODDF[,c(-1,-2,-4:-6,-11:-13,-32,-34)]
				SumDataList[[locomotion_scoring_event]]<-MODDF
				#### Correlation within trial ----
				# rcorr creates a list of 3 with 1 - Correlation matrix r, 2 n and 3 p values

				CorMat <- rcorr(as.matrix(MODDF),type = 'spearman')

				#r
				DFCor<-rownames_to_column(as.data.frame(CorMat[1]))
				correlation_list[[locomotion_scoring_event]]<-as.data.frame(DFCor[,1:2])

				#n - values
				DFCor<-rownames_to_column(as.data.frame(CorMat[2]))
				numList[[locomotion_scoring_event]]<-as.data.frame(DFCor[,1:2])

				#p-values
				DFCor<-rownames_to_column(as.data.frame(CorMat[3]))
				pList[[locomotion_scoring_event]]<-as.data.frame(DFCor[,1:2])

				print("locomotion_scoring_event")
				print(locomotion_scoring_event)
			}
		}

		#Results begin ----
		#Combine key columns from the lists created above into summary table
		correlation_listA<-as.data.frame(correlation_list[1])
		for (i in 2:7){
			correlation_listA<-left_join(correlation_listA,
			                             as.data.frame(correlation_list[[i]]),"rowname")
		}

		#P-value summary table ----
		pListA<-as.data.frame(pList[1])

		for (i in 2:7){
			pListA<-left_join(
			pListA,
			as.data.frame(pList[[i]]),
			"rowname"
		)

		colnames(pListA)[ncol(pListA)]<-ncol(pListA)-1 }

		pListA$AveragePVal <- rowMeans(pListA[,2:8])
		   
		correlation_listA$AverageAbsoluteCorrelation <- 
		  rowMeans(correlation_listA[,2:8])
		
		correlation_listA$SqrAverageAbsoluteCorrelation<-
		  sqrt(correlation_listA$AverageAbsoluteCorrelation*
		         correlation_listA$AverageAbsoluteCorrelation)
	
			is.num <- sapply(correlation_listA, is.numeric)

		correlation_listB<-cbind(correlation_listA[,1],
		                         as.data.frame(lapply(
		                           correlation_listA[is.num],
		                                              round,2)))

		#Blank values of negligible size
		correlation_listC<-replace(correlation_listB,
		                           correlation_listB<0.2 & 
		                             correlation_listB > -0.2,NA)


		colnames(correlation_listC)<-c("Variable", "JerseysA","JerseysB", 
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

write.csv(x = correlation_listC,file = "Tablex1Not in Paper.csv")
write.csv(x = plstSig,file = "Table1xPvals.csv")
#P Values - Manually add in stars for the few that are significant.
