#This is a script written to load and analyse the data of Haladchen et al 2018 who kindly shared their data. 

#???? What is different about my implementation of steps to Haladjian's?

#The right is the most robust as doesn't impact the leg with the sensor?


#Setup----
{
  # Adjustable variables for the run
  #Variables to widen
#  [1] "ax" "ay" "az"   [4] "yaw"  "pitch"  "roll"             [7] "x"                "y"                "z" 
 # [13] "x2"               "y2"               "z2"   
  wvar<-2
  
  windowStart<- (15) # Hundreths of a second before strike - # Minus numbers = after nadir
  
  windowLength<-35 # want to capture all the data of interest but don't want to make the program too slow 
  #Minimum window 25?
library(data.table);library(dplyr);  library(RcppRoll); library(Rfast); library(e1071) ;library(tibble);
  options(scipen=999, digits = 6)
  SD <- function(x) sd(x)#/sqrt(length(x))
  home<- "D:/NonZipLameness/HaladajanData/Raw_XYZ" #Change for your computer
  setwd(home)
  inde<-dir()
  kuh_nummer<-substr(inde,start = 1,stop= 1)
  bein2<- substr(inde, start=3, stop =15) #bein = leg in German
  bein<-substr(bein2, start=1,stop= (nchar(bein2)-4))
  Results <- cbind.data.frame(inde,kuh_nummer,bein)
  AveragesList<- vector(mode ="list",length = (nrow(Results)))
  WalkList<- vector(mode ="list",length = (nrow(Results)))
  
  #Loading data----
  i<-1
  for (i in 1:nrow(Results)){
  print(c("file",i))
  walk <- fread(input = paste(Results[i,1]),sep2 = ";", header=T)  # #unbalanced data set - 3 x more records from 'healthy' period than either left or right-  Consider limiting data imported to smallest n walk to speed up analysis - balance data?

    nwalk<-nrow(walk)
  Results$nwalkFull[[i]]<- nwalk
#Standardise to Standard Deviations
  walk$x<-scale(x = walk$ax,center = T, # Not centering causes an error if False? Don't understand why.
      scale = T)
  walk$y<-scale(x = walk$ay,center = F,scale = T)
  walk$z<-scale(x = walk$az,center = F,scale = T)
  walk$index<-1:nwalk
  walk$ID<-Results$kuh_nummer[i] # Cow Number
  walk$loco<-Results$bein[i] # Leg
  #Necessary? Squared root 
  walk$x2<-sqrt(walk$x^2)  # used for rolling max for consistent walking
  walk$y2<-sqrt(walk$y^2)
  walk$z2<-sqrt(walk$z^2)
#Step identification looking for the local nadir to identify candidate points where the cows' leg may have struck the ground. 
  
walk$xmin<-roll_min(x =walk$x,n = 180,
        # 0.9 of a second each direction 
      fill = NA,align = "center")  # rolling minimum
walk$strike<-(walk$x==walk$xmin) # step nadir found = local minimum

### Consistent walking----

#The objective is to identify when the cow is walking for an extended period as this is 
    #when manual locomotion scorers can discern if a cow is lame or not.
    #Alternatively - exclude When the cow is standing or moving slowly (relative to itself) or walking intermittently 
#To do this we ascertain the maximum recorded accelertion over 20 seconds and create a vector with this rolling max

walk$x2mean<-roll_mean(x =walk$x2,n = 2000,
         fill = NA,align = "center")  
#We only want greater than x percentile of this rolling max data as it is more more likely to be the walking consistently  
#Setting a threshold
Walkingthresh<-quantile(walk$x2mean,probs = 0.4,na.rm = T)
#Exceeds threshold
walk$WalkingTrueFalse<- walk$x2mean>Walkingthresh 

#Select data more like consistent walking 
walka<-walk[walk$WalkingTrueFalse==T,]
walk2<-walka[walka$strike==T,] # Select local minimum
walk2<-walk2[walk2$x<(-2),] # filter non strike points
#After filtering - create index for measuring duration of steps
walk2$indexm1[1:nrow(walk2)]<- # create an index
  c(0,# add the 0 at the start
    walk2$index[1:(nrow(walk2)-1)]) # index x minus one

walk2$duration<-walk2$index-walk2$indexm1
# filter step durations outside reasonable limits - literature says 1.3 seconds approximately on average
walk3<-walk2[walk2$duration>80 & walk2$duration <170,] 
#  hist(walk3$duration)
Results$DurSkew[i]<-skew(na.omit(walk3$duration))
Results$DurKurt[i]<-kurtosis(na.omit(walk3$duration))
steps<-walk3$index # list of records matching the step nadir points
  
#Now need to assign values prior & after nadir in columns to the right on same row  - widen the data- each row a step

# Swing start 0.3 - 0.7 seconds before strike - 31 - 76 hundreths of a second before the strike  


#Transpose Matrix----
print("Start Widening")
#Matrix for faster calculations
tranmat<-matrix(ncol = (windowLength+2), #size of transpose window
  nrow = (nwalk)) # full length of raw data set
holder<-as.matrix(walk[,..wvar])

tranmat[(1:nwalk),1]<-holder
 #tranmat[,1]<-walk$ax # v 31 L& R, v 18 
# tranmat[,1]<-walk$x
#  tranmat[,1]<-walk$x2
# tranmat[,1]<-walk$ay # doesnt work - can't do swing duration
#  tranmat[,1]<-walk$az
 #  tranmat[,1]<-walk$yaw #v38 1 sided
  #  tranmat[,1]<-walk$pitch # Error swingPC
  # tranmat[,1]<-walk$roll # Error swing dur
#Tried to automate pulling each of them in but created errors 

tranmat[,2]<-walk$index # required for left join later 46
head(tranmat)
 #Loop widen data ----
 p<-110 # for troubleshooting 
print("Loop Start 1")
for (p in steps){ # rows to be filled (stike rows)
tranmat[p,(3:ncol(tranmat))] <- tranmat[(p-windowStart-1):(p-(windowStart+windowLength)),1] # 61 values
} # from the input column 
# After input and index
# 1 because of overlap?

tranmat2<-as.data.frame(tranmat)# back to Data Frame after fast matrix calculations
names(tranmat2)<-c("SelectedInput","index",(windowStart+1):(windowStart     +windowLength))
steps2<-left_join(walk3,tranmat2,"index")
#names(steps2)

#First transpose to use fast roll mean function
TransSwingDF<-t(as.matrix(steps2[,23:(22+windowLength)]))  
#Rolling mean to get more consistent direction facilitating detection of swing phase
rSwingDF<-as.data.frame(t # transpose back
    (roll_mean(x = TransSwingDF, n = 15))) # Judgement
names(rSwingDF)<-((windowStart+7):# ((15-1)/2)=7
          ((windowStart+windowLength-1)-7))# center of 15
  
#Individual steps -  ----
#Create a new matrix for above or below threshold which distinguishes stance from swing (in reverse swing back to stance)

print("start swing analysis")

rSwingDF[rSwingDF<0.35]<-0 #Tuned...----
#to achieve results consistent with Alsaaod et al 2017 (cow pedogram) ~37%

Append<-(windowStart+7):(windowLength-7)# For use in for loop as the index in the last row 
  rSwingDFd<-rbind.data.frame(rSwingDF,Append) # Rbind

#This is a for loop in a for loop. For row R, go to column Col 
for (r in 1:(nrow(rSwingDFd)-1)){ # row by row
for (Col in (ncol(rSwingDFd)-1):1){ # column by column reverse order to get 0 closest to nadir - overwritting past ones

#calculate swing duration by ...
if(rSwingDFd[r,Col]==0){ # if 0...
    steps2$SwingDur[r]<- # Assign from the... 
  (rSwingDFd[nrow(rSwingDFd), # ... last row (index - appended earlier)...
         Col]) #the current column number
  
# causing error when window moved to 20 or below. For squared values
    } 
  }
  }

#Results loop----
steps2$swingPC<-steps2$SwingDur /steps2$duration
names(steps2)
WalkList[[i]]<-steps2
#Here check----  
steps3<-steps2[,c(-11,-12,-17,-19)]
names(steps3)

#Means & Var----
ResCol<-7
Results[i,ResCol:(ncol(steps3)+ResCol-1)]<-as.vector(colMeans(na.omit(steps3))) # Column means from steps3 
  
Results[i,(ncol(steps3)+ResCol):((ncol(steps3)*2)+ResCol-1)]<-as.vector(apply(X = steps3,2,FUN = var, na.rm = T))
#Column variances from steps3 
} 
# END INDIVIDUAL Cow data analysis ----
  
#Names to for the results table 
  names(Results)<-
  c(names(Results[,1:(ResCol-1)]), 
  paste("M",names(steps3),sep="_"), #Mean
  paste("V", names(steps3),sep="_"))#Variance

#Create Tables where the block is not on, on left, on right.

  {  nc <- ncol(Results)
    Left <- Results[Results$bein == "left", 4:nc]
    Normal <- Results[Results$bein == "normal", 4:nc]
    Right <- Results[Results$bein == "right", 4:nc]
    
    # T tests -----
    
    #Pval 1 = group comparison 
    PVal1 <- as.data.frame(matrix(nrow = ncol(Left), ncol = 3))
    row.names(PVal1) <- names(Normal)
    
    #Normal - Left
    for (i in 1:nrow(PVal1)) {
      r<-t.test( #Probability the
        Normal[,i], # change in cow
          Left[,i] # differs from 0 
      )
      PVal1[i,1]<-  r$p.value  } # assign p - value
    
    #Normal - Right
    for (i in 1:nrow(PVal1)) {
      r<-t.test(Normal[,i], Right[,i]) # change
      PVal1[i,2]<-  r$p.value  }
    #Left - Right
    for (i in 1:nrow(PVal1)) {
      r<-t.test(Left[,i], Right[,i])
      PVal1[i,3]<-  r$p.value  }
    
    PVal1$Lame<-(PVal1$V1+PVal1$V2)/2
    names(PVal1)<-c("NormLeft","NormRight","LeftRight","LameAverage")
  }
  
  
  
{  
  # T tests -----

  #Pval 2 = change from one state to another
 PVal2 <- as.data.frame(matrix(nrow = ncol(Left), ncol = 3))
 row.names(PVal2) <- names(Normal)

#Normal - Left
for (i in 1:nrow(PVal2)) {
  r<-t.test( #Probability the
    Normal[,i]- # change in cow
              Left[,i],
            mu=0 # differs from 0 
            )
  PVal2[i,1]<-  r$p.value  } # assign p - value

#Normal - Right
for (i in 1:nrow(PVal2)) {
  r<-t.test(Normal[,i]- Right[,i],mu=0) # change
  PVal2[i,2]<-  r$p.value  }
#Left - Right
for (i in 1:nrow(PVal2)) {
  r<-t.test(Left[,i] - Right[,i], mu=0)
  PVal2[i,3]<-  r$p.value  }

PVal2$Lame<-(PVal2$V1+PVal2$V2)/2
names(PVal2)<-c("NormLeft","NormRight","LeftRight","LameAverage")
  }
}


# Ttests reported in paper
t.test(Left$M_duration- Normal$M_duration, mu=0)
t.test(Normal$M_swingPC-Left$M_swingPC,mu=0)

#Step Duration----
t.test(Normal$M_duration -Left$M_duration, mu = 0)
t.test(Normal$V_duration -Left$V_duration, mu = 0)
t.test(Normal$V_SwingDur -Left$V_SwingDur, mu = 0)
mean(Results$M_duration)
mean(Normal$M_duration)
mean(Left$M_duration)
mean(Right$M_duration)

#Swing pc ----
plot(Results$V_swingPC~Results$bein)
plot(Results$M_SwingDur~Results$bein)

ListSwing<-c("M_SwingDur","M_swingPC","V_SwingDur","V_swingPC")
PVal2b<-rownames_to_column(PVal2)
SwingPval<- (PVal2b[PVal2b$rowname %in% ListSwing,])
setwd("D:/Lameness")
setwd("./Tables" ) 
write.csv(x = SwingPval,file = "SwingSummaryHalad.csv")

#Novel features----
plot(Results$V_30~Results$bein)
plot(Results$V_x2mean~Results$bein)
plot(Results$M_pitch~Results$bein)
