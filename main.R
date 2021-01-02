#!/usr/bin/env Rscript
# Load libraries
library(cli)
library(dplyr)
library(stringr)

# Misc.:

# File import func
fileImport<-function(header){
  # Import the file
  filex<-file.choose()
  # Fix newline problem
  cat("\n", file = filex, append = TRUE)
  # Read the file as CSV
  x<-read.csv(file=filex,header = header)

  return (x)
}


# Split input func
inpSplit<-function(text){
  result<- strsplit(readline(prompt=text),",")
  return(result)
}

# Convert to integer func
toInt<-function(list){
  for (variable in list) {
    int<-as.numeric(variable)
    return(int)
  }
}

# Welcome message
welcomeMsg<-'Hi Welcome to the RM Terminal by Aviv'
cli::cat_boxx(welcomeMsg)

# Topic 1 (Forecasting)
# Main Menu List
menuListT1<-c(
  'Occupancy Forecast with LOS and Pick-Up Matrix',
  'Forecast Analysis (Forecast Table with All LOS Required | Change ALL LOS1 to LOS1-X | Max LOS = 3)',
  'Back'
)

# Topic I menu
topicI<-function(){
  choice<-menu(menuListT1,title='What do you need?')
  switch (choice,
          '1' = {occForecast(); cat("\n");topicI()},
          '2'={forecastAnalysis(); cat("\n");topicI()},
          '3'=topicSelect()
  )
}

occForecast<-function(){
  # Import the file
  x<-fileImport(FALSE)
  # Convert it to data frame
  dfc<-data.frame(x)
  # Find today: get the index of the first NA in the DBA=-1 column
  todayIndex<-which(is.na(dfc[,4]))[1]
  # Find the corresponding day in the DOW & date column using the index
  today<-c(dfc[,2][todayIndex],dfc[,1][todayIndex])
  # Convert it to data frame + remove the date
  df<-data.frame(x[,-1])
  # Extract the DBA
  DBA<-df[1,seq(3,length(df[1,]),by=1)]
  # Pre-pend a value to DBA as the colname for DOW
  DBA<-cbind(100,DBA)
  # Extract DOW
  DOW<-unique(df[1])
  # Remove the text value: DOW
  DOW<-DOW[-1,]
  #Get pure DOW and their respective bookings regarding to DBA
  df<-df[df$V2 %in% DOW,]
  # Remove the last year's value
  df<-df[,-2]
  # Add colname for df
  colnames(df)<-DBA
  # Create the avg booking curve
  ## Re-order the df by DOW
  orderedDf<-NULL
  for (dsow in DOW) {
    orderedDf<-rbind( orderedDf,df[df$'100' ==dsow,])
  }
  # Split the ordered df into separate dfs grouped by DOW + remove NAs
  splitedOrderedDf<-list()
  for (i in 1:length(DOW)) {
    splitedOrderedDf[[i]]<-na.omit(subset(orderedDf,orderedDf$'100' %in% DOW[i]))
  }

  # Calculate the col means of each DBA values to create the avg booking curve table
  BCT<-list()
  for (i in 1:length(splitedOrderedDf)) {
    BCT[[i]] <-
      data.frame(
        colMeans(
          splitedOrderedDf[[i]]
          [,2:length(colnames(splitedOrderedDf[[i]]))] #omit the DOW as it's not numerical
        )
      )
  }

  # Give each of the df in the list a name (DOW)
  for (i in 1:length(BCT)) {
    colnames(BCT[[i]])<-DOW[i]
  }
  # Combine, transpose, round all to form an average BCT
  ABCT<-t(data.frame(BCT))
  cli_alert_success('Average Booking Curve Table: ')
  cat('\n')
  print(ABCT)
  cat('\n')
  #Pickup matrix
  PPM<-data.frame(round(ABCT[,1]-ABCT))
  cli_alert_success('Pickup Matrix: ')
  cat('\n')
  print(PPM)
  cat('\n')
  # Print today
  print(paste('Today:',today))
  # Ask for no. of days to forecast
  fDate<-toInt(inpSplit('How many days in advance do you want to forecast: '))
  # Get the forecast index by adding the forecast days to todayIndex
  forecastIndex<-todayIndex+fDate
  # Get all the indices in between
  forecastIndecies<-seq(todayIndex,forecastIndex,by=1)
  # Forecast info.
  forecastDOW<-dfc[,2][forecastIndecies]
  forecastDate<-dfc[,1][forecastIndecies]
  forecastDBA<-seq(0,fDate,by=1)
  # ROH
  forecastLastCol<-length(forecastDBA)+2
  forecastData<-df[,3:forecastLastCol]
  ROHIndex<-which(is.na(forecastData))
  ROHIndex<-ROHIndex[1:7]-1
  ROH<-numeric()
  for (i in 1:length(forecastData)) {
    ROH<-c(ROH,forecastData[i][ROHIndex[i],])
  }
  # DF first, otherwise all will be convert to characters
  forecastTable<-data.frame(cbind(forecastDOW,forecastDate))
  # Add numerical data
  forecastTable<-cbind(forecastTable,forecastDBA,ROH)
  # Pickup values
  pickupDate<-fDate+2
  pickupIndex<-seq(1,pickupDate-1)
  pickupData<-PPM[,2:pickupDate]
  pickup<-numeric()
  for (i in 1:length(pickupData)) {
    pickup<-c(pickup,pickupData[i][pickupIndex[i],])
  }
  forecastTable<-cbind(forecastTable,pickup)
  forecastFinal<-forecastTable[,4]+pickup
  forecastTable<-cbind(forecastTable,forecastFinal)
  cli_alert_success('Forecast Table: ')
  cat('\n')
  print(forecastTable)
  cat('\n')
}

forecastAnalysis<-function(){
  # Import the file
  x<-fileImport(TRUE)
  # Convert it to data frame
  df<-data.frame(x)
  # Get the col name with LOS = 1.xx
  LOS_1<-grep("LOS1.",colnames(df))
  # Get the last column no.
  lastColNo<-which(colnames(df)==colnames(df)[length(colnames(df))])
  # Get LOS range
  losRange<-numeric()
  for (i in 1:length(LOS_1)) {
   if (!is.na(LOS_1[i+1])) {
     losRange<-c(losRange,seq(LOS_1[i],LOS_1[i+1]))
   }else{
     losRange<-c(losRange,seq(LOS_1[i],lastColNo))
   }
  }
  # Remove duplicates
  losRange<-unique(losRange[-length(losRange)])
  # Sum of each row starting from the 3rd row
  sumRest<-numeric()
  for (i in 1:length(df[,1])) {
    sumRest<-c(sumRest,sum(df[,losRange][i,],na.rm = TRUE))
  }
  # Fill the room occupied with the sum of each row
  df[,lastColNo][1:length(df[,lastColNo])]<-sumRest
  # Calculate the rooms occupied for the 2nd row
  ## Get LOS 2,3...
  losFilterSec<-losRange[!(losRange %in% LOS_1)]
  # Sum of LOS > 1 of the 1st row
  df[,lastColNo][2]<-sum(df[1,losFilterSec])+df[,lastColNo][2]

  # Calculate room occupied for the rest
  ## Get the 3rd LOS
  DIFF<-diff(c(losFilterSec,losFilterSec[length(losFilterSec)]+2))
  thirdLos<-DIFF[DIFF!=1]
  # Get the index of them DIFF which corresponds to losFilterSec
  thirdLosIndex<-which(DIFF %in% thirdLos)
  # Get LOS3
  losFilterThird<-losFilterSec[thirdLosIndex]
  # Sum of LOS3
  sumLosThird<-numeric()
  for (i in 1:length(df[,1])) {
    sumLosThird<-c(sumLosThird,sum(df[,losFilterThird][i,],na.rm = TRUE))
  }
  # Length of sumLosThird
  lLosThird<-length(sumLosThird)
  lLosThird1<-lLosThird-1
  # Add them to the existing room occupied
  df[,lastColNo][3:length(df[,lastColNo])]<-
    sumLosThird[-lLosThird1:-lLosThird]+ df[,lastColNo][3:length(df[,lastColNo])]
  # Sum of LOS 2,3
  sumLosSecThird<-numeric()
  for (i in 1:length(df[,1])) {
    sumLosSecThird<-c(sumLosSecThird,sum(df[,losFilterSec][i,],na.rm = TRUE))
  }
  # Add them to the existing room occupied
  lLosSecThird<-length(sumLosThird)
  df[,lastColNo][3:length(df[,lastColNo])]<-
    sumLosSecThird[c(-1,-lLosSecThird)]+ df[,lastColNo][3:length(df[,lastColNo])]

  # Ask for no. of days to forecast
  fDate<-toInt(inpSplit('How many days in advance do you want to forecast: '))
  lastHistoricalDate<-length(df[,lastColNo])-fDate
  # The minimum hotel capacity
  minCap<-max(df[,lastColNo][1:lastHistoricalDate-1])

  cli_alert_success('Forecast Analysis: ')
  cat('\n')
  print(df)
  cat('\n')

  cli_alert_success('Minimum Hotel Capacity ')
  cat('\n')
  print(minCap)
  cat('\n')

}

# Topic 2 (Group Request)
# Main Menu List
menuListT2<-c(
  'Group Request',
  'Back'
)

# Topic II menu
topicII<-function(){
  choice<-menu(menuListT2,title='What do you need?')
  switch (choice,
          '1' = {gr(); cat("\n");topicII()},
          '2'=topicSelect()
  )
}

gr<-function(){
  # Import the file
  x<-fileImport(TRUE)
  # Convert it to data frame
  df<-data.frame(x)
  # Extract the service cost/price
  serviceDf<-df[5:length(rownames(df)),]
  # Remove the service cost/price from the main data frame
  df<-df[-(5:length(rownames(df))),]
  # Set row names
  rownames(serviceDf)<-serviceDf[,1]
  rownames(df)<-df[,1]
  # Remove the first col, which is used as row names
  serviceDf<-serviceDf[,-1]
  df<-df[,-1]
  # Remove NA columns from serviceDf
  serviceDf<-serviceDf[, colMeans(is.na(serviceDf)) != 1]
  # Set colnames for service Df
  colnames(serviceDf)<-c('Price','Cost','Client_Percentage')
  # Ask for the hotel's capacity
  hotelCapacity<-toInt(inpSplit('Hotel Capacity: '))
  # Calculate transient + group
  df['FIT_Group',] <- df['Constrained_Demand',]+df['Room_Group_Demand',]
  # Calculate the displaced transient
  df['Displaced_Transient',]<-df['FIT_Group',]-hotelCapacity
  # If FI_Group < hotelCapacity => displaced transient = 0
  df['Displaced_Transient',][sign(df['Displaced_Transient',])==-1]<-0
  # Room demand
  rdm<-df
  # Total
  totalGroupRooms<-sum(rdm['Room_Group_Demand',],na.rm = TRUE)
  # Displace contribution
  dc<-NULL
  # Copy the displaced transient to the displace contribution data frame
  for (i in 1:length(rownames(serviceDf))) {
    dc<-rbind(df['Displaced_Transient',],dc)
  }
  # Set row names for displaced contribution
  rownames(dc)<-rownames(serviceDf)

  # Ancillary contribution
  acType<-toInt(inpSplit('Does the group cost differs from date? [1=Y,0=N]:'))
  if(acType==0){
    # Remove group
    dc<-dc[-grep("G.",rownames(dc)),]
    # Remove NAs
    dc<-dc[, colMeans(is.na(dc)) != 1]
    # Calculate the displaced contribution
    dc<-dc*serviceDf[grep("T.",rownames(serviceDf)),2]*serviceDf[grep("T.",rownames(serviceDf)),3]
    # Total displaced contribution
    tdc<-sum(dc[grep("T.",rownames(dc)),])
    rac<-
      sum(serviceDf[grep("G.",rownames(serviceDf)),'Cost'])*length(colnames(dc))
    # Ask for the room cost
    roomCost<-toInt(inpSplit('Ask for room cost:'))
    # Calculate the minimum rate (MAR)
    mar<-(tdc-rac)/totalGroupRooms+12
    # Calculate minimum contribution
    mc<-sum(serviceDf[grep("G.",rownames(serviceDf)),'Price'])*length(colnames(dc))+totalGroupRooms*mar
  }else{
    # Remove NAs
    dc<-dc[, colMeans(is.na(dc)) != 1]
    # Calculate the displaced contribution
    dc<-dc*serviceDf[,2]*serviceDf[,3]
    # Total displaced contribution
    tdc<-sum(dc[grep("T.",rownames(dc)),])
    # Anc contrib
    rac<-toInt(inpSplit('Enter the Ancillary Contribution:'))
    # Ask for the room cost
    roomCost<-toInt(inpSplit('Ask for room cost:'))
    # Calculate the minimum rate (MAR)
    mar<-(tdc-rac)/totalGroupRooms+12
    # Calculate minimum contribution
    preMc<-toInt(inpSplit('Enter the Combined Service Price for the Group:'))
    mc<-preMc+totalGroupRooms*mar
  }




 # Results
  cli_alert_success('Room Demanded: ')
  cat('\n')
  print(rdm)
  cat('\n')
  cli_alert_success('Displaced Contribution: ')
  cat('\n')
  print(dc)
  print(paste('Total:',tdc))
  cat('\n')
  cli_alert_success('Ancillary Contribution: ')
  cat('\n')
  print(paste('Total:',rac))
  cat('\n')
  print(paste('Minimum Rate:',mar))
  cat('\n')
  print(paste('Minimum Contribution:',mc))


}


# Main Menu Selection Function
topicSelect=function(){
  menuList<-c(
    'Forecasting',
    'Group Request'
  );

  choice<-menu(menuList, title='Please Select A Topic:');
  # Menu Selection Function
  mSelect<-function(topic){
    switch (topic,
            '1' = topicI(),
            '2' = topicII(),
    )
  };
  mSelect(choice);

}
topicSelect()


