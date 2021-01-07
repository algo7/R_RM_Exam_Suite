#!/usr/bin/env Rscript
# Load libraries
library(cli)
library(dplyr)
library(stringr)
library(lpSolve)
library(boot)



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

# Convert to character func
toCha<-function(list){
  for (variable in list) {
    int<-as.character(variable)
    return(int)
  }
}

# Custom lookup func(same as excel lookup)
# Get the value that's smaller or equal to and closest to the input val
lookUp<-function(val,toCompare){
  res<-val-toCompare
  # When overbook no. is 0
  if(length(res)==0){
    return(NA)
  }else{
    # Return the position of val. that has the smallest diff from the val
    return(which(res==min(res)))
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

# Topic 3 (Linear Programming)
# Main Menu List
menuListT3<-c(
  'Linear Programming',
  'LP Exam',
  'Back'
)

# Topic III menu
topicIII<-function(){
  choice<-menu(menuListT3,title='What do you need?')
  switch (choice,
          '1' = {lpf(); cat('\n');topicIII()},
          '2' = {lpExam(); cat('\n');topicIII()},
          '3'=topicSelect()
  )
}

lpf<-function(){
  # Import the file
  x<-fileImport(FALSE)
  # Convert it to data frame
  df<-data.frame(x,row.names = 1)
  roomAvailable<-na.omit(data.frame(df[,length(colnames(df))]))
  df<-df[,-length(colnames(df))]
  dfo<-df
  # Rooms occupied
  ## Replaced the the data frame with ones
  df[3:9,]<-1
  # Sum of each row
  sumRows<-data.frame(apply(df, 1, sum))
  sumRows<-sumRows[-(1:2),,drop = FALSE]
  # Colmuns where LOS != 1
  losNotOne<-df[,which(df['LOS',]!=1)]
  losNotOne<-data.frame(apply(losNotOne, 1, sum))
  losNotOne<-losNotOne[-(1:2),,drop = FALSE]
  # Dataframe of DOW except for Mon along with LOS!=1
  pre<-cbind(losNotOne,sumRows)[2:length(rownames(cbind(losNotOne,sumRows))),]
  # Rooms occupied
  roomOccupied<-rbind(sumRows[1,],data.frame(apply(pre,1,sum)))
  colnames(roomOccupied)<-'RoomsOccupied'
  rownames(roomOccupied)[1]<-'Monday'
  # Calculate the revenue
  dfc<-df # Make a copy of the original df
  for (i in 3:length(rownames(dfc))) {
   dfc[i,]<-dfc[i,]*df[1,]*dfc[2,]
  }
  rev<-data.frame(apply(dfc[3:9,], 1, sum))
  colnames(rev)<-'Revenue'
  revSum<- sum(rev)

  # Room Occupied <= Room Available
  # pick-ups to be optimized <= unconstrained pick-up
  unconPikcup<-as.vector(unlist(dfo[3:9,])) # Unconstrained pick-up
  optPickup<-as.vector(unlist(df[3:9,])) # pick-ups to be optimized
  ra<-as.vector(unlist(roomAvailable))
  ro<-as.vector(unlist(roomOccupied))
  # diag martrix optPickup & unconPickup

  lp('max',as.vector(rev),as.vector(cbind(optPickup,ro)),'<=',as.vector(cbind(unconPikcup,ra)),all.int=TRUE)


}

# Excel solver is not allowed during the exams
# So there will only be basic questions in terms of LP
lpExam<-function(){

  f.obj<-toInt(inpSplit('Enter the Coefficient of the Objective Function in CSV, e.g.(1,2,3):'))
  f.con.no<-toInt(inpSplit('Enter the Number of Constraints:'))
  f.con.pre<-toInt(inpSplit('Enter the Coefficient of the Constraints in CSV, e.g.(1,2,3):'))
  f.dir<-toCha(inpSplit('Enter the Direction in CSV, e.g.(=,<,<=):'))
  f.rhs<-toInt(inpSplit('Enter the Value on the Righthand Side of the Constraints:'))
  opType<-toInt(inpSplit('Enter the direction:, 1=Max, 0=Min:'))
  f.con <- matrix(c(f.con.pre), nrow = f.con.no,byrow = TRUE)
  res<-NULL
  if(opType==0){
    res<-lp('min',f.obj,f.con,f.dir,f.rhs,compute.sens = TRUE)
    opVal<-res$objval
    sol<-res$solution
    res<-paste('Optimized Value:',opVal,'Solutions:',sol)
  }else if(opType==1){
    res<-lp('max',f.obj,f.con,f.dir,f.rhs,compute.sens = TRUE)
    opVal<-res$objval
    sol<-res$solution
  }

  cli_alert_success('Results: ')
  cat('\n')
  print(paste('The Optimized Value:',opVal))
  print('The Solutions:')
  print(sol)
  print(res$duals)

}


# Topic 4 (Overbooking)
# Main Menu List
menuListT4<-c(
  'Overbooking (Economic Model)',
  'Overbooking (Service Model) | 1 guest out of xxx guests',
  'EMRR',
  'EMRR (Exam)',
  'Back'
)

# Topic IV menu
topicIV<-function(){
  choice<-menu(menuListT4,title='What do you need?')
  switch (choice,
          '1' = {obe(); cat('\n');topicIV()},
          '2' = {obs(); cat('\n');topicIV()},
          '3' = {emrr(); cat('\n');topicIV()},
          '4' = {emrrEx(); cat('\n');topicIV()},
          '5'=topicSelect()
  )
}

obe<-function(){
  # Import the file
  x<-fileImport(FALSE)
  # Convert it to data frame
  df<-data.frame(x,row.names = 1)
  # Ask for the hotel's capacity
  hotelCapacity<-toInt(inpSplit('Hotel Capacity: '))
  # Ask for the var. cost for the room
  varCost<-toInt(inpSplit('The Variable Cost of the Room: '))
  # Update the df
  df['Variable-costs',]<-varCost
  # Calculate the goodwill
  ## Whether to calculate the goodwill (the row can be pre-populated in the csv as well)
  gwCal<-toInt(inpSplit('Do you need to calculate the goodwill (1=Yes,0=No) '))
  if(gwCal==1){
    gwFactor<-toInt(inpSplit('The Goodwill Factor, e.g.(2,3 [times the room price]): '))
    df['Goodwill',]<-df['Rates',]*gwFactor
  }
  # Calculate the re-accommodation cost
  ## Whether to calculate re-accommodation cost (the row can be pre-populated in the csv as well)
  rcCal<-toInt(inpSplit('Do you need to calculate the re-accommodation cost (1=Yes,0=No) '))
  if(rcCal==1){
    rcFactor<-toInt(inpSplit('The Re-accommodation Cost Factor e.g.(2,3,0.8, times the room rate), '))
    df['Re-accomodation',]<-df['Rates',]*rcFactor
  }
  # Calculate the cost of walk
  df['Cost-of-walk',]<-df['Re-accomodation',]+df['Goodwill',]
  # Calculate the cost of an empty room
  df['Empty-room',]<-df['Rates',]-df['Variable-costs',]
  # Calculate the critical fractile
  df['Critical-Fractile',]<-df['Cost-of-walk',]/(df['Cost-of-walk',]+df['Empty-room',])
  # Calculate the now-show table
  # Import the file
  y<-fileImport(TRUE)
  # Convert it to data frame
  df.1<-data.frame(y)
  # Calculate the col. sums and add update the df
  df.1<-rbind(df.1,colSums(df.1))
  # Split the df into dfs for different rates
  noShowTables<-list()
  for (i in 1:length(df.1)) {
    noShowTables[[i]] <-data.frame(df.1[i])
  }
  # Calculate the percentage of exact no-shows val.
  for (i in 1:length(noShowTables)) {
    noShowTables[[i]] <-cbind(noShowTables[[i]],Exact=c(noShowTables[[i]][,1]/noShowTables[[i]][,1][length(noShowTables[[i]][,1])]),moreThan=1)
    noShowTables[[i]][,3]<-c(1,rep.int(NA,length(noShowTables[[i]][,3])-1))
  }

  # Calculate the percentage of more than no-shows val.
  noShowMoreThanCal<-function(noShowTable){
    for (i in 2:length(noShowTable[,1])) {
      noShowTable[,3][i]<-round(noShowTable[,3][i-1]-noShowTable[,2][i-1],3)
    }
  # Remove the last row, which is the sum
    return(noShowTable[-length(rownames(noShowTable)),])
  }
  # Turn scientific notation to normal val. and convert the df to numerical func
  for (i in 1:length(noShowTables)) {
    noShowTables[[i]]<-data.frame(lapply(format(noShowMoreThanCal(noShowTables[[i]]),scientific = FALSE),as.numeric))
  }
  # Calculate the lookup column
  for (i in 1:length(noShowTables)) {
    noShowTables[[i]]<-cbind(noShowTables[[i]],Lookup=1-noShowTables[[i]][,3])
  }
  # Perform excel lookup and find the no. overbooked rooms
  lookupVals<-unlist( 1-df['Critical-Fractile',])
  # The loop which will apply the function (might be able to be converted to using lappy)
  lookedUpVals<-list()
  for (i in 1:length(lookupVals)) {
    # Filter for non-zero vals.
    nonZero<-noShowTables[[i]][,4]!=0
    # Filter for <= 1-critical fractile vals.
    smallerThan<-noShowTables[[i]][,4][noShowTables[[i]][,4]!=0]<=lookupVals[i]
    # Results that's non-zero and is smaller or equal to 1 - critical fractile vals.
    temp<-noShowTables[[i]][,4][nonZero][smallerThan]
    # The looked up values
    lookedUpVals<-c(lookedUpVals,temp[lookUp(lookupVals[i],temp)])
  }
  # Find the actual overbook no.
  overbookedNo<-list()
  for (i in 1:length(noShowTables)) {
    # Filter for matched vals.
    matched<-which(noShowTables[[i]][,4]==lookedUpVals[[i]])-1
    if(length(matched)==0){
      matched<-0
    }
    overbookedNo<-c(overbookedNo,matched)
  }
  # Convert the overbooked no. to df
  overbookedNo<-data.frame(overbookedNo)
  # Assign the col. & row names
  colnames(overbookedNo)<-colnames(df.1)
  rownames(overbookedNo)<-'Overbook'
  # Print the result
  cli_alert_success('Overbook Number: ')
  cat('\n')
  print(overbookedNo)
  cat('\n')

}

ons<-function(){
  # Import the file
  x<-fileImport(TRUE)
  # Convert it to data frame
  df<-data.frame(x,row.names = 1)
  # Ask for the hotel's capacity
  hotelCapacity<-toInt(inpSplit('Hotel Capacity: '))
  # Calculate the no-show percentage
  df[,'Percentage.No.Shows']<-df[,'Average.No.Shows']/df[,'Average.Arrivals']
  # Import the 2nd file
  y<-fileImport(TRUE)
  # Convert it to data frame
  df.1<-data.frame(y,row.names = 1)
  # Ask for the acceptable risks (1 out of xxx guests)
  acceptableRisk<-toInt(inpSplit('Acceptable Risk e.g.(1 out of 300) | Format:1,300: '))
  # Calculate the acceptable risk
  acceptableRisk<-acceptableRisk[1]/acceptableRisk[2]
  df.1[,'Acceptable.Risk']<-acceptableRisk
  # Populate the no. of no-shows in df.1 from df
  for (i in 1:length(df.1[,'DOW'])) {
    tempPercentageNoShows<-df[,'Percentage.No.Shows'][which(df[,'DOW']==df.1[,'DOW'][i])]
      df.1[,'Percentage.No.Shows'][i]<-tempPercentageNoShows
  }
  # Calculate the binomial distribution table
  maxNoShow<-max(df[,'Average.No.Shows'])
  # Generate the col. names for the bi Table
  biTableColNames<-seq(0,maxNoShow,1)
  biTableMatrix<-matrix(NA, nrow=length(rownames(df.1)), ncol=length(biTableColNames))
  # Create a data frame for the binomial distro. table
  biTable<-data.frame(biTableMatrix)
  colnames(biTable)<-biTableColNames
  rownames(biTable)<-rownames(df.1)
  # P(NS<=x) & Populate the binomial distro. table
  for (i in 1:length(biTable)) {
    biTable[,i]<-pbinom(biTableColNames[i],
           df.1[,'Arrivals.Forecast'],
           df.1[,'Percentage.No.Shows'],
           lower.tail = TRUE)
  }

  lookupVal<-acceptableRisk
  lookedUpVals<-list()
  for (i in 1:length(biTable[,1])) {
    # Filter for matched va.
    matched<-biTable[i,]<=lookupVal
    lookedUpVals<-c(lookedUpVals,lookUp(lookupVal,biTable[i,][matched]))
  }

  # Find the actual overbook no.
  overbookedNo<-list()
  for (i in 1:length(biTable[,1])) {

    if(is.na(lookedUpVals[[i]])){
      overbookedNo<-c(overbookedNo,NA)
    }else{
      overbookedNo<-c(overbookedNo,colnames(biTable[i,][lookedUpVals[[i]]]))
    }



  }

  # Convert the overbooked no. to df
  overbookedNo<-data.frame(overbookedNo)
  # Assign the col. & row names
  colnames(overbookedNo)<-rownames(df.1)
  rownames(overbookedNo)<-'Overbook'
  # Print the result
  cli_alert_success('Overbook Number: ')
  cat('\n')
  print(t(overbookedNo))
  cat('\n')

}

emrr<-function(){
  # Import the file
  x<-fileImport(FALSE)
  # Convert it to data frame
  df<-data.frame(x,row.names = 1)
  # Ask for the hotel's capacity
  hotelCapacity<-toInt(inpSplit('Hotel Capacity: '))
  # The optimization matrix
  opMat<-matrix(NA, nrow=5, ncol=length(df)-1)
  # Convert it to df
  opDf<-data.frame(opMat)
  # Assign row names
  rownames(opDf)<-c('Nested.Protection.Level','EMRR.Sup','EMRR.Inf','Diff','Rounded.NPL')
  # Populate nested protection lv.
  lAvgUnD<-length(df['Average.unconstrained.demand',])
  npl<-rev(df['Average.unconstrained.demand',][(lAvgUnD-2):lAvgUnD])
  opDf['Nested.Protection.Level',]<-npl
  # Populate EMRR superior
  lDf<-length(df)
  revDf<-rev(df[,(lDf-2):lDf])
  uNpl<-unlist(opDf['Nested.Protection.Level',])
  uAvgUnD<-unlist(revDf['Average.unconstrained.demand',])
  uStdev<-unlist(revDf['Standard.deviation',])
  opDf['EMRR.Sup',]<-revDf['Rate',]*(1-pnorm(uNpl,uAvgUnD,uStdev))
}

emrrEx<-function(){
  # Import the file
  x<-fileImport(FALSE)
  # Convert it to data frame
  df<-data.frame(x,row.names = 1)
  # Ask for the hotel's capacity
  hotelCapacity<-toInt(inpSplit('Hotel Capacity: '))

}


# Main Menu Selection Function
topicSelect=function(){
  menuList<-c(
    'Forecasting',
    'Group Request',
    'Linear Programming',
    'Overbooking'
  );

  choice<-menu(menuList, title='Please Select A Topic:');
  # Menu Selection Function
  mSelect<-function(topic){
    switch (topic,
            '1' = topicI(),
            '2' = topicII(),
            '3' = topicIII(),
            '4' = topicIV()
    )
  };
  mSelect(choice);

}
topicSelect()
