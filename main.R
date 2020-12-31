#!/usr/bin/env Rscript
# Load libraries
library(cli)
library(dplyr)

# Welcome message
welcomeMsg<-'Hi Welcome to the RM Terminal by Aviv'
cli::cat_boxx(welcomeMsg)

# Topic 1 (Forecasting)
# Main Menu List
menuListT1<-c(
  'Occupancy Forecast with LOS and Pick-Ups',
  'Back'
)

# Topic I menu
topicI<-function(){
  choice<-menu(menuListT1,title='What do you need?')
  switch (choice,
          '1' = {occForecast(); cat("\n");topicI()},
          '2'={topicI()}
  )
}

occForecast<-function(){
  # Import the file
  filex<-file.choose()
  # Fix newline problem
  cat("\n", file = filex, append = TRUE)
  # Read the file as CSV
  x<-read.csv(file=filex,header = FALSE)
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
   orderedDF<-rbind(pp,df[df$'100' ==dsow,])
  }
  # Split the ordered df into separate dfs grouped by DOW + remove NAs
  splitedOrderedDf<-list()
  for (i in 1:length(DOW)) {
    splitedOrderedDf[[i]]<-na.omit(subset(pp,pp$'100' %in% DOW[i]))
  }

  oo<-list()
  # Calculate the col means of each DBA values to create the avg booking curve table
  for (i in 1:length(splitedOrderedDf)) {
   oo[[i]] <-as.data.frame(
     colMeans(
     data.frame(splitedOrderedDf[i])
     [,2:length(colnames(data.frame(splitedOrderedDf[i])))]
     )
     ,optional = TRUE,
   )
  }

  BCT<-list()
  # Calculate the col means of each DBA values to create the avg booking curve table
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

  # Combine, transpose, round all to form a BCT
  BCTF<-round(t(data.frame(BCT)))

}



# Main Menu Selection Function
topicSelect=function(){
  menuList<-c(
    'Forecasting'
  );

  choice<-menu(menuList, title='Please Select A Topic:');
  # Menu Selection Function
  mSelect<-function(topic){
    switch (topic,
            '1' = topicI(),
    )
  };
  mSelect(choice);

}
topicSelect()
