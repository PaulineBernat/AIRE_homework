#Show number of missing entries per variables in df in decreasing number
SortColWithMissData = function(df)
{
  na.cols <- which(colSums(is.na(df)) > 0)
  sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)
}

#Info about a variable (numeric or category - normality)
GetInfoVar=function(cols,df)
{
  for(col in cols)
  {
    n_miss_entry <-colSums(sapply(df[col],is.na))
    n_unique_entry <-length(unique(df[,col]))
    print(paste("Variable ",col, " has: "))
    print(paste("Number of missing data ",n_miss_entry))
    print(paste("Number of unique data ",n_unique_entry))
    is.num<-(sapply(df[col],is.numeric))*1
    if (is.num == 1 ) 
    {
      print(paste("Data is numeric "))
      nzer = length(which(df[col] == 0))
      print(paste(nzer, " values at 0"))
      #Investigate Skweness of distribution
      sk=skewness(df[,col],na.rm=TRUE)
      ku=kurtosis(df[,col],na.rm=TRUE)
      print(paste("Skewness= ",sk, ", Kurtosis = ",ku))
      if (abs(sk>0.8 & abs(ku)>3))
      {
        log.df<-log(df[,col]+1)
        sk=skewness(log.df,na.rm=TRUE)
        ku=kurtosis(log.df,na.rm=TRUE)
        if (abs(sk>0.8 & abs(ku)>3)) 
        { print(paste("--> Log (NORMAL) : Skewness= ",sk, ", Kurtosis = ",ku))
        }
        else{
          print(paste("--> Log values : Skewness= ",sk, ", Kurtosis = ",ku))
        }
      }
      else{
        print(paste("Distribution is normal"))
      }
      PlotNumericData(col,df)
    }
    else
    {
      print(paste("Data is categorical"))
      PlotCategoryData(col,df)
    }
  }
}
#Plot of the Data when variable is categorical
PlotCategoryData = function(cols, df){
  for (col in cols) {
    order.cols = names(sort(table(df[,col]), decreasing = TRUE))
    num.plot = qplot(df[,col]) +
      geom_bar(fill = 'lightblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
    print(num.plot)
  }
}

#Plot of the Data when variable is numerical
PlotNumericData = function(cols, df){
  for (col in cols) {
    num.plot = qplot(df[,col]) +
      geom_histogram(fill='lightblue',color='black') +
      theme_minimal() +
      xlab(col) 
    print(num.plot)
  }
}

#Plot log of the Data when variable is numerical
PlotLogNumericData = function(cols, df){
  for (col in cols) {
      log.col<-log(df[,col])
      num.plot = qplot(log.col) +
      geom_histogram(fill='lightblue',color='black') +
      theme_minimal() +
      xlab(col) 
    print(num.plot)
  }
}

#Replace missing data with 0 or none in column
ReplaceMissingValues = function(cols,df){
  for (col in cols)
  { if (sapply(df[col],is.numeric) == TRUE)
  {
    df[sapply(df[col],is.na),col]<-0
  }
    else{
      df[sapply(df[col],is.na),col]<-'None'
      df[sapply(df[col],is.null),col]<-'None'
    }
  }
  return(df)
}

#Group by Int Rate
group.int_rate = function(col,df){
  group.int_rate.table = df[,c(col,"int_rate")] %>% group_by_(col) %>% summarise(mean.IntRate=round(mean(int_rate),2),n=n()) %>% arrange(mean.IntRate)
  
  print(qplot(x=reorder(group.int_rate.table[[col]],group.int_rate.table[['mean.IntRate']]),y=group.int_rate.table[['mean.IntRate']]) + theme_minimal() +labs(x=col,y='Mean Int Rate') + theme(axis.text.x = element_text(angle=45)) + geom_bar(stat='identity', fill='lightblue'))
  
  return(data.frame(group.int_rate.table))
}

#Replace cataegorical values by numeric value using the val_list map
ReplaceCatbyVal = function(cols,df,val_list)
{
  for (col in cols)
  {
    df[,col]<-as.numeric(val_list[df[,col]])
  }
  return(df)
}
