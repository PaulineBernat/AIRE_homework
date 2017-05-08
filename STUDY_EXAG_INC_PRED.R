#EXAG INCOME COMMON
source("lib2load.R")
source("util2load.R")

#Create a map for each zip/state and job title with annual income median.

aire.data.stream<-read.csv('aire.data.stream.withJobTitle.csv', stringsAsFactors = FALSE,header=TRUE,sep=",",na.strings = c(""))
aire.data<-read.csv('aire.data.baseline.withJobTitle.csv', stringsAsFactors = FALSE,header=TRUE,sep=",",na.strings = c(""))



#Only select Job with 5 letters or more (need a better dict of stopwords)
aire.data$job_title<-NULL
aire.data$job_title<-aire.data$JobTitle
aire.data[nchar(aire.data$JobTitle)<5,"job_title"]<-"unclear"

aire.data.stream$job_title<-NULL
aire.data.stream$job_title<-aire.data.stream$JobTitle
aire.data.stream[nchar(aire.data.stream$JobTitle)<5,"job_title"]<-"unclear"


#Find all states
all.state.av<-sort(unique(aire.data$addr_state),decreasing=TRUE)
#51 states!

#Data loose selection
#Replace all NA with 000(xx)
aire.data[is.na(aire.data$zip_code),"zip_code"]<-"000xx"
aire.data.stream[is.na(aire.data.stream$zip_code),"zip_code"]<-"000xx"
aire.data$zip.3dig<-as.numeric(str_sub(aire.data$zip_code,1,3))
aire.data.stream$zip.3dig<-as.numeric(str_sub(aire.data.stream$zip_code,1,3))

#New variable: check which application has a zip code, an income, a job and a state
aire.data$hasZip<-(aire.data$zip.3dig > 0)*1
aire.data.stream$hasZip<-(aire.data.stream$zip.3dig > 0)*1
aire.data$hasJob<-(!is.na(aire.data$emp_title))*1
aire.data$hasInc<-(!is.na(aire.data$annual_inc))*1
aire.data$hasState<-(!is.na(aire.data$addr_state))*1
aire.data$hasJobTitle<-(!grepl("not specified|unclear|unknown",aire.data$job_title))*1
aire.data.stream$hasJob<-(!is.na(aire.data.stream$emp_title))*1
aire.data.stream$hasInc<-(!is.na(aire.data.stream$annual_inc))*1
aire.data.stream$hasState<-(!is.na(aire.data.stream$addr_state))*1
aire.data.stream$hasJobTitle<-(!(grepl("not specified",aire.data.stream$job_title) | grepl("unknown",aire.data.stream$job_title) | grepl("unclear",aire.data.stream$job_title)))*1
aire.data$hasJobTitle<-(!(grepl("not specified",aire.data$job_title) | grepl("unknown",aire.data$job_title) | grepl("unclear",aire.data$job_title)))*1
aire.data$hasJobTitle<-0
aire.data[!(grepl("not specified",aire.data$job_title) | grepl("unknown",aire.data$job_title) | grepl("unclear",aire.data$job_title)),"hasJobTitle"]<-1
aire.data.stream$hasJobTitle<-0
aire.data.stream[!(grepl("not specified",aire.data.stream$job_title) | grepl("unknown",aire.data.stream$job_title) | grepl("unclear",aire.data.stream$job_title)),"hasJobTitle"]<-1




aire.data.training<-aire.data[aire.data$hasInc==1 & aire.data$hasJobTitle==1,]
library(e1071)
#New variable: Log of annual_income
aire.data.training$log.annual_inc<-log(aire.data.training$annual_inc+1)

#Map of annual income median for each pair of (addr_state,job_title)
state.inc.job<-NULL
library(dplyr)
state.inc.job = aire.data.training[,c('job_title','annual_inc','log.annual_inc','addr_state')] %>% 
  group_by(job_title,addr_state) %>% 
  summarise(n=n(), median.inc = median(annual_inc, na.rm = TRUE) , median.inc.log=median(log.annual_inc, na.rm = TRUE),
            sd.inc = sd(annual_inc, na.rm = TRUE), sd.inc.log = sd(log.annual_inc, na.rm = TRUE), skewness.inc.log = skewness(log.annual_inc, na.rm = TRUE), kurtosi.inc.log = kurtosis(log.annual_inc, na.rm = TRUE)) %>% arrange(job_title,addr_state)


state.inc.job %>% sort(state.inc.job$n)
state.inc.job<-state.inc.job[state.inc.job$n > 10,]

arrange(state.inc.job,-n)
length(unique(aire.data.training[state.inc.job$n > 0,"job_title"]))
arrange(state.inc.job[state.inc.job$n>10,],-n)

#1184*52 = 61568; 29885 (29885/61568=48.5%);7540/29885=25.5% with n > 10

state.inc.job<-arrange(state.inc.job[state.inc.job$n>10,],-n)

count_event_exag<-0
for (i in 1:nrow(aire.data.stream))
{
  if (aire.data.stream[i,"hasJob"] == 1 & aire.data.stream[i,"hasInc"] == 1 & aire.data.stream[i,"hasState"] == 1 & !(aire.data.stream[i,"job_title"] %in% c("not_specified", "Unknown")))
  {
    if(length(state.inc.job[aire.data.stream[i,"job_title"] == state.inc.job$job_title & grepl(aire.data.stream[i,"addr_state"],state.inc.job$addr_state) == TRUE,"median.inc.log"])>0)
    {
      med_salary<-NULL
      n_jobs<-NULL
      act_inc = 0
      threshold <- 0
      med_salary <- state.inc.job[aire.data.stream[i,"job_title"] == state.inc.job$job_title & grepl(aire.data.stream[i,"addr_state"],state.inc.job$addr_state) == TRUE,"median.inc.log"]
      sd_salary <- state.inc.job[aire.data.stream[i,"job_title"] == state.inc.job$job_title & grepl(aire.data.stream[i,"addr_state"],state.inc.job$addr_state) == TRUE,"sd.inc.log"]
      n_jobs <- state.inc.job[aire.data.stream[i,"job_title"] == state.inc.job$job_title & grepl(aire.data.stream[i,"addr_state"],state.inc.job$addr_state) == TRUE,"n"]
      if (nrow(med_salary) == 1) {threshold <- exp(med_salary+(2.5*sd_salary))
      med_salary<-exp(med_salary)
      if (n_jobs > 10 & aire.data.stream[i,"annual_inc"] > threshold)
      {
        print(paste("Application ",i," flag for potential exaggerated income"))
        print(paste("State = ",aire.data.stream[i,"addr_state"],", Job = ",aire.data.stream[i,"job_title"],", inc =  ",aire.data.stream[i,"annual_inc"], " [vs med(",n_jobs,") = ",med_salary,"], thr = ", threshold/med_salary*100,"%"))
        count_event_exag<-(count_event_exag+1)
      }
      }
    }
  }
}
count_event_exag
#
#3700 --> ~1% of aire.data.stream application

