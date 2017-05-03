#Flag exagerated income, compare annual_inc given by borrower to estimated income
#Estimated income: distribution log normal. 
#Assumption: salary is correlation with the job title and the geo area (State, or zip) 

#Create a map for each (zip,job_title) find the med income, the mid grade, the standard deviation (if available)
#Job Titles: use emp_title
#length(unique(aire.data$emp_title))
#Too messy! Find words with most occurence in training data
library(tidytext)
library(xml2)
library(rvest)
library(tidyverse)
all.emp_title<-NULL
all.1wd.emp_title <-NULL
all.2wd.emp_title <-NULL
utitles<-NULL
utitles<-sort(unique(aire.data$emp_title), decreasing=FALSE)
all.emp_title = data.frame(utitles)
all.emp_title$utitles<-as.character(all.emp_title$utitles)
#Unnest words
all.1wd.emp_title <- all.emp_title %>% unnest_tokens(words, "utitles") 
all.2wd.emp_title <- all.emp_title %>% unnest_tokens(biwords, "utitles",token = "ngrams", n = 2)

#clean list of words:
# - remove small words and stop_words (limitation: will miss some abreviation)
# - what about the misspell?
# - which word has priority within a list? Manager Assistant --> Manager or Assitant?

all.1wd.emp_title[nchar(all.1wd.emp_title$words) < 4 | all.1wd.emp_title$words %in% stop_words$word,"words"]<-'wxyz'
all.1wd.emp_title.order <-all.1wd.emp_title %>% count(words, sort=TRUE)
all.2wd.emp_title.order <-all.2wd.emp_title %>% count(biwords, sort=TRUE)

#Visualise?
library(RColorBrewer)
library(wordcloud)
#wordcloud(all.1wd.emp_title.order$words, all.1wd.emp_title.order$n , max.words = 70, colors = "purple", scale = c(3, .5))

#Remove trash data, treat special case (tech = technician?)
all.1wd.emp_title[grepl("tech",all.1wd.emp_title$words)==TRUE,"words"]<-'technician'
all.1wd.emp_title[grepl("asst",all.1wd.emp_title$words)==TRUE,"words"]<-'assistant'
all.1wd.emp_title[grepl("management",all.1wd.emp_title$words)==TRUE,"words"]<-'manager'
all.1wd.emp_title[grepl("engineering",all.1wd.emp_title$words)==TRUE,"words"]<-'engineer'
all.1wd.emp_title[grepl("administrator",all.1wd.emp_title$words)==TRUE,"words"]<-'admin'
all.1wd.emp_title[grepl("office",all.1wd.emp_title$words)==TRUE,"words"]<-'office_job'
all.1wd.emp_title[grepl("service",all.1wd.emp_title$words)==TRUE,"words"]<-'service_job'
all.1wd.emp_title.clean.order <-NULL
all.1wd.emp_title.clean.order <-all.1wd.emp_title %>% count(words, sort=TRUE)

all.1wd.emp_title.clean.order[grepl("wxyz",all.1wd.emp_title.clean.order$words),"n"]<-0

all.1wd.emp_title.clean.order<-all.1wd.emp_title.clean.order %>% as_tibble %>% arrange(desc(as.numeric(n)))

wordcloud(all.1wd.emp_title.clean.order$words, all.1wd.emp_title.clean.order$n , max.words = 70, colors = "purple", scale = c(3, .5))

#Check cleaned data
#? : how many entries to decide to use the title
#63 words appear at least 1000 times. Use 70 first entry

#list.job.titles<-list.job.titles %>% as_tibble %>% arrange(desc(as.numeric(n)))
list.job.titles<-all.1wd.emp_title.clean.order[1:70,]

#Look for job title, once got one don't change (Need to rethink that!!!)
#Problem of priority. asst and assistant should overcome manager (for instance); 
list.job.titles$priority<-0
list.job.titles[grepl("assistant", list.job.titles$words) == TRUE, "priority"]<-1  
list.job.titles<-list.job.titles %>% as_tibble %>% arrange(desc(as.numeric(priority)))

for (i in 1:70)
{
  job_i = as.character(list.job.titles[i,"words"])
  aire.data$JobTitle[grepl(list.job.titles[i,"words"],tolower(aire.data$emp_title)) == TRUE & grepl("unknown",tolower(aire.data$JobTitle)) == TRUE]<-job_i
  aire.stream.data$JobTitle[grepl(list.job.titles[i,"words"],tolower(aire.stream.data$emp_title)) == TRUE & grepl("unknown",tolower(aire.stream.data$JobTitle)) == TRUE]<-job_i
}

#aire.data %>% count(JobTitle, sort=TRUE)
#Look for the special cases
aire.data$JobTitle[grepl("asst",aire.data$emp_title)==TRUE & grepl("Unknown",aire.data$JobTitle)==TRUE]<-'assistant'
aire.data$JobTitle[grepl("administrator",aire.data$emp_title)==TRUE & grepl("Unknown",aire.data$JobTitle)==TRUE]<-'admin'
aire.data$JobTitle[grepl("tech",aire.data$emp_title)==TRUE & grepl("Unknown",aire.data$JobTitle)==TRUE]<-'technician'
aire.data$JobTitle[grepl("engineering",aire.data$emp_title)==TRUE & grepl("Unknown",aire.data$JobTitle)==TRUE]<-'engineer'
aire.data$JobTitle[grepl("management",aire.data$emp_title)==TRUE & grepl("Unknown",aire.data$JobTitle)==TRUE]<-'manager'
aire.data$JobTitle[grepl("not_specified",aire.data$emp_title)==TRUE & grepl("Unknown",aire.data$JobTitle)==TRUE]<-'not_specified'
aire.data$JobTitle[grepl("office",aire.data$emp_title)==TRUE & grepl("Unknown",aire.data$JobTitle)==TRUE]<-'office_job'
aire.data$JobTitle[grepl("service",aire.data$emp_title)==TRUE & grepl("Unknown",aire.data$JobTitle)==TRUE]<-'service_job'

PlotCategoryData("JobTitle",aire.data)
#159383 unknown (~23%)
#52473 not_specified (~8%)

aire.stream.data$JobTitle[grepl("asst",aire.stream.data$emp_title)==TRUE & grepl("Unknown",aire.stream.data$JobTitle)==TRUE]<-'assistant'
aire.stream.data$JobTitle[grepl("administrator",aire.stream.data$emp_title)==TRUE & grepl("Unknown",aire.stream.data$JobTitle)==TRUE]<-'admin'
aire.stream.data$JobTitle[grepl("tech",aire.stream.data$emp_title)==TRUE & grepl("Unknown",aire.stream.data$JobTitle)==TRUE]<-'technician'
aire.stream.data$JobTitle[grepl("engineering",aire.stream.data$emp_title)==TRUE & grepl("Unknown",aire.stream.data$JobTitle)==TRUE]<-'engineer'
aire.stream.data$JobTitle[grepl("management",aire.stream.data$emp_title)==TRUE & grepl("Unknown",aire.stream.data$JobTitle)==TRUE]<-'manager'
aire.stream.data$JobTitle[grepl("not_specified",aire.stream.data$emp_title)==TRUE & grepl("Unknown",aire.stream.data$JobTitle)==TRUE]<-'not_specified'
aire.stream.data$JobTitle[grepl("office",aire.stream.data$emp_title)==TRUE & grepl("Unknown",aire.stream.data$JobTitle)==TRUE]<-'office_job'
aire.stream.data$JobTitle[grepl("service",aire.stream.data$emp_title)==TRUE & grepl("Unknown",aire.stream.data$JobTitle)==TRUE]<-'service_job'

#Assumption: Annual income varies with job and state or zip code.
#Create a map for each zip/state and job title with annual income median.
#Find all states
all.state.av<-sort(unique(aire.data$addr_state),decreasing=TRUE)
#51 states!
state.inc.job<-NULL
library(e1071)
#For average income a) should check if data are verified and if income available, 
#b) compute log of annual_inc, should check the normality. 
#aire.data.verified<-aire.data[aire.data$verification_status %in% c("Verified","Source Verified") & !is.na(aire.data$annual_inc),]
aire.data.verified<-aire.data[aire.data$hasInc==1,]
aire.data.verified$Log.annual_inc<-log(aire.data.verified$annual_inc+1)

#Map of annual income median for each pair of (state,job)
state.inc.job = aire.data.verified[,c('JobTitle','annual_inc','Log.annual_inc','addr_state')] %>% 
  group_by(JobTitle,addr_state) %>% 
  summarise(n=n(), median.inc = median(annual_inc, na.rm = TRUE) , median.inc.log=median(Log.annual_inc, na.rm = TRUE),
            sd.inc = sd(annual_inc, na.rm = TRUE), sd.inc.log = sd(Log.annual_inc, na.rm = TRUE), skewness.inc.log = skewness(Log.annual_inc, na.rm = TRUE), kurtosi.inc.log = kurtosis(Log.annual_inc, na.rm = TRUE)) %>% arrange(JobTitle,addr_state)

#Map of annual income median for each pair of (zip,job)
zip.inc.job = aire.data.verified[,c('JobTitle','annual_inc','Log.annual_inc','zip.3dig')] %>% 
  group_by(JobTitle,zip.3dig) %>% 
  summarise(n=n(), median.inc = median(annual_inc, na.rm = TRUE) , median.inc.log=median(Log.annual_inc, na.rm = TRUE),
            sd.inc = sd(annual_inc, na.rm = TRUE), sd.inc.log = sd(Log.annual_inc, na.rm = TRUE)) %>% arrange(JobTitle,zip.3dig)

#Add the grade
#ReplaceMissingValues("grade",aire.data)
#col.gr<-c("grade")
#aire.data = ReplaceCatbyVal(col.gr,aire.data,qual.list)
#qual.list<-c('None'=0,'G'=1,'F'=2,'E'=3,'D'=4,'C'=5,'B'=6,'A'=7)

#3229 pair found out of 3570 (=51*70)
#1897/3229 have 20 entries or more
nrow(state.inc.job[state.inc.job$n > 19, "addr_state"])

#Look for event with exagerated income (>med_salary+2*sigma)
#Condition: have an income! and a JobTitle. And there is a threshold for the pair (state, JobTitle), at least 20 entries per pair.

#state.inc.job[grepl("teacher",state.inc.job$JobTitle) & grepl("NY",state.inc.job$addr_state),]

count_event_exag<-0
for (i in 1:nrow(aire.stream.data))
{
  if (aire.stream.data[i,"hasJob"] == 1 & aire.stream.data[i,"hasInc"] == 1 & aire.stream.data[i,"hasState"] == 1 & !(aire.stream.data[i,"JobTitle"] %in% c("not_specified", "Unknown")))
  {
    if(length(state.inc.job[grepl(aire.stream.data[i,"JobTitle"],state.inc.job$JobTitle) == TRUE & grepl(aire.stream.data[i,"addr_state"],state.inc.job$addr_state) == TRUE,"median.inc"])>0)
    {
      med_salary<-NULL
      n_jobs<-NULL
      act_inc = 0
      threshold <- 0
      med_salary <- state.inc.job[grepl(aire.stream.data[i,"JobTitle"],state.inc.job$JobTitle) == TRUE & grepl(aire.stream.data[i,"addr_state"],state.inc.job$addr_state) == TRUE,"median.inc"]
      sd_salary <- state.inc.job[grepl(aire.stream.data[i,"JobTitle"],state.inc.job$JobTitle) == TRUE & grepl(aire.stream.data[i,"addr_state"],state.inc.job$addr_state) == TRUE,"sd.inc"]
      n_jobs <- state.inc.job[grepl(aire.stream.data[i,"JobTitle"],state.inc.job$JobTitle) == TRUE & grepl(aire.stream.data[i,"addr_state"],state.inc.job$addr_state) == TRUE,"n"]
      if (nrow(med_salary) == 1) {threshold <- med_salary+(2*sd_salary)
      if (n_jobs > 20 & aire.stream.data[i,"annual_inc"] > threshold)
      {
        print(paste("Application ",i," flag for potential exaggerated income"))
        print(paste("State = ",aire.stream.data[i,"addr_state"],", Job = ",aire.stream.data[i,"JobTitle"],", inc =  ",aire.stream.data[i,"annual_inc"], " [vs med(",n_jobs,") = ",med_salary,"]"))
        count_event_exag<-(count_event_exag+1)
      }
      }
    }
  }
}

#Check how many flag:
count_event_exag
#~3-4% of aire.stream.data application

#Bug
#state.inc.job[grepl("president",state.inc.job$JobTitle) == TRUE & grepl("ID",state.inc.job$addr_state) == TRUE,]
