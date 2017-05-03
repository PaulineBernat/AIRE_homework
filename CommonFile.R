#Common file
source("lib2load.R")
source("util2load.R")

#Load data
aire.data<-read.csv('baseline.csv', stringsAsFactors = FALSE,header=TRUE,sep=",",na.strings = c(""))
aire.stream.data<-read.csv('stream.csv', stringsAsFactors = FALSE,header=TRUE,sep=",",na.strings = c(""))
#Load external zip data (for visualition only)
zip.map.data<-read.csv('zip_code_database.csv', stringsAsFactors = FALSE,header=TRUE,sep=",",na.strings = c(""))


#Data compatibility: create a numeric 3-digits zip
zip.data<-zip.map.data[grepl("STANDARD",zip.map.data$type) == TRUE,]
zip.data$zip.3dig<-as.integer(round(zip.data$zip/100,0))

#Replace all NA with 000(xx)
aire.data[is.na(aire.data$zip_code),"zip_code"]<-"000xx"
aire.stream.data[is.na(aire.stream.data$zip_code),"zip_code"]<-"000xx"
aire.data$zip.3dig<-as.numeric(str_sub(aire.data$zip_code,1,3))
aire.stream.data$zip.3dig<-as.numeric(str_sub(aire.stream.data$zip_code,1,3))

#New variable: check which application has a zip code, an income, a job and a state
aire.data$hasZip<-(aire.data$zip.3dig > 0)*1
aire.stream.data$hasZip<-(aire.stream.data$zip.3dig > 0)*1
aire.data$hasJob<-(!is.na(aire.data$emp_title))*1
aire.data$hasInc<-(!is.na(aire.data$annual_inc))*1
aire.data$hasState<-(!is.na(aire.data$addr_state))*1
aire.stream.data$hasJob<-(!is.na(aire.stream.data$emp_title))*1
aire.stream.data$hasInc<-(!is.na(aire.stream.data$annual_inc))*1
aire.stream.data$hasState<-(!is.na(aire.stream.data$addr_state))*1

#Check if emp_title is NA
aire.data[is.na(aire.data$emp_title),"emp_title"]<-'not_specified'
aire.stream.data[is.na(aire.stream.data$emp_title),"emp_title"]<-'not_specified'
length(aire.data[grepl("not_specified",aire.data$emp_title),"emp_title"])
#52473 missing in aire.data (~<10%)
length(aire.stream.data[grepl("not_specified",aire.stream.data$emp_title),"emp_title"])
#29481 missing in aire.stream.data (~<10%)

#New variable: JobTitle
aire.data$JobTitle<-NULL
aire.data$JobTitle<-'Unknown'
aire.stream.data$JobTitle<-NULL
aire.stream.data$JobTitle<-'Unknown'