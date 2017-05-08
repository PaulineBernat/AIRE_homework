source("lib2load.R")
source("util2load.R")

library(tidytext)
library(xml2)
library(rvest)
library(tidyverse)


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("qdap") #check for misspell
library(hunspell)
#Load data
aire.data<-read.csv('baseline.csv', stringsAsFactors = FALSE,header=TRUE,sep=",",na.strings = c(""))
######## DATA WORD CLEANING ############
#Aire data tranformtion first: lower case, white space, remove numbers
aire.data$emp_title_mod<-aire.data$emp_title
aire.data$emp_title_mod<-sapply(aire.data$emp_title_mod,tolower)
aire.data$emp_title_mod<-sapply(aire.data$emp_title_mod,function(x) {gsub('[[:digit:]]+', '',x)})
#remove .,/&-@
aire.data$emp_title_mod<-sapply(aire.data$emp_title_mod,function(x) {gsub("[^[:alnum:]]"," ",x)})

aire.data$emp_title_mod<-paste(" ",aire.data$emp_title_mod," ")

#stopwords and acronym (obvious ones)
aire.data$emp_title_mod<-gsub(" of "," ",aire.data$emp_title_mod)
aire.data$emp_title_mod<-gsub(" to "," ",aire.data$emp_title_mod)
aire.data$emp_title_mod<-gsub(" and "," ",aire.data$emp_title_mod)
aire.data$emp_title_mod<-gsub(" in "," ",aire.data$emp_title_mod)
aire.data$emp_title_mod<-gsub(" for "," ",aire.data$emp_title_mod)

aire.data$emp_title_mod<-gsub(" sr "," senior ",aire.data$emp_title_mod)
aire.data$emp_title_mod<-gsub(" vp "," vice president ",aire.data$emp_title_mod)
aire.data$emp_title_mod<-gsub(" v p "," vice president ",aire.data$emp_title_mod)
aire.data$emp_title_mod<-gsub(" admin "," administrative ",aire.data$emp_title_mod)
aire.data$emp_title_mod<-gsub(" dir "," director ",aire.data$emp_title_mod)

repeat{
  aire.data$emp_title_mod<-sapply(aire.data$emp_title_mod,function(x) {gsub("  "," ",x)})
  if (length(aire.data[grepl("  ",aire.data$emp_title_mod),"emp_title_mod"]) == 0) {break}
}

#Extract all emp_title (or 5 characters at least)
utitles_new<-NULL
utitles_new<-sort(unique(aire.data[nchar(aire.data$emp_title_mod) > 4,"emp_title_mod"]), decreasing=FALSE)
#Check for misspell 
word<-NULL
misspell.words<-NULL
word<-hunspell_find(utitles_new, format = "latex")
misspell.words<-unlist(word)
#37223 words misspelled
#Find suggested word
misspell.suggest.corr<-NULL
#Take a while. Should limit to max occurence of right words? But  need to suggest first... tricky
#At least, unique (length(misspell.words)) : 37223 --> 10893
#Only considered misspell of 5 letters or more --> 8247

misspell.words<-unique(misspell.words)
misspell.words.df<-data.frame(word=misspell.words)
misspell.words.df[nchar(misspell.words.df$word) < 5,"word"]<-"zyxcba"
misspell.words.vec<-as.vector(misspell.words.df$word)
misspell.words.df$word<-as.character(misspell.words.df$word)
misspell.words<-unique(misspell.words.vec)
misspell.words<-sort(misspell.words,decreasing = FALSE)

#Find suggestion for each misspell

misspell.suggest.corr<-hunspell_suggest(misspell.words, dict = dictionary(lang = "en_US", affix = NULL, cache = TRUE))
misspell.corr1<-NULL
misspell.corr1<-sapply(misspell.suggest.corr, function (x) x[1])

misspell.words.and.corr.df<-data.frame(word=misspell.words,corr=misspell.corr1)
misspell.words.and.corr.df$word<-as.character(misspell.words.and.corr.df$word)
misspell.words.and.corr.df$corr<-as.character(misspell.words.and.corr.df$corr)
#words with 3 letters or less aren't going to be useful
misspell.words.and.corr.df<-misspell.words.and.corr.df[nchar(misspell.words.and.corr.df$word)>4,]

#or
misspell.words.and.corr.df.read<-read.csv('misspell_emp_title.csv', stringsAsFactors = FALSE,header=TRUE,sep=",",na.strings = c(""))

misspell.words.and.corr.df.read<-misspell.words.and.corr.df.read[nchar(misspell.words.and.corr.df.read$word)>5,]
misspell.words<-misspell.words.and.corr.df.read$word
misspell.corr1<-misspell.words.and.corr.df.read$corr


#/or

misspell.words<-misspell.words.and.corr.df$word
misspell.corr1<-misspell.words.and.corr.df$corr

misspell.words<-paste(" ",misspell.words," ")
misspell.corr1<-paste(" ",misspell.corr1," ")
misspell.words<-substr(misspell.words,2,nchar(misspell.words)-1)
misspell.corr1<-substr(misspell.corr1,2,nchar(misspell.corr1)-1)

#length(misspell.corr1) -->9871

aire.data$need.corr<-0
for (i in 1:98)
{
  min=(i-1)*100+1
  max=i*100
  aire.data[aire.data$need.corr == 0 & grepl(paste(misspell.words[min:max], collapse='|'),aire.data$emp_title_mod),"need.corr"]<-1
  print(paste("done [",min, ":" ,max, "] :",length(aire.data[aire.data$need.corr == 1,c("X")])))
}
aire.data[aire.data$need.corr == 0 & grepl(paste(misspell.words[9801:9871], collapse='|'),aire.data$emp_title_mod),"need.corr"]<-1

#write.csv(aire.data, file = "aire.stream.dev.csv", na="")

#aire.data[grepl("vice ", aire.data$emp_title_mod)==FALSE & grepl("vice_", aire.data$emp_title_mod)==TRUE ,"emp_title_mod"]
#Vice president --> one word
#aire.data$emp_title_mod<-gsub(" vice president "," vice_president ",aire.data$emp_title_mod)
#Or replace for all vice
aire.data$emp_title_mod<-gsub(" vice "," vice_",aire.data$emp_title_mod)

aire.data$emp_title_mod.corr<-aire.data$emp_title_mod
aire.data$emp_title_mod.cont<-aire.data$emp_title_mod
aire.data.misspell<-NULL
aire.data.misspell<-aire.data[aire.data$need.corr == 1,c("X","emp_title_mod","emp_title_mod.cont","emp_title_mod.corr")]

for (i in 1:length(misspell.corr1))
{
  #print(paste("line ",i," replacing -",misspell.words[i],"-by-",misspell.corr1[i],"-"))
  aire.data.misspell[aire.data.misspell$emp_title_mod.corr == aire.data.misspell$emp_title_mod.cont,"emp_title_mod.corr"]<-gsub(misspell.words[i],misspell.corr1[i],aire.data.misspell[aire.data.misspell$emp_title_mod.corr == aire.data.misspell$emp_title_mod.cont,"emp_title_mod.corr"])
}

aire.data.misspell$X2<-aire.data.misspell$X
aire.data.misspell$X<-NULL
aire.data.misspell$emp_title_mod2<-aire.data.misspell$emp_title_mod
aire.data.misspell$emp_title_mod<-NULL
aire.data.misspell$emp_title_mod.cont2<-aire.data.misspell$emp_title_mod.cont
aire.data.misspell$emp_title_mod.cont<-NULL
aire.data.misspell$emp_title_mod.corr2<-aire.data.misspell$emp_title_mod.corr
aire.data.misspell$emp_title_mod.corr<-NULL

aire.data.all<-merge(aire.data, aire.data.misspell, by.x=c("X"), by.y=c("X2"), all.x=TRUE) 

aire.data.all[is.na(aire.data.all$emp_title_mod.corr2),"emp_title_mod.corr2"]<-"no_rep"
#aire.data.all$emp_title_mod.corr<-aire.data.all$emp_title_mod
aire.data.all[!grepl("no_rep",aire.data.all$emp_title_mod.corr2),]$emp_title_mod.corr<-aire.data.all[!grepl("no_rep",aire.data.all$emp_title_mod.corr2),]$emp_title_mod.corr2


#Remove extra space at beginning and end
#aire.data.all$emp_title_mod.corr<-substr(aire.data.all$emp_title_mod.corr,2,nchar(aire.data.all$emp_title_mod.corr)-1)
#aire.data.all$emp_title_mod.cont<-substr(aire.data.all$emp_title_mod.cont,2,nchar(aire.data.all$emp_title_mod.cont)-1)
#aire.data.all$emp_title_mod<-substr(aire.data.all$emp_title_mod,2,nchar(aire.data.all$emp_title_mod)-1)
#Better name
aire.data.all$emp_title.corr<-aire.data.all$emp_title_mod.corr
#aire.data.all[!is.na(aire.data.all$emp_title_mod.corr),"emp_title.corr"]<-aire.data.all$emp_title_mod.corr
aire.data.all[is.na(aire.data.all$emp_title),"emp_title.corr"]<-" not_specified "

repeat{
  aire.data.all$emp_title.corr<-sapply( aire.data.all$emp_title.corr,function(x) {gsub("  "," ",x)})
  if (length(aire.data.all[grepl("  ",aire.data.all$emp_title.corr),"emp_title.corr"]) == 0) {break}
}


all.1wd.emp_title_n <-NULL
all.2wd.emp_title_n <-NULL
all.3wd.emp_title_n <-NULL

utitles_n<-NULL
all.emp_title_n<-NULL
utitles_n<-sort(unique(aire.data.all$emp_title.corr), decreasing=FALSE)
all.emp_title_n = data.frame(utitles_n)
all.emp_title_n$utitles_n<-as.character(all.emp_title_n$utitles_n)
#After cleaning: how many unique title: 167891->138023 (need to run full corre)
#Unnest words and biwords
all.1wd.emp_title_n <- all.emp_title_n %>% unnest_tokens(words, "utitles_n") 
all.2wd.emp_title_n <- all.emp_title_n %>% unnest_tokens(biwords, "utitles_n",token = "ngrams", n = 2)
all.3wd.emp_title_n <- all.emp_title_n %>% unnest_tokens(triwords, "utitles_n",token = "ngrams", n = 3)
#sort words and biwords
all.1wd.emp_title_n.order <-all.1wd.emp_title_n %>% count(words, sort=TRUE)
all.2wd.emp_title_n.order <-all.2wd.emp_title_n %>% count(biwords, sort=TRUE)
all.3wd.emp_title_n.order <-all.3wd.emp_title_n %>% count(triwords, sort=TRUE)




all.2wd.emp_title_n.order<-all.2wd.emp_title_n.order[nchar(all.2wd.emp_title_n.order$biwords)>4,]
max_search_2w<-nrow(all.2wd.emp_title_n.order[all.2wd.emp_title_n.order$n > 40,"biwords"])

#Put a threshold. At least 5 letters for a words of biwords
aire.data.all$JobTitle<-"Unknown"

#To avoid bits of words, add space
all.1wd.emp_title_n.order$words<-paste(" ",all.1wd.emp_title_n.order$words," ")
all.2wd.emp_title_n.order$biwords<-paste(" ",all.2wd.emp_title_n.order$biwords," ")

all.1wd.emp_title_n.order$words<-substr(all.1wd.emp_title_n.order$words,2,nchar(all.1wd.emp_title_n.order$words)-1)
all.2wd.emp_title_n.order$biwords<-substr(all.2wd.emp_title_n.order$biwords,2,nchar(all.2wd.emp_title_n.order$biwords)-1)

#Using biwords, give a Job title to each application
for (i in 1:max_search_2w)
{
  job_i = as.character(all.2wd.emp_title_n.order[i,"biwords"])
  print(paste("row",i," : ",job_i))
  aire.data.all$JobTitle[grepl(job_i,tolower(aire.data.all$emp_title.corr)) == TRUE & grepl("unknown",tolower(aire.data.all$JobTitle)) == TRUE]<-job_i
}

#Using 1 word, need to redefine the list. For instance senior should have been used a lot
all.1wd.emp_title_n.order<-all.1wd.emp_title_n.order[nchar(all.1wd.emp_title_n.order$words)>4,]
max_search_1w<-nrow(all.1wd.emp_title_n.order[all.1wd.emp_title_n.order$n > 40,"words"])
all.1wd.emp_title_n.order$n_new<-all.1wd.emp_title_n.order$n


for (i in 1:max_search_1w)
{
  word_count<-all.1wd.emp_title_n.order[i,"n"] 
  word_ind<-all.1wd.emp_title_n.order[i,"words"] 
  tot_count<-0
  print(paste("doing row",i," w/ ",word_ind))
  for (j in 1:max_search_2w)
  {
    if (grepl(word_ind,all.2wd.emp_title_n.order[j,"biwords"]))
    {
      tot_count<-tot_count+all.2wd.emp_title_n.order[j,"n"]
    }
  }
  all.1wd.emp_title_n.order[i,"n_new"]<-all.1wd.emp_title_n.order[i,"n"]-tot_count
}



all.1wd.emp_title_n.order<-all.1wd.emp_title_n.order[order(-all.1wd.emp_title_n.order$n_new),]
max_search_1w<-nrow(all.1wd.emp_title_n.order[all.1wd.emp_title_n.order$n_new > 40,"words"])
for (i in 1:max_search_1w)
{
  job_i = as.character(all.1wd.emp_title_n.order[i,"words"])
  print(paste("row",i," : ",job_i))
  aire.data.all$JobTitle[grepl(job_i,tolower(aire.data.all$emp_title.corr)) == TRUE & grepl("unknown",tolower(aire.data.all$JobTitle)) == TRUE]<-job_i
}


aire.data.all[grepl("Unknown",aire.data.all$JobTitle),"JobTitle"]

#RESULT: How many job not found
aire.data.all[grepl("Unknown",aire.data.all$JobTitle),"JobTitle"]<-" unknown "
aire.data.all[is.na(aire.data.all$emp_title),"JobTitle"]<-" not specified "
aire.data.all$JobTitle<-substr(aire.data.all$JobTitle,2,nchar(aire.data.all$JobTitle)-1)

aire.data.all[grepl(" unknown ",aire.data.all$JobTitle),"JobTitle"]<-"unknown"
#How many not found in dictionnary 30620
length(aire.data.all[grepl("unknown",aire.data.all$JobTitle),"JobTitle"])
#How many not specifed  52473
length(aire.data.all[grepl("not specified",aire.data.all$JobTitle),"JobTitle"])
#check how many not defined
length(aire.data.all[is.na(aire.data.all$emp_title),"JobTitle"])
#How many unique 1364
length(unique(aire.data.all[is.character(aire.data.all$JobTitle),"JobTitle"]))




length(aire.data.all[grepl("not_specified",aire.data.all$emp_title),"JobTitle"])

#Left to do: acronyms, mirror biwords, stem

PlotCategoryData("JobTitle",aire.data.all)
write.csv(aire.data.all, file = "aire.data.baseline.withJobTitle.csv", na="")

all.1wd.emp_title_n.order$words<-substr(all.1wd.emp_title_n.order$words,2,nchar(all.1wd.emp_title_n.order$words)-1)
all.2wd.emp_title_n.order$biwords<-substr(all.2wd.emp_title_n.order$biwords,2,nchar(all.2wd.emp_title_n.order$biwords)-1)