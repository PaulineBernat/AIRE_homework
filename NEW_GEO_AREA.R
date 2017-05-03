
aire.zip<-sort(unique(aire.data[aire.data$zip.3dig > 0,"zip.3dig"]),decreasing=TRUE)
aire.stream.zip<-sort(unique(aire.stream.data[aire.stream.data$zip.3dig > 0,"zip.3dig"]),decreasing=TRUE)
#normalise vectors! difference of vectors length?
aire.stream.zip<-c(aire.stream.zip,rep(999,37))
aire.zip<-c(aire.zip,rep(999,15))
zip.to.explore.id<-length(setdiff(zip.data$zip.3dig,aire.zip))
zip.found.id<-length(setdiff(aire.stream.zip,aire.zip))


#############################################################
#######  In total, 941 areas in 50 states ###################
####### 37 areas to explore - 14 found in the stream ########
####### 4% areas to explore - 1.5% found in the stream ######
#############################################################

library(spatstat)
library(maptools)

zip.data<-zip.data[zip.data$latitude > 5 & zip.data$latitude < 72 & zip.data$longitude > -180 & zip.data$longitude < -65,]
zip.data<-zip.data[!(zip.data$latitude==0.00 | zip.data$longitude==0.00),]

list_unique_zip<-sort(unique(zip.data$zip.3dig),decreasing=TRUE)

squares<-NULL 
for (ind_zip in list_unique_zip)
{
  list.point<-NULL
  list.point<-zip.data[zip.data$zip.3dig == ind_zip,c("latitude","longitude")]
  for (i in list.point)
  {
    x_min = min(list.point$longitude)
    x_max = max(list.point$longitude)
    y_min = min(list.point$latitude)
    y_max = max(list.point$latitude)
    
    p1<-c(x_min,y_min)
    p2<-c(x_min,y_max)
    p3<-c(x_max,y_max)
    p4<-c(x_max,y_min)
    
    squares<-rbind(squares,t({
      p1
      c(p1,p2,p3,p4,p1)}
    ))
    
  }}
squares<-unique(squares)
ID <- paste0('sq', seq_len(nrow(squares)))

# Create Spatial Polygones
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(squares, row(squares)), ID))

# Visualise
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
plot(polys.df, col=rainbow(50, alpha=0.5))
#Add text elements -->  text(coordinates(polys),ID,cex=0.25) 

#Check for new areas: any zip found in the stream data.

zip.inc.job.count<-zip.inc.job[,c("JobTitle","zip.3dig","n")]

count_new_events<-0
for (i in 1:nrow(aire.stream.data))
{
  if (aire.stream.data[i,"hasJob"] == 1 & aire.stream.data[i,"hasInc"] == 1 & aire.stream.data[i,"hasState"] == 1  & aire.stream.data[i,"hasZip"] == 1 & !(aire.stream.data[i,"JobTitle"] %in% c("not_specified", "Unknown")))
  {
    n_jobs<-NULL
    nj<-0
    total.nj<-0
    n_jobs <- zip.inc.job.count[grepl(aire.stream.data[i,"JobTitle"],zip.inc.job.count$JobTitle) == TRUE & grepl(aire.stream.data[i,"zip.3dig"],zip.inc.job.count$zip.3dig) == TRUE,"n"]
    # n_jobs <- state.inc.job[grepl(aire.stream.data[i,"JobTitle"],zip.inc.job$JobTitle) == TRUE & grepl(aire.stream.data[i,"zip.3dig"],zip.inc.job$zip.3dig) == TRUE,"n"]
    if (nrow(n_jobs) == 1) {nj = n_jobs}
    if (nj == 0)
    {
      print(paste("row ",i," --> New area found: zip ",aire.stream.data[i,"zip_code"], " Job : ", aire.stream.data[i,"JobTitle"]," (emp: ",aire.stream.data[i,"emp_title"],")"))
      if (nrow(state.inc.job[grepl(aire.stream.data[i,"JobTitle"],state.inc.job$JobTitle) == TRUE & grepl(aire.stream.data[i,"addr_state"],state.inc.job$addr_state) == TRUE,"median.inc"]) > 0) {
      print(paste("zip in state ",aire.stream.data[i,"addr_state"], " Average income (pop=", state.inc.job[grepl(aire.stream.data[i,"JobTitle"],state.inc.job$JobTitle) == TRUE & grepl(aire.stream.data[i,"addr_state"],state.inc.job$addr_state) == TRUE,"n"], ") :", state.inc.job[grepl(aire.stream.data[i,"JobTitle"],state.inc.job$JobTitle) == TRUE & grepl(aire.stream.data[i,"addr_state"],state.inc.job$addr_state) == TRUE,"median.inc"] ))}
      else {print(paste("zip in state ",aire.stream.data[i,"addr_state"], ": no income available yet"))}
      count_new_events<-(count_new_events+1)
    }
    total.nj <- (nj + 1)
    zip.inc.job.count[grepl(aire.stream.data[i,"JobTitle"],zip.inc.job.count$JobTitle) == TRUE & grepl(aire.stream.data[i,"zip.3dig"],zip.inc.job.count$zip.3dig) == TRUE,"n"]<-total.nj
  }
}


#Check how many flag:
count_new_events
#~7-8% of aire.stream.data application