
countingReport<-function(roomuse,maxTime) {
  rooms<-sort(as.character(unique(roomuse$roomname)))
  educ<-as.character(unique(roomuse$school_id))
  title<-as.character(unique(paste(roomuse$title,roomuse$school_id,sep="_")))
  o<-order(educ)
  educ<-educ[o]
  title<-title[o]
  mat<-matrix(0,nrow=length(rooms)+1,ncol=length(educ),dimnames=list(room=c("Alle",rooms),educ=title))



  for (r in 1:length(rooms)) {
    for (e in 1:length(educ)) {
      room<-rooms[r]
      edu<-educ[e]
      rows<-subset(roomuse,roomname==room & school_id==edu)
      #don't count minutes after maxTime, and make sure that lesson starting after maxtime don't generate
      #negativ values
      rows$lessonCount<-pmax(0,(pmin(rows$timeto,maxTime)-rows$timefrom))/45
      c<-round(sum(rows$lessonCount),digits = 1)
      mat[r+1,e]<-c
    }
  }

  #calculate total
  for (e in 1:length(educ)) {
    c<-sum(mat[,e])
    mat[1,e]<-c
  }


  #writeTable(mat,rowNames=T,showHeader=T,stringAsText = T)
  mat
}



#' createCountingReport
#'
#' @param campuses list of campuses
#' @param weekselection weekselection
#' @param maxTime don't count minutes after maxTime
#'
#' @return nothing
#' @export
#' @import xlsx
#' @import RMariaDB
#' @import gplots
#'
#' @examples xxxx
createCountingReport<-function(campuses,dateSelection,maxTime) {
  warning("filplacering hardcodet")
  file<-"c:/Users/THBE/Desktop/lokalerapport.xlsx"
  if (file.exists(file)) {
    file.remove(file)
  }

  description <-data.frame(x=c("Lokaleoptælling",
            "Antal lektioner a 45 minutter",
            paste("Mandag til fredag, kl 8-??"),
            "Angiv optalte uger her"))
  write.xlsx(x=description,file=file,col.names=F,
             row.names=F,sheetName="beskrivelse")

  for (i in 1:length(campuses)) {
    s<-campuses[[i]]
    extraLimits<-s$extraLimits
    roomgroup<-s$roomgroup
    cleanup<-s$cleanupFunction
    roomuse<-setupData(roomgroup=roomgroup,weekselection=dateSelection,extraLimits = extraLimits,cleanupFunction=cleanup)
    cat("data setup for ",roomgroup,"\n")
    mat<-countingReport(roomuse,maxTime)
    write.xlsx(x=mat,file=file,col.names = T,row.names=T,sheetName =paste(roomgroup,collapse="_"),append = T)
  }
  cat("rapport plarecet i ",file,sep="")
}
