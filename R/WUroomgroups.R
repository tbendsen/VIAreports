
#' loadRooms
#' Load rooms from WebUntis
#'
#' @param roomgroupsOnly If TRUE, only rooms belonging to a roomgroup will be loaded
#'
#' @return nothing
#' @import rjson
#' @import RMariaDB
#'
#' @examples xxx
#' @export
loadRooms<-function(roomgroupsOnly=T) {
  con<-getReportConnection()
  untis<-getUntisConnection()
  #get room capacities from untis-database
  roomCapacities<-dbGetQuery(untis,"SELECT Name,Capacity FROM Room
                             WHERE SCHOOL_ID=1 AND SCHOOLYEAR_ID=2019 AND VERSION_ID=1
                             AND Deleted=0")
  file=getURLwithAPIkey(location="/locations",params = "&format=json-full&active=true")
  json<-fromJSON(file=file)
  rooms<-c()
  dbExecute(con,"TRUNCATE room")
  dbExecute(con,"TRUNCATE roomgroup_room")
  dbExecute(con,"TRUNCATE timetable")

  #insert into room and roomgroup_room
  l<-list()
  for (i in 1:length(json)) {
    room<-json[[i]]
    name<-room$name
    capacity<-as.numeric(roomCapacities%>%filter(Name==name)%>%select(Capacity))
    if (is.na(capacity)) {
      capacity<-0
    }
    department<-room$department

    groups<-room$locationgroups
    id<-room$untis_id
    minGroupLength=1
    if (!roomgroupsOnly) {
      minGroupLength=0
    }
    if (length(groups)>=minGroupLength) {
      #check if masterdata are from the autoritative dataset
      #this isn't really used by now, as capacity is read directly from Untis
      if (length(department)==0) {
        l[[name]]<-room
        # warning(paste("Ingen afdeling for ",name,sep=""))
      }
      else if (!(department>=140 & department<=148)) {
        l[[name]]<-room
        warning(paste("Ikke autoritative lokalestamdata for ",name,sep=""))
      }


      queryString<-"INSERT INTO room (`id`,name,capacity) VALUES (%id,\"%name\",\"%capacity\")"
      queryString<-gsub("%id",id,queryString)
      queryString<-gsub("%name",enc2native(name),queryString)
      if (length(capacity)==0) {
        capacity<-0
      }
      queryString<-gsub("%capacity",capacity,queryString)
      #print(queryString)
      dbExecute(conn = con,queryString)
      if (roomgroupsOnly) {
        for (j in 1:length(groups)) {
          queryString<-"INSERT INTO roomgroup_room (roomgroup,room) VALUES (%group,%room)"
          queryString<-queryString<-gsub("%group",groups[j],queryString)
          queryString<-queryString<-gsub("%room",id,queryString)
          dbExecute(con,queryString)
          cat("loaded ",name," ",groups[j],"\n",sep="")
        }
      }
    }
  }#end room-insert
  assign("l",l,envir = .GlobalEnv)
  #add labels to roomgroup-room
  dbExecute(con,"UPDATE roomgroup_room as rgr
              LEFT JOIN room as r ON rgr.room=r.id
           LEFT JOIN roomgroup as rg ON rg.id=rgr.roomgroup
           SET groupname=rg.name,roomname=r.name ")
  dbDisconnect(conn = con)
}


#snthoesch
#' loadRoomGroups
#' Load roomgroups from WebUntis. Will subsequently load rooms
#'
#'
#' @return nothing
#' @import rjson
#' @import RMariaDB
#'
#' @examples xxx
#' @export
loadRoomGroups<-function() {
  con<-getReportConnection()
  file=getURLwithAPIkey(location="/locationgroups",params = "&format=json-full")
  json<-fromJSON(file=file)
  roomgroups<-c()
  dbExecute(con,"TRUNCATE roomgroup")
  for (i in 1:length(json)) {
    group<-json[[i]]
    id<-group$untis_id
    name<-group$name
    queryString<-"INSERT INTO roomgroup (`id`,name) VALUES (%id,'%name')"
    queryString<-gsub("%id",id,queryString)
    queryString<-gsub("%name",enc2native(name),queryString)
    #print(queryString)
    dbExecute(conn = con,queryString)
    cat("loaded ",name,"\n",sep="")
  }
  dbDisconnect(conn = con)
  loadRooms()
}


