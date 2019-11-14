##Dette script indlæser lektioner for udvalgte lokalegrupper fra WU-API'et
##Data gemmes i en localhost database, og kan derfra benyttes til diverse rapporter.
##Brug funktionen loadRoomGroups(roomgroups,start,end) til at indlæse skemadata
# library(rjson)
# library(RMariaDB)





calculateMinutes<-function(date) {
  hours<-as.numeric(format(x = date,format="%H",tz="Europe/Paris"))
  minutes<-as.numeric(format(x = date,format="%M",tz="Europe/Paris"))
  hours*60+minutes
}

extractLessonID<-function(json) {
  json$untis_id
}

extractCourseID<-function(json) {
  c(course=json$course,lesson=json$untis_id,start=json$start,end=json$end)
}

#notice: this function will return null, if a lesson is booked in webuntis
extractDepartmentID<-function(json) {
  dep<-json$department
  if (!is.null(dep)) {
    c(department=json$department,course=json$untis_id)
  }
  else {
    c(department=-1,course=json$untis_id)
  }


}

extractSchoolID<-function(json) {
  sch<-json$name
  c(school=json$name,department=json$untis_id)
}

getCourses<-function(lessonDataFrame) {
  lessonIds<-as.character(lessonDataFrame$lessonId)
  groupLength<-100
  n<-ceiling(length(lessonIds)/groupLength)
  courses<-data.frame()
  for (i in 1:n) {
    minId<-(i-1)*groupLength+1
    maxId<-min(i*groupLength,length(lessonIds))
    ids<-paste(lessonIds[minId:maxId],collapse=",")
    queryString<-getURLwithAPIkey(location = "/lessons",params=paste("&untis_ids=",ids,sep=""))
    json<-fromJSON(file=queryString)
    m<-sapply(json,FUN = extractCourseID)
    courses<-rbind(courses,data.frame(courseId=m[1,],lessonId=m[2,],start=m[3,],end=m[4,],stringsAsFactors = F))
  }
  lessonDataFrame<-merge(lessonDataFrame,courses,all.x=T,all.y=F)
  lessonDataFrame$start<-as.POSIXct(x=lessonDataFrame$start,format="%Y-%m-%dT%H:%M:%S",tz="GMT")
  lessonDataFrame$end<-as.POSIXct(x=lessonDataFrame$end,format="%Y-%m-%dT%H:%M:%S",tz="GMT")
  lessonDataFrame
}

getDepartments<-function(lessonDataFrame) {
  courseIds<-unique(lessonDataFrame$courseId)
  groupLength<-100
  n<-ceiling(length(courseIds)/groupLength)
  courses<-data.frame()
  for (i in 1:n) {
    minId<-(i-1)*groupLength+1
    maxId<-min(i*groupLength,length(courseIds))
    ids<-paste(courseIds[minId:maxId],collapse=",")
    queryString<-getURLwithAPIkey(location = "/courses",params=paste("&untis_ids=",ids,sep=""))
    json<-fromJSON(file=queryString)
    m<-sapply(json,FUN = extractDepartmentID)
    courses<-rbind(courses,data.frame(departmentId=m[1,],courseId=m[2,]))

    #cat(m,"\n")


    #cat(queryString)
  }
  lessonDataFrame<-merge(lessonDataFrame,courses,all.x=T,all.y=F)
  lessonDataFrame
}

getSchools<-function(lessonDataFrame) {
  depIds<-unique(lessonDataFrame$departmentId)
  groupLength<-100
  n<-ceiling(length(depIds)/groupLength)
  deps<-data.frame()
  for (i in 1:n) {
    minId<-(i-1)*groupLength+1
    maxId<-min(i*groupLength,length(depIds))
    ids<-paste(depIds[minId:maxId],collapse=",")
    queryString<-getURLwithAPIkey(location = "/departments",params=paste("&untis_ids=",ids,sep=""))

    json<-fromJSON(file=queryString)
    m<-sapply(json,FUN = extractSchoolID)

    deps<-rbind(deps,data.frame(school_id=m[1,],departmentId=m[2,]))



    #cat(queryString)
  }
  lessonDataFrame<-merge(lessonDataFrame,deps,all.x=T,all.y=F)
  lessonDataFrame
}


loadRoom<-function(roomId,start,end) {
  roomId<-roomId
  apiDate<-paste("&start=",start,"&end=",end,sep="")
  queryString<-getURLwithAPIkey(location = paste("/locations/",roomId,"/lessons",sep=""),
                                params=paste("&format=json",apiDate,sep=""))
  json<-fromJSON(file=queryString)
  x<-sapply(json,extractLessonID)
  lessonDataFrame=data.frame()
  if (length(x)>0) {
    lessonDataFrame<-data.frame(lessonId=x,roomId=roomId)
    lessonDataFrame<-getCourses(lessonDataFrame)
    lessonDataFrame<-getDepartments(lessonDataFrame)
    lessonDataFrame<-getSchools(lessonDataFrame)
  }
  #schoolid will be NA if lesson is booked directly in webuntis
  lessonDataFrame<-lessonDataFrame[complete.cases(lessonDataFrame),]
}

loadRoomGroupData<-function(name,start,end,roomcount=0,roomname="") {
  roomLimit<-""
  if (roomname!="") {
    roomLimit<-paste(" AND r.name='",roomname,"'",sep="")
  }
  con<-getReportConnection()
  #find rooms
  roomgroup<-name
  queryString<-"SELECT r.name,r.id FROM via_report.room as r
  LEFT JOIN roomgroup_room as rgr ON r.id=rgr.room
  LEFT JOIN roomgroup as rg ON rg.id=rgr.roomgroup
  WHERE rg.name='%roomgroup' %roomLimit "
  queryString<-gsub(pattern = "%roomgroup",replacement = roomgroup,x = queryString)
  queryString<-gsub(pattern = "%roomLimit",replacement = roomLimit,x = queryString)

  lessonDataFrame<-data.frame()
  rooms<-dbGetQuery(conn = con,statement =  queryString)
  roomIds<-as.character(rooms$id)
  roomNames<-as.character(rooms$name)
  n<-length(roomIds)
  if (roomcount>0) {
    n<-roomcount
  }
  for (i in 1:n) {
    r<-roomIds[i]
    name<-roomNames[i]
    cat(r,"-",name," indlæses\n",sep="")
    data<-loadRoom(r,start,end)
    if (nrow(data)>0) {
      lessonDataFrame<-rbind(lessonDataFrame,data)
    }

  }
  dbDisconnect(conn = con)
  lessonDataFrame
}

#Loads data for several roomgroups (vector), and appends to database timetable====
#' loadRoomGroupsData
#' Indæser skemabrikker for en eller flere lokalegrupper
#'
#' @param roomgroups En vektor bestående af navne på lokalegrupper
#' @param start Startdato, f.eks. "2019-01-01"
#' @param end Slutdato, f.eks. "2019-07-01"
#' @param roomcount Bruges til at begrænse antal indlæste lokaler - kun i forbindelse med tests
#' @param roomname begræns til et lokalenavn. Kun i forbindelse med test.
#' @import rjson
#' @import RMariaDB
#'
#' @examples xxx
#' @export
loadRoomGroupsData<-function(roomgroups,start,end,roomcount=0,roomname="") {
  con<-getReportConnection()

  #make sure that ids are not writte using scientific notation
  options(scipen=999)

  lessonDataFrame<-data.frame()

  for (i in 1:length(roomgroups)) {
    r=roomgroups[i]
    message("===indlæser gruppe ",r,"===\n",sep="")
    lessonDataFrame<-rbind(lessonDataFrame,loadRoomGroupData(r,start,end,roomcount,roomname))
  }
  #Denne kode skal køres efter alle lokalegrupper er indlæst.
  #filter away invalid schoolIds (ending with "old")
  #lessonDataFrame<-subset(lessonDataFrame,!is.na(as.numeric(as.character(lessonDataFrame$school_id))))
  lessonDataFrame$timefrom<-calculateMinutes(lessonDataFrame$start)
  lessonDataFrame$timeto<-calculateMinutes(lessonDataFrame$end)
  lessonDataFrame$year<-as.numeric(format(lessonDataFrame$start,format="%G",tz="Europe/Paris"))
  lessonDataFrame$week<-as.numeric(format(lessonDataFrame$start,format="%V",tz="Europe/Paris"))
  lessonDataFrame$weekday<-as.numeric(format(lessonDataFrame$start,format="%u",tz="Europe/Paris"))

  lessonDataFrame<<-lessonDataFrame
  #delete old data,
  dbExecute(conn=con,paste("set @start='",start,"'",sep=""))
  dbExecute(conn=con,paste("set @end='",end,"'",sep=""))
  roomgroups<-paste("(",paste("'",roomgroups,"'",collapse=",",sep=""),")",sep="")
  #x<-dbGetQuery(con,"SELECT @start,@end")
  #View(x)
  str<-"delete from timetable where id in (select id from
(select timetable.id,roomId,year,week,@day:=weekday%7,timetable.room,roomgroup.name,str_to_date(concat_ws('-',year,week,@day),'%x-%v-%w') as date from via_report.timetable
left join roomgroup_room as rgr on rgr.room=roomId
left join roomgroup on rgr.roomgroup=roomgroup.id) as tt
where date<=@end and date>=@start and name IN %roomgroups) "
  ptm<-proc.time()
  dbExecute(con,"set autocommit=0")
  dbExecute(con,"start transaction")
  str<-gsub("%roomgroups",roomgroups,str)
  n<-dbExecute(conn=con,str)
 # cat(str,"\n")
  cat("slettet:",n,"\n")

  #write new data to database
  dbWriteTable(conn = con,"timetable",lessonDataFrame[,c("school_id","roomId","timefrom","timeto","year","week","weekday")],append=T,row.names=F)
  options(scipen=0)
  #add roomNames
  dbExecute(conn=con,"UPDATE timetable as tt LEFT JOIN room as r ON tt.roomId=r.id SET tt.room=r.name")
  dbExecute(con,"commit")
  dbExecute(con,"set autocommit=1")
  print(proc.time()-ptm)
  dbDisconnect(con)

}


