# library(RMariaDB)
# library(dplyr)
# library(tidyr)
# library(VIAreports)


#' RoomUse
#'
#' @slot data data.frame.
#' @slot campus Campus
#' @slot dateSelection DataSelection.
#' @slot dayNames character.
#' @slot minCapacity minCapacity in room
#' @slot maxCapacity maxCapacity in room
#' @import RMariaDB
#'
#' @return RoomUse
#' @export
#'
#' @examples Use function RoomUse to create instance
RoomUse<-setClass(Class = "RoomUse",
                  slots=list(
                    data="data.frame",
                    campus="Campus",
                    dateSelection="DateSelection",
                    minCapacity="numeric",
                    maxCapacity="numeric",
                    dayNames="character"
                  )
)

setMethod("initialize","RoomUse",function(
  .Object,
  data="data.frame",
  campus="Campus",
  dateSelection="DateSelection",
  minCapacity="numeric",
  maxCapacity="numeric",
  ...
  ) {
    if (!missing(data)) {
      .Object@data<-data
    }
    .Object@campus<-campus
    .Object@dateSelection<-dateSelection
    .Object@minCapacity<-minCapacity
    .Object@maxCapacity<-maxCapacity
    .Object@dayNames<-c("Man","Tir","Ons","Tor","Fre","Lør","Søn")

    #message(dateSelection)

    con<-dbConnect(drv = RMariaDB::MariaDB(),dbname="via_report",username="root",password="anne7GRY",host="localhost")

    #outer query makes sure that doublebookings across educs in only counted once.
    queryString <- paste("SELECT GROUP_CONCAT(school_id) AS school_id,
        if(GROUP_CONCAT(school.text)!='',GROUP_CONCAT(school.text),GROUP_CONCAT(school_id)) AS title,
                year,week,weekday,timefrom,timeto,roomname,capacity,updatedate FROM
                         (SELECT school_id,year,week,weekday,timefrom,
                         timeto,
                         GROUP_CONCAT(DISTINCT room) as roomname,capacity,updatedate

                         FROM via_report.timetable
                         LEFT JOIN via_report.room ON timetable.room=room.name
                         WHERE room IN

                         (SELECT room.name FROM via_report.room
                         LEFT JOIN via_report.roomgroup_room ON room.`id`=roomgroup_room.room
                         WHERE roomgroup_room.roomgroup IN
                         (SELECT id FROM via_report.roomgroup WHERE name IN (%roomgroups))
                         ORDER BY name)
                         AND %dateSelection %extraLimits AND school_id IS NOT NULL
                         AND capacity>=%minCapacity AND capacity<=%maxCapacity
                         GROUP by school_id,year,week,weekday,timefrom,timeto,room) AS roomuse
                          LEFT JOIN via_report.school USING (school_id)
                         GROUP BY year,week,weekday,timefrom,timeto,roomname")

    queryString<-gsub(pattern="%roomgroups",replacement = paste("'",paste(campus@roomgroups,collapse="','"),"'",sep=""),x = queryString)

    queryString<-gsub(pattern = "%dateSelection",replacement = getSQL(dateSelection),x=queryString)

    queryString<-gsub(pattern = "%extraLimits",replacement = campus@extraLimits,x=queryString)
    queryString<-gsub(pattern = "%minCapacity",replacement = minCapacity,x=queryString)
    queryString<-gsub(pattern = "%maxCapacity",replacement = maxCapacity,x=queryString)
    #message(queryString)
    roomuse<-dbGetQuery(con,queryString)

    dbDisconnect(conn=con)

    if (nrow(roomuse)==0) {
      message("Ingen timer fundet")
      return("error")
      #cat("ingen timer fundet")
      #cat(queryString)
    }


    #Filter roomuse:
    roomuse$school_id<-as.character(roomuse$school_id)
    roomuse$title<-as.character(roomuse$title)

    #change educ_name in the cases with more than one educ
    roomuse[grep(x=roomuse$school_id,pattern = ","),"school_id"]<-"Flere"
    roomuse[grep(x=roomuse$title,pattern = ","),"title"]<-"Flere"



    .Object@data<-roomuse
    x<-.Object
    cleanedData<-(.Object@campus)@cleanupFunction(x)

    #cleanupfunction may introduce new duplicates. Make sure they are eliminated
    roomuse<-cleanedData@data%>%group_by(year,week,weekday,timefrom,timeto,roomname)%>%
      summarise(school_id=first(school_id),title=first(title),capacity=max(capacity),updatedate=min(updatedate))%>%ungroup()

    #beregner lektionsnummer
    startTimes<-sort(unique(roomuse$timefrom))

    lessonNumber<-function(timefrom) {
      which(startTimes==timefrom)
    }

    roomuse$lessonNumber<-mapply(lessonNumber,roomuse$timefrom)

    .Object@data<-roomuse

    validObject(.Object)
    return(.Object)

  }
)


#' RoomUse (S4-class)
#'
#' @param campus S4 class campus
#' @param dateSelection sql-string with dateselection
#' @param minCapacity xx
#' @param maxCapacity yy
#'
#' @return an object of class RoomUse
#' @export
#'
#' @examples blabla
RoomUse<-function(campus,dateSelection,minCapacity=0,maxCapacity=1000) {

  new("RoomUse",campus=campus,dateSelection=dateSelection,
      ,minCapacity=minCapacity,
      maxCapacity=maxCapacity)
}
