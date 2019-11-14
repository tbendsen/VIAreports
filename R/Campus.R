
#' Campus
#'
#' @slot extraLimits sql-string (starting with AND)
#' @slot roomgroups Name of roomgroup in WU
#' @slot lessons list (not documentet now)
#' @slot cleanupFunction (function that takes and return a RoomUse object)
#' @slot comment characterstring
#'
#' @return Object
#' @export
#'
#' @examples blablabla
Campus <- setClass(
  "Campus",
  slots=list(
    extraLimits="character",
    roomgroups="character",
    lessons="list",
    cleanupFunction="function",
    comment="character",
    title="character"
    ),
  validity = function(object) {
    return(TRUE)
  }
)

#' Campus
#'
#' @param extraLimits sql-string (starting with AND)
#' @param roomgroups Array of roomgroups in WU
#' @param lessons list (not documentet now)
#' @param cleanupFunction (function that takes and return a RoomUse object)
#' @param excludeRooms Array
#' @param comment characterstring
#'
#' @return S4 object of class Campus
#' @export
#'
#' @examples xxx
Campus<-function(extraLimits="",roomgroups,lessons=list(),cleanupFunction=function(r){return(r)},excludeRooms=as.character(c()),
                       comment="",title="") {
  extraLimits<-paste(extraLimits, " AND room NOT IN (%excludeRooms) ")
  exR<-paste("'",excludeRooms,"'",sep="",collapse=",")
  extraLimits<-gsub(pattern="%excludeRooms",replacement = exR,x = extraLimits)
  new("Campus",extraLimits=extraLimits,roomgroups=roomgroups,lessons=lessons,cleanupFunction=cleanupFunction,
      comment=comment,title=title)
}


