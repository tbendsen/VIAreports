
#' DateSelection
#'
#' @slot year numeric. starting year (2019)
#' @slot startWeek numeric. starting week (ie. 5)
#' @slot endWeek numeric. ending week (may be next year)
#' @slot maxDay numeric. (default 5)
#' @slot maxTime numeric. (minutes after midnight)
#' @slot minTime numeric.
#' @slot excludeWeeks numeric. (weeks to exclude from report)
#' @slot name name of term eg. Efterår 2019
#'
#' @return DateSelection
#' @export
#'
#' @examples blablabla
DateSelection <- setClass(
  "DateSelection",
  slots=list(
    year="numeric",
    startWeek="numeric",
    endWeek="numeric",
    maxDay="numeric",
    maxTime="numeric",
    minTime="numeric",
    excludeWeeks="numeric",
    name="character"
    ),
    #defines default values
    prototype = list(maxTime=18*60,minTime=8*60,maxDay=5,excludeWeeks=numeric(0)),
  validity = function(object) {
    return(TRUE)
  }
)

setGeneric(name="getSQL",
           def=function(dateSelection){
             standardGeneric("getSQL")
           })

#' getSQL
#'
#' @param DateSelection
#'
#' @return a SQL-string
#' @export
#' @examples blabla
setMethod(f = "getSQL",signature="DateSelection",
          definition=function(dateSelection){
            year<-dateSelection@year
            endYear<-year
            endWeek<-dateSelection@endWeek
            startWeek<-dateSelection@startWeek
            maxTime<-dateSelection@maxTime
            minTime<-dateSelection@minTime
            maxDay<-dateSelection@maxDay
            exclude<-dateSelection@excludeWeeks

            yearString<-"(year=%year AND week>=%startWeek AND week<=%endWeek)"

            if (endWeek<startWeek) {
              endYear<-year+1
              yearString<-"((year=%year AND week>=%startWeek) OR (year=%endYear AND week<=%endWeek))"
            }
            yearString<-sub("%year",year,yearString)
            yearString<-sub("%startWeek",startWeek,yearString)
            yearString<-sub("%endWeek",endWeek,yearString)
            yearString<-sub("%endYear",endYear,yearString)

            sql<-'(%yearString AND week NOT IN (%excludeWeeks))  AND weekday<=%maxDay AND timeto<=%maxTime
                  AND timefrom>=%minTime'
            sql<-sub("%yearString",yearString,sql)
            sql<-sub("%maxDay",maxDay,sql)
            sql<-sub("%maxTime",maxTime,sql)
            sql<-sub("%minTime",minTime,sql)
            sql<-sub("%excludeWeeks",paste(exclude,collapse=",",sep=""),sql)
            #remove AND week NOT IN () if excludeweeks is empty
            sql<-sub("AND week NOT IN ()",replacement = "",sql,fixed = TRUE)
            #message(sql)
            return(sql)
          })

#' DateSelection
#'
#' @param year numeric. starting year (2019)
#' @param startWeek numeric. starting week (ie. 5)
#' @param endWeek numeric. ending week (may be next year)
#' @param maxDay numeric. (default 5)
#' @param maxTime numeric. (minutes after midnight)
#' @param minTime numeric.
#' @param excludeWeeks numeric. (weeks to exclude from report)
#' @param name character name of term
#'
#' @return S4 object of class DateSelection
#' @export
#'
#' @examples blabla
DateSelection<-function(year,startWeek,endWeek,maxDay=5,maxTime=18*60,minTime=8*60,excludeWeeks=numeric(0),name="") {
  d<-new("DateSelection",year=year,startWeek=startWeek,endWeek=endWeek,maxDay=maxDay,maxTime=maxTime,
         minTime=minTime,excludeWeeks=excludeWeeks,name=name)
  d
}
