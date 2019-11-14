getReportConnection<-function() {
  con<-dbConnect(drv = RMariaDB::MariaDB(),dbname="via_report",username="root",
                 password="anne7GRY",host="localhost")
  con
}

getUntisConnection<- function() {
  pw<-readline("Kode ")
  con<-dbConnect(drv=RMariaDB::MariaDB(),dbname="VIAUC",username="THBE",
                 password=pw,host="10.253.153.12")
}

#params should be a string of get-parameters, starting with &
#location is the url-part after api - starting with /
getURLwithAPIkey<-function(location,params) {
  apiURL<-"https://skema.via.dk/api"
  apiKey<-"?api_key=ac5d5bb7c6e78b3bdb65f2f8a064f682aa8abaa4"
  str<-paste(apiURL,location,apiKey,params,sep="")
  str
}


#omregner fra minutter efter midnat til ringetid
#' ringetid
#'
#' @param t minutter efter midnat
#'
#' @return textstreng
#' @export
#'
#' @examples blabla
ringetid<-function(t) {
  t<-as.integer(t)
  hour<-floor(t/60)
  min<-t%%60
  for (i in 1:length(min)) {
    if (min[i]==0) {
      min[i]="00"
    }
    if (min[i]==5) {
      min[i]="05"
    }
  }
  paste(hour,".",min,sep="")

}

