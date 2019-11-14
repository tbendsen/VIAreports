
#run this file to generate the graph-reports

defaultPath="C:/Users/THBE/Desktop/rapporter/"

#' createGraphReports
#' Laver en grafisk rapport, som på forskellig vis illustrerer lokaleudnyttelsen.
#' Creates one report pr. campus, placed in defaultfolder/names.pdf (defaultname is campus_suffix)
#' Creates one xlsx-file with week-based counts, and one sheet pr. campus
#' Creates one xlsx-file with room-based counts, and one sheet pr. campus.
#' xlsx-files placed in defaultfolder/suffix/type_suffix.xlsx
#'
#' @param campuses En liste med et eller flere campusser (genereres med createCampus)
#' @param dateSelection En SQL-tekststreng, genereres med funktionen createDateSelection
#'
#' @param names Et navn til hver rapport (default lokalegruppen)
#' @param suffix en fælles endelse til alle rapporter
#' @param ... arguments passed to RoomUse (max- and minCapacity)
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import gridExtra
#'
#' @return nothing
#' @export
#'
#' @examples createGraphReports(campus("AarhusN_UV"),createDateSelection(2019,5,10,maxTime=960))
createGraphReports<-function(campuses,dateSelection,names=NULL,suffix="x",...) {

  if (class(dateSelection)!="DateSelection") {
    warning("dateselection skal være af klassen DateSelection")
  }
  if (class(campuses[[1]])!="list") {
    message("variabel campuses skal være en liste med et eller flere campusser")
  }
  n<-length(campuses)

  warning("xlsxfil hardcodet - ligger på skrivebordet")
  #delete old files
  unlink(paste(defaultPath,suffix,"/*.xlsx",sep=""))
  dir.create(paste(defaultPath,suffix,sep=""))

  for (i in 1:length(campuses)) {
    camp<-campuses[[i]]
    name<-NA
    if (!is.null(names)) {
      name<-names[i]
    }
    createReport(campus = camp,dateSelection = dateSelection,name = name,suffix = suffix,...)
  }
}

writexlsx<-function(data,weight,factor,suffix,sheetName) {
  fileName<-paste(defaultPath,suffix,"/",factor,"_",suffix,".xlsx",sep="")
  #save data in xlsxfile
  xlsxData<-data[,c("title","year","week","weekday","timefrom","timeto","roomname")]
  x<-xlsxData %>% mutate(week=if_else(week<10,paste("0",week,sep=""),as.character(week))) %>% mutate(week=paste(year,"-",week,sep=""))
  #renames columns with names weight and factor
  x<-x%>%rename(factor=factor,weight=weight)


  weightCount<-length(unique(x$weight))


  x<-x %>%group_by(title,factor) %>% summarize(lessonPrWeight=round(n()/weightCount,1))
  x<-x %>% spread(key=title,value=lessonPrWeight)
  x<-as.data.frame(x)
  #x<-x[,-2]
  f<-""
  if (factor=="roomname") {
    f<-"Lokalenavn"
  }
  else if (factor=="week") {
    f<-"Ugenummer"
  }
  #I have only a vague idea about the next two lines, but they rename "factor" back to original column-name

  f<-sym(f)
  x<-x%>%rename(!!f:=factor)
  write.xlsx(x,file=fileName,sheetName = sheetName,row.names = FALSE,showNA=FALSE,append=TRUE)

}

createReport<-function(campus,dateSelection,name,suffix,...) {
  #cat("\n---",campus$roomgroup,"---\n")
  s<-campus
  #I can't remember what this does
  doFill<-1


  texts<-c()
  for (i in 1:length(s$lessons)) {
    l<-s$lessons[[i]]
    mi<-min(l)
    ma<-max(l)
    if (mi==ma) {
      texts[i]<-paste(ma,".",sep="")
    }
    else {
      texts[i]<-paste(mi,".-",ma,".",sep="")
    }

  }
  s$texts<-texts
  roomgroup<-s$roomgroup
  extraLimits<-s$extraLimits
  texts<-s$texts
  les<-s$lessons
  cleanup<-s$cleanupFunction

  comment<-s$comment




  plots<-list()
  plotNames<-c()
  roomuse<-RoomUse(roomgroup=roomgroup,dateSelection=dateSelection,
                   extraLimits=extraLimits,cleanupFunction=cleanup,...)
  sheetName=paste(roomgroup,collapse="_")
  roomData<-roomuse@data
  writexlsx(data = roomData,weight = "week",factor = "roomname",suffix = suffix,sheetName=sheetName)
  writexlsx(data = roomData,weight = "roomname",factor = "week",suffix=suffix,sheetName=sheetName)



  #generer opgørelse over udnyttelse====

  myText<-"";
  myText<-paste(myText,"Lokaleudnyttelse: ",name,"\n\n",sep="")
  myText<-paste(myText,"Lokalegrupperne: ",paste(roomgroup,collapse =" og "),"\n" ,sep="")
  myText<-paste(myText,("Data opdateret: "),min(roomData$updatedate),"\n",sep="")
  myText<-paste(myText,length(unique(roomData$roomname))," lokaler \n",sep="")
  myText<-paste(myText,lessonUsage(roomuse=roomData,texts = texts,lessons = les),sep="")

  # #find rooms with lowest usage
  # lessonCountRoom<-lapply(X = split(x=roomuse,f = interaction(roomuse$roomname)),FUN = nrow)
  # weekCount<-length(unique(roomuse[,"week"]))

  #calculate effective timetableusage
  sort(unique(roomData[,"timefrom"]))->a
  sort(unique(roomData[,"timeto"]))->b
  n<-length(b)
  rep(1080,n)->m
  pmin(m,b)-a->l
  sum(l[which(l>0)])->mySum
  u<-round(100-mySum/(1080-480)*100)
  myText<-paste(myText,u,"% af tiden mellem 8 og 18 er pauser\n",sep="")
  myText<-paste(myText,"Der er ",length(l[which(l>0)])," lektioner med start mellem 8 og 18\n",sep="")

  #weekdays
  #bemærk: crascher hvis datasæt ikke indeholder timer i max-lektionsnummer
  myText<-paste(myText,"Benyttelse mandag og fre i ",texts[3],"lektion\n",sep="")

  myText<-paste(myText,dayUsage(roomuse = roomData,day=1,dayLength=length(les[[3]])),sep="")
  myText<-paste(myText,dayUsage(roomuse=roomData,day=5,dayLength=length(les[[3]])),sep="")



  if (!comment=="") {
    myText<-paste(myText,"\nBemærk: ",comment,"\n",sep="")
  }


  #tjek ringetider
  myText<-paste(myText,"\nRingetider (minutter til pause - minutter til næste lektionstart)\n",sep="")
  lessonLength<-list()
  unique(roomData[,c("timefrom","timeto")])->r
  r[order(r[,1]),]->r
  #beregn lektionslængde (dvs. frem til næste lektionsstart)

  for (i in 1:nrow(r)) {
    diff<-r[i,"timeto"]-r[i,"timefrom"]

    index<-as.character(r[i,"timefrom"])
    if (i<nrow(r)) {
      lessonLength[[index]]<-r[i+1,"timefrom"]-r[i,"timefrom"]
    }
    else {
      lessonLength[[index]]<-r[i,"timeto"]-r[i,"timefrom"]
    }
    myText<-paste(myText,i," (",diff," - ",lessonLength[[index]],")",": ",ringetid(r[i,"timefrom"])," - ",ringetid(r[i,"timeto"]),"\n",sep="")
  }
  for (i in 1:nrow(roomData)) {
    roomData[i,"effectiveLength"]<-
      lessonLength[[as.character(roomData[i,"timefrom"])]]

  }
  cat(myText)
  #writeClipboard(myText)


  #plot lektionsnummer====

  myPlot<-createGraph(roomuse=roomData,weights=c("week","roomname"),
                      myFactor="timefrom",
                      xlabel="starttidspunkt",
                      ylabel="lektioner pr. uge pr. lokale",
                      labels=function(xvalues) {
                        ringetid(xvalues)
                      },
                      doFill=doFill,
                      doRotate=1
  )

  i<-length(plots)+1
  plotNames[i]<-paste(paste(roomgroup,collapse=","),"_starttid",".png",sep="")
  plots[[i]]<-myPlot


  #plot week====


  myPlot<-createGraph(roomuse=roomData,weights=c("roomname"),
                      myFactor="week",
                      xlabel="ugenummer",
                      ylabel="lektioner pr. lokale",
                      labels=function(xvalues) {
                        xvalues
                      },
                      doFill=doFill,doRotate=1
  )

  plotRoomgroup<-paste(roomgroup,collapse="_")

  i<-length(plots)+1
  plotNames[i]<-paste(plotRoomgroup,"_uge",".png",sep="")
  plots[[i]]<-myPlot

  #plot dage ====

  labels<-function(xvalues) {
    dage[as.integer(xvalues)]
  }

  myPlot<-createGraph(roomuse=roomData,weights=c("roomname","week"),
                      myFactor="weekday",
                      xlabel="ugedag",
                      ylabel="lektioner pr. lokale pr. uge",
                      labels=labels,
                      doFill=doFill
  )

  i<-length(plots)+1
  plotNames[i]<-paste(plotRoomgroup,"_ugedag",".png",sep="")
  plots[[i]]<-myPlot

  #plot rooms====
  #lokaler
  labels<-function(xvalues) {
    xvalues
  }
  myPlot<-createGraph(roomuse=roomData,weights=c("week"),
                      myFactor="roomname",
                      xlabel="lokale",
                      ylabel="lektioner pr. uge",
                      labels=labels,
                      doFill=doFill,doRotate=1
  )

  i<-length(plots)+1
  plotNames[i]<-paste(plotRoomgroup,"_lokaler",".png",sep="")
  plots[[i]]<-myPlot



  #plot and save
  grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],ncol=2)


  if (is.na(name)) {
    name<-plotRoomgroup
  }
  file<-paste(defaultPath,name,suffix,".pdf",sep="")
  pdf(file=file,paper="a4")

  textplot(myText)

  grid.arrange(plots[[1]],ncol=1)
  grid.arrange(plots[[2]],ncol=1)
  grid.arrange(plots[[3]],ncol=1)
  grid.arrange(plots[[4]],ncol=1)

  dev.off()

  #save individual plots
  for (i in 1:length(plots)) {
    name=plotNames[i]
    plot=plots[[i]]
    #ggsave(filename=name,path="lokaler/speciel/",plot=plot)
  }


}


