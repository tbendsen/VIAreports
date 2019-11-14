#' calculateTotal
#'
#' Calculates total roomutilizition
#'
#' @param roomuse RoomUse object
#' @param singleWeek if given roomutilization is calculated only for this week. However
#' number of rooms and number of lessons is calculated from entire dataset. Number of days is only
#' calculated for this week.
#'
#' @return fraction of periods used
#' @export
#'
#' @examples blabla
calculateTotal<-function(roomuse,singleWeek=NA) {
   data<-roomuse@data
   #number of lessons pr. day
   lessonCount<-as.numeric(data%>%select(lessonNumber)%>%distinct()%>%summarise(n()))
   #number of rooms
   roomCount<-as.numeric(data%>%select(roomname)%>%distinct()%>%summarise(n()))

   if (!is.na(singleWeek)) {
      data<-data%>%filter(week==singleWeek)
      title=paste("Udnyttelsesgrad, uge",singleWeek)
   }
   #number of weeks
   weekCount<-as.numeric(data%>%select(week)%>%distinct()%>%summarise(n()))

   #calculate total use. totalDayCount is only calculated from current week, as to exclude hollidays
   totalDayCount<-as.numeric(data%>%select(week,weekday)%>%distinct()%>%summarise(n()))
   totalUse<-as.numeric(data%>%summarise(n()))
   totalUse<-signif(totalUse/(totalDayCount*roomCount*lessonCount)*100,2)
   return(totalUse)
}

#' totalsReport
#'
#' @param data dataframe
#'
#' @return nothing
#' @export
#'
#' @examples blabla
totalsReport<-function(data,comment) {
   file<-"c:\\Users\\thbe\\Desktop\\rapporter\\oversigt.pdf"
   params<-list(data=data,comment=comment)
   rmarkdown::render("totalReport.Rmd", output_file = file,
                     params = params,
                     envir = new.env(parent = globalenv())
   )

}

#' termReport creates in pdf-report for a campus
#'
#' @param roomuse S4 class RoomUse
#' @param file filename
#' @param title title to be used in report. If "" it will be a combination of campus title and date name
#'
#' @return nothing
#' @export
#'
#' @examples blabla
termReport<-function(roomuse,file=NA,title="") {
   campus<-roomuse@campus
   dateSelection<-roomuse@dateSelection
   if (is.na(file)) {
      file<-"c:\\Users\\thbe\\Desktop\\test.pdf"
   }
   else {
      file<-paste("c:\\Users\\thbe\\Desktop\\rapporter\\pdf\\",file,".pdf",sep="")
   }

   if (title=="") {
      title=paste(campus@title," - ",dateSelection@name,sep="")
   }

   params<-list(roomuse=roomuse,title=title)
   rmarkdown::render("pdfgenerator.Rmd", output_file = file,
                     params = params,
                     envir = new.env(parent = globalenv())
   )

}

#' createTermGraph
#'
#' @param data S4 class RoomUse
#' @param singleWeek if set, displays only that week
#'
#' @return ggplot
#' @export
#'
#' @examples none
createTermGraph<-function(roomuse,singleWeek=NA) {
   createRingeTid<-function(from,to) {
      paste(ringetid(from),ringetid(to),sep="-")
   }

   viaGreen <-rgb(177,205,69,1,maxColorValue = 255)

   data<-roomuse@data

   ringetidLevels<-data%>%select(timefrom,timeto)%>%distinct()%>%arrange(timefrom)%>%
      mutate(str=createRingeTid(timefrom,timeto))%>%pull(str)

   title="Udnyttelsesgrad alle uger"
   roomCount<-as.numeric(data%>%select(roomname)%>%distinct()%>%summarise(n()))
   if (!is.na(singleWeek)) {
      data<-data%>%filter(week==singleWeek)
      title=paste("Udnyttelsesgrad, uge",singleWeek)
   }
   weekCount<-as.numeric(data%>%select(week)%>%distinct()%>%summarise(n()))
   totalUse<-calculateTotal(roomuse,singleWeek)
   subtitle<-paste("Max antal bookinger pr. tid: ",weekCount*roomCount,"\n",
                   "Samlet udnyttelse af disponibel undervisningstid: ",totalUse,"%",sep="")

   ugedage<-c("man","tir","ons","tor","fre")

   dayCount<-length(unique(data$weekday))




   data<-as.data.frame(data %>% group_by(weekday,timefrom,timeto,lessonNumber) %>% summarise(antal=n())) %>%
      mutate(use=signif(antal/(roomCount*weekCount),2)) %>%
      mutate(xmax=weekday,xmin=weekday-1,ymax=-lessonNumber+1,ymin=-lessonNumber)

   data$ugedag<-factor(x = ugedage[data$weekday],levels=ugedage)
   #data$ringeTid<-factor(x=ringetid(data$timefrom),levels=ringeTid)
   data<-data%>%arrange(timefrom)%>%mutate(ringetid=factor(createRingeTid(timefrom,timeto),levels=ringetidLevels))

   ylabels<-data %>% distinct(ringetid,lessonNumber)
   ylabels<-data.frame(ringetid=ringetidLevels,lessonNumber=1:length(ringetidLevels))
   xlabels<-data.frame(weekday=1:5,ugedag=ugedage)
   #not used - and probably not calculated correctly
   xavg<-data %>% group_by(weekday) %>% summarise(avg=mean(use))
   #average pr. lessonnumber
   yavg<-data %>% group_by(timefrom,lessonNumber) %>% summarise(avg=sum(use)/dayCount)

   caption<-""
   if (weekCount>1) {
      caption<-"I beregning af gennemsnit pr. lektionsnummer er helligdage ikke ekskluderet."
   }

   p<-ggplot(data=data)+
      theme(panel.background = element_rect(fill="white"))+
      theme_void()+
      #black bordens around each cell
      geom_rect(mapping=aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                fill="white",color="black",alpha=0.0)+
      #green bar in each cell
      geom_rect(mapping=aes(xmin=xmin,ymin=ymin,ymax=ymax,xmax=xmin+use),
                fill=viaGreen,alpha=0.5)+
      #print usage count in each cell
      geom_text(mapping=aes(x=(xmax+xmin)/2,y=(ymin+ymax)/2,label=paste(antal,sep="")))+
      #print ringetider on y-axis
      geom_text(mapping=aes(x=-0.6,y=-lessonNumber+0.5,label=ringetid),data=ylabels)+
      #print title on y-axis
      geom_text(mapping=aes(x=-1.5,y=-length(ringetidLevels)/2,label="Ringetid"),angle=90,size=4)+
      #grey shade on right sum
      geom_rect(mapping=aes(xmin=nrow(xlabels),xmax=nrow(xlabels)+0.6,ymin=-length(ringetidLevels),ymax=0),fill="grey",alpha=0.5,data=data.frame())+
      #sum pr. lessonnumber
      geom_text(mapping=aes(x=nrow(xlabels)+0.3,y=-lessonNumber+0.5,label=paste(round(avg*100,0),"%",sep="")),data=yavg)+
      #add x-labels (day names)
      geom_text(mapping=aes(y=0.3,x=weekday-0.5,label=ugedag),data=xlabels)+
      #grey shade, and means for days - not used now
      #geom_rect(mapping=aes(xmin=-1,xmax=dayCount,ymin=0,ymax=0.7),fill="grey",alpha=0.5,data=data.frame())+
      #geom_text(mapping=aes(x=weekday-0.5,y=0.35,label=paste(signif(avg*100,2),"%",sep="")),color="black",data=xavg)+
      labs(title = title,subtitle = subtitle,caption=caption)

   p

}

