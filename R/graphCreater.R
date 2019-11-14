
dage<-c("Man","Tir","Ons","Tor","Fre","Lør","Søn")



createGraph<-function(roomuse,weights,myFactor,xlabel,ylabel,labels,doFill=0,doRotate=0) {

  angle<-0
  vjust<-1
  hjust<-0.5
  if (doRotate) {
    angle<-90
    vjust<-0.5
    hjust<-1

  }

  #calculate weight for barchart
  w<-1
  for (we in weights) {
    w<-w*length(unique(roomuse[,we]))

  }

  #find unikke elementer i den valgte faktor
  b<-as.character(sort(unique(roomuse[,myFactor])))

  #hvis faktoren er ugenr, skal det håndteres specielt, aht. årsskift
  if (myFactor=="week") {
    x<-unique(roomuse[,c("year","week")])
    o<-order(x$year,x$week)
    b<-as.character(x[o,"week"])
    #assign("weekfactor",b,envir=.GlobalEnv)
  }
  myLabels<-labels(b)

  title<-paste("Fordeling på ",xlabel,sep="")

  #det her er langhåret: aes() finder sine variable enten i .GlobalEnv
  #eller i den dataframe der angives. Derfor er det nødvendigt
  #at konstruere en dataframe hvor man kender kolonnenavnene, og
  #som indeholder alle relevante parametre

  x<-as.factor(roomuse[,myFactor])


  school_id<-roomuse$title

  myDataFrame<-data.frame(x=x,school_id=as.factor(school_id),weight=1/w)

  #calculate number of x-values
  xNumber<-length(unique(myDataFrame$x))
  labelSize<-10
  if (xNumber>40) {
    labelSize<-8
  }
  if (xNumber>50) {
    labelSize<-5
  }

  if (doFill) {
    ggplot() +
      ggtitle(title)+
      scale_fill_discrete(name="Uddannelse")+
      geom_bar(aes(x=x,
                   weight=weight
                   ,fill=school_id
      ),data=myDataFrame)+
      scale_x_discrete(name=paste(xlabel),breaks=b,limits=b,labels=myLabels)+
      scale_y_continuous(name=ylabel)+
      theme(axis.text.x = element_text(angle = angle, vjust=vjust,hjust = hjust, size=labelSize))
  }
  else {
    ggplot() +
      ggtitle(title)+
      scale_fill_discrete(name="skoleId")+
      geom_bar(aes(x=x,
                   weight=weight
      ),data=myDataFrame)+
      scale_x_discrete(name=paste(xlabel),breaks=b,limits=b,labels=myLabels)+
      scale_y_continuous(name=ylabel)+
      theme(axis.text.x = element_text(angle = angle, vjust=0.5,hjust = 1, size=10))
  }




}
#end createGraph




#denne funktion regner med 5 dage pr. uge, og tager således ikke
#hensyn til hvor mange der inkluderes i opgørelsen
#det betyder principielt at udnyttelsen overvurderes, hvis man laver
#opgørelsen for hele ugen.
lessonUsage<-function(roomuse,texts,lessons) {
  #beregn procentsats
  #antal uger:
  weeks<-length(unique(roomuse[,"week"]))
  #antal lokaler
  rooms<-length(unique(roomuse[,"roomname"]))
  #lektionsnummer
  startTimes<-sort(unique(roomuse[,"timefrom"]))
  f<-weeks*rooms*5/100

  myText<-""

  for (i in 1:length(texts)) {
    text<-texts[i]
    les<-lessons[[i]]
    x<-nrow(subset(roomuse,timefrom %in% startTimes[les]))/f/length(les)
    #cat(text," lektion: ",round(x),"%\n",sep="")
    myText<-paste(myText,text," lektion: ",round(x),"%\n",sep="")

  }
  myText
}


dayUsage<-function(roomuse,day,dayLength) {
  #beregn procentsats
  #antal uger:
  weeks<-length(unique(roomuse[,"week"]))

  #antal lokaler
  rooms<-length(unique(roomuse[,"roomname"]))
  #days
  days<-sort(unique(roomuse[,"weekday"]))
  f<-weeks*rooms*dayLength/100
  #ringetiderne
  unique(roomuse[,c("timefrom","timeto")])->r
  r[order(r[,1]),]->r
  #max ringetid:
  maxTime<-r[dayLength,1]

  x<-nrow(subset(roomuse,weekday==day&timefrom<=maxTime))/f
  paste(dage[day],": ",round(x),"%\n",sep="")

}
