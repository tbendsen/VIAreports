#
# library(RMariaDB)
# # source("./reportDefinitions.R",encoding="UTF-8")
# # source("./WU-roomgroups.R",encoding="UTF-8")
# # source("./loadLessonsFromWU.R",encoding="UTF-8")
# # source("./graphCreater.R",encoding="UTF-8")
# # source("./countingReport.R",encoding="UTF-8")
# # source("./graphReport.R",encoding="UTF-8")
#
# con<-dbConnect(drv = RMariaDB::MariaDB(),dbname="via_report",username="root",password="anne7GRY",host="localhost")
# #indstillinger for diverse udvalg i reportDefinition.R
#
# #settings====
# #aarhusn
# #aarhusc
# #viborg
# #holstebro
# #herning
# #horsens
#
#
# #initial databasesetup====
# # loadRoomGroups()
# # loadRooms(roomgroupsOnly = T)
#
# #load data====
# #!!!!remember truncateparameter#
# #loadRoomGroupsData(roomgroups=c("AarhusN_Øvrige","AarhusN_UV"),start = "2019-01-01",end = "2019-08-30")
#
# #Excelopgørelse====
# createCountingReport(campuses = list(aarhusn_fag,aarhusn),weekselection = weekselection)
#
# #udnyttelsesopgørelse
# # args<-commandArgs(trailingOnly = T)
# # args<-as.numeric(args)
# # args<-2
# #createGraphReports(campuses = list(aarhusn_fag,aarhusn),weekselection = weekselection)
#  dbDisconnect(conn = con)
