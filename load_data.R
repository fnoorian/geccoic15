library("timeDate")
library("lubridate")
library("xts")

Sys.setenv(TZ="CET")
setRmetricsOptions(myFinCenter="CET")

dt = readRDS("data/challengeData.RDS")
dt = xts(dt[,-1], dt$Timestamp)
times = rbind(c("No data removed", "2013-11-18 05:12:00", "2014-01-24 11:02:00"),
              c("Supply temperature setpoint", "2014-01-24 11:03:00", "2014-03-24 11:43:00"),
              c("System supply temperature", "2014-03-24 11:44:00", "2014-05-22 13:24:00"),
              c("Return temperature", "2014-05-22 13:25:00", "2014-07-20 14:05:00"),
              c("System power", "2014-07-20 14:06:00", "2014-09-17 14:46:00"),
              c("All 4 time series", "2014-09-17 14:47:00", "2014-11-15 14:27:00"),
              c("No data removed", "2014-11-15 14:28:00", "2015-01-13 15:08:00"))

sp0 = paste0(times[1,2], "/", times[1,3])
sp1 = paste0(times[2,2], "/", times[2,3])
sp2 = paste0(times[3,2], "/", times[3,3])
sp3 = paste0(times[4,2], "/", times[4,3])
sp4 = paste0(times[5,2], "/", times[5,3])
sp5 = paste0(times[6,2], "/", times[6,3])
sp6 = paste0(times[7,2], "/", times[7,3])

sp0r = 1:96831
sp1r = 96832:181832
sp2r = 181833:266833
sp3r = 266834:351834
sp4r = 351835:436835
sp5r = 436836:521836

daylight_saving = "2014-10-26 02:00:00/2014-10-26 02:59:00"
#index_daylight_saving = which((index(dt) >= "2014-10-26 02:00:00") &
#                                (index(dt) <= "2014-10-26 02:59:00"))
index_daylight_saving = 492229:492348
