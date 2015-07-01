source("prediction/pred-1-2-3-4.R")
source("prediction/pred-5.R")

####################
# merge results
print("Merging Final results:")
dt_merged = dt_5
dt_merged[sp1,1] = na.ts.merge(dt_merged[sp1,1], dt_1_2_3_4[sp1,1])
dt_merged[sp2,2] = na.ts.merge(dt_merged[sp2,2], dt_1_2_3_4[sp2,2])
dt_merged[sp3,3] = na.ts.merge(dt_merged[sp3,3], dt_1_2_3_4[sp3,3])
dt_merged[sp4,4] = na.ts.merge(dt_merged[sp4,4], dt_1_2_3_4[sp4,4])

stopifnot(!any(is.na(dt_merged)))
####################
# add the Daylight saving period
dt_timesaving = readRDS("data/challengeData.RDS")
dt_timesaving = dt_timesaving[index_daylight_saving,]

# fill na's using Na.approx
for (i in 2:5) {
  dt_timesaving[,i] = round(na.approx(dt_timesaving[,i]), 1)
}

####################
dt_merged = data.frame(index(dt_merged), dt_merged[,1], dt_merged[,2], dt_merged[,3], dt_merged[,4])
colnames(dt_merged) = c("Timestamp","Supply_temperature_setpoint","System_supply_temperature","Return_Temperature","System_Power")

timesaving_index_break = index_daylight_saving[1]-1
dt_final = rbind(dt_merged[1:timesaving_index_break,],
                 dt_timesaving,
                 dt_merged[(timesaving_index_break+1):(nrow(dt_merged)),])
        

stopifnot(!any(is.na(dt_final)))
         
write.table(dt_final, file="final.csv", sep=";", row.names = FALSE, col.names = TRUE, eol="\r\n")
