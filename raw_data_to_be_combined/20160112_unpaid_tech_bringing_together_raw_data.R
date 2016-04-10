# stitching together all of the differnet files from all the contributors

fl <- list.files("~/unpaid_technicians/raw_data_to_be_combined/", pattern=".csv")


unpaid <- list()

for(i in 1:length(fl)){
  ii <- i + 1
  unpaid[[ii]] <- read.csv(paste0("~/unpaid_technicians/raw_data_to_be_combined/",fl[i]))
}


un <- do.call(rbind, unpaid)


un$percent <- NA

firsts <- seq(1,nrow(un),4)

for(i in firsts){
  tot <- sum(un[i:(i+3),"count"])
  un[i,"percent"] <- un[i,"count"]/tot
  un[(i+1),"percent"] <- un[(i+1),"count"]/tot
  un[(i+2),"percent"] <- un[(i+2),"count"]/tot
  un[(i+3),"percent"] <- un[(i+3),"count"]/tot
}

un[is.na(un$percent),]$percent <- 0

un <- un[!is.na(un$month),]

un[un$month=="Jan",]$month <- 1

write.csv(un, "~/unpaid_technicians/ongoing_collection.csv", row.names=FALSE)
