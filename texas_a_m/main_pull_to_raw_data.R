dat <- read.csv("~/unpaid_technicians/texas_a_m/march_2016.csv")

datsum <- dat[,c("agency","salary")] %>% group_by(agency, salary) %>% summarize(count = n())

datsum$job_board <- "texasAM"
datsum$month <- 3
datsum$year <- 2016


datsum <- datsum[,c("salary","job_board","agency","month","year","count")]

colnames(datsum) <- c("type","job_board","employer","month","year","count")

write.csv(datsum, file=c("~/unpaid_technicians/raw_data_to_be_combined/texas_a_and_m_march_2016.csv"), row.names=FALSE)