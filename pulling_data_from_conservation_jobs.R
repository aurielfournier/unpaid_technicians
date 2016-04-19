
dat <- main(pages=10)
dollar <- dat[grep("$",dat$text),]
unpaid <- dat[grep("unpaid",dat$text),]
volunteer <- dat[grep("volunteer",dat$text),]