# Apologies in advance for my code.
# quite my thing yet.
library(rvest)
library(stringr)
library(magrittr)
#content
#.job-listing-link
#.job-posted-date

main <- function(page = NULL){

  # Work out and set the URL
  if(is.null(page)){
    url <- "http://wfscjobs.tamu.edu/job-board/"
  } else {
    url <- paste0("http://wfscjobs.tamu.edu/job-board/page/", page)
  }

  # Retrieve the individual listings
  content <- read_html(url) %>%
    html_nodes(css = ".job-listing-link")

  # Extract the dates and normalise them
  dates <- html_nodes(content, css = ".job-posted-date") %>%
    html_text %>%
    str_replace(pattern = "Posted: ", replacement = "") %>%
    strptime(format = "%b-%d-%y", tz = "UTC")

  # Extract the titles and normalise them
  titles <- html_nodes(content, css = ".job-title") %>%
    html_text

  # Mess around to get the actual URLs, since we'll need those, and the agency
  urls <- html_attr(content, "href")
  agency <- html_nodes(content, css = ".job-agency") %>%
    html_text

  # And done!
  index <- data.frame(listing_url = urls, date_posted = dates, job_title = titles,
                      agency = agency, stringsAsFactors = FALSE)

  listing_data <- do.call("rbind", lapply(index$listing_url, function(url){

      # Retrieve the content for that page
      content <- read_html(url)
      entries <- html_nodes(content, css = ".job-posting-dd") %>%
        html_text
      titles <- html_nodes(content, css = ".job-posting-dt") %>%
        html_text

      # Get the bits we care about
      position_type <- ifelse("Job Category" %in% titles, entries[titles=="Job Category"], NA)
      salary <- ifelse("Salary" %in% titles, entries[titles=="Salary"], NA)

      # Return!
      return(data.frame(position_type = position_type, salary = salary,
                        stringsAsFactors = FALSE))
  }))

  return(cbind(index, listing_data))
}


main1 <- main(page=1)
main2 <- main(page=2)
main3 <- main(page=3)
main4 <- main(page=4)
main5 <- main(page=5)

unpaid <- rbind(main1, main2, main3, main4, main5)

positions_we_care_about <- c("Temporary/Seasonal Positions","Volunteer Openings","Internships","Internships, Undergraduate Assistantships","Training")

unpaid <- unpaid[unpaid$position_type %in% positions_we_care_about,]

unpmarch <- unpaid %>% filter(date_posted >= as.POSIXct("2016-03-01 UTC"), date_posted <= as.POSIXct("2016-03-31 UTC"))

unpfeb <- unpaid %>% filter(date_posted >= as.POSIXct("2016-02-01 UTC"), date_posted <= as.POSIXct("2016-02-29 UTC"))


write.csv(unpmarch, "~/unpaid_technicians/texas_a_m/march_2016.csv")
write.csv(unpfeb, "~/unpaid_technicians/texas_a_m/feb_2016.csv")
