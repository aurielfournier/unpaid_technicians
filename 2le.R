library(xml2)
library(rvest)
library(magrittr)

main <- function(pages = 1){

  # Get the index details
  index_get <- function(page){

    # Generate the URL for the page
    url <- paste0("http://www.conservationjobboard.com/",
                  ifelse(page > 1, paste0("home/index/", 20 * (page - 1)),
                         ""))

    # Read the HTML! We know that ".listingPg" is the element we want because we scan over the page
    # with the SelectorGadget (https://cran.r-project.org/web/packages/rvest/vignettes/selectorgadget.html)
    # active
    content <- xml2::read_html(url) %>%
      html_node(".ListingPg") %>%
      html_nodes("a") %>% # We can then grab the links to the profiles, which are "a" using the SelectorGadget
      html_attr(., "href") # We then get the actual links

    # And then we're done! Return the first 20, since it's 20 per page
    return(content[1:20])
  }

  # Get the info off the individual page
  page_get <- function(url){

    content <- xml2::read_html(url) %>%
      html_node(".PostDtl") # This time we want ".PostDtl"

    # Extract the title, which the selector says is a h1, and sanitise a bit
    title <- gsub(x = html_text(html_node(content, "h1")), pattern = "\\n", replacement = "")

    # Grab the details
    details <- html_node(content, ".DtalPaginnr")

    # Employer
    employer <- gsub(x = html_text(html_node(details, "h2")), pattern = "\\n", replacement = "")

    # Other metadata! Location, URL, categories, job class
    metadata <- html_nodes(details, "span") %>%
      html_text %>%
      gsub(x = ., pattern = "\\n", replacement = "") %>%
      .[1:4]

    # Get the page text! May or may not be useful, but.
    text <- html_text(html_node(content, ".DtalPgText"))

    # Tie it all together and return
    return(data.frame(job_title = title, employer = employer,
                      employer_website = metadata[2],
                      location = metadata[1],
                      categories = metadata[3],
                      job_type = metadata[4],
                      text = text,
                      job_url = url,
                      stringsAsFactors = FALSE))
  }

  # Generate the page numbers
  pages <- seq(1, pages, 1)

  # Retrieve the URLs
  urls <- unlist(lapply(pages, index_get))

  # Retrieve the page data!
  data <- do.call("rbind", lapply(urls, page_get))
}

