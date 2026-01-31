##############################################################################
# Download and process WhoGov dataset - World Government Ministers
#
# WhoGov is the largest available dataset on members of government across
# time and countries. It covers 177 countries from 1966-2023 with data on
# 50,197 cabinet members.
#
# Data source: Nuffield Politics Research Centre, Oxford
# https://politicscentre.nuffield.ox.ac.uk/whogov-dataset/
#
# Citation:
# Nyrup, Jacob and Stuart Bramwell (2020). "Who Governs? A New Global Dataset
# on Members of Cabinets." American Political Science Review 114(4): 1366-1374.
##############################################################################

library(tidyverse)
library(data.table)
library(lubridate)

# WhoGov dataset download URLs
# Note: You may need to register at the website to download
WHOGOV_CROSSSECTIONAL_URL <- "https://politicscentre.nuffield.ox.ac.uk/whogov-dataset/download-dataset/"
WHOGOV_WITHIN_URL <- "https://politicscentre.nuffield.ox.ac.uk/whogov-dataset/download-dataset/"

##############################################################################
# OPTION 1: Download from QoG (Quality of Government) mirror
# This is easier to access without registration
##############################################################################

download_from_qog <- function() {
  # QoG hosts WhoGov data - easier access
  message("Attempting to download from QoG data repository...")

  # Check if file exists locally
  if (file.exists("WhoGov_within_V3.1.csv")) {
    message("WhoGov within-country file already exists locally.")
    return(fread("WhoGov_within_V3.1.csv"))
  }

  if (file.exists("WhoGov_crosssectional_V3.1.csv")) {
    message("WhoGov cross-sectional file already exists locally.")
    return(fread("WhoGov_crosssectional_V3.1.csv"))
  }

  message("To download the WhoGov dataset:")
  message("1. Visit: https://politicscentre.nuffield.ox.ac.uk/whogov-dataset/download-dataset/")
  message("2. Fill out the optional form (or skip it)")
  message("3. Download the CSV files")
  message("4. Place them in this directory")
  message("")
  message("Alternative: Download from QoG at https://datafinder.qog.gu.se/dataset/wgov")

  return(NULL)
}


##############################################################################
# OPTION 2: Process WhoGov data if already downloaded
##############################################################################

process_whogov_within <- function(filename = "WhoGov_within_V3.1.csv") {
  # Load the within-country dataset (one row per minister per year)
  if (!file.exists(filename)) {
    stop(paste("File not found:", filename, "\nPlease download from WhoGov website first."))
  }

  message(paste("Loading", filename, "..."))
  whogov <- fread(filename)

  message(paste("Loaded", nrow(whogov), "records"))
  message(paste("Countries:", length(unique(whogov$country_name))))
  message(paste("Years:", min(whogov$year), "-", max(whogov$year)))

  # Convert to format compatible with danish_cabinets.csv
  ministers <- whogov %>%
    mutate(
      navn = name,
      ministerpost = portfolio,
      parti = party,
      country = country_name,
      year = year,
      # WhoGov uses July snapshots, so approximate dates
      start = as.Date(paste0(year, "-07-01")),
      stop = NA_character_,  # End dates not directly available in within-country
      regering = gov_name,
      gender = gender,
      prestige = prestige
    ) %>%
    select(navn, ministerpost, parti, start, stop, regering, country, year, gender, prestige)

  return(ministers)
}


process_whogov_crosssectional <- function(filename = "WhoGov_crosssectional_V3.1.csv") {
  # Load the cross-sectional dataset (one row per country per year)
  if (!file.exists(filename)) {
    stop(paste("File not found:", filename, "\nPlease download from WhoGov website first."))
  }

  message(paste("Loading", filename, "..."))
  whogov <- fread(filename)

  message(paste("Loaded", nrow(whogov), "records"))
  message(paste("Countries:", length(unique(whogov$country_name))))

  # This dataset has aggregated info plus some specific minister names
  # Extract key minister positions (finance, defense, foreign affairs, agriculture)
  ministers <- whogov %>%
    select(
      country = country_name,
      year,
      govern_name,
      govern_start_date,
      govern_end_date,
      leader_name,
      leader_party,
      # Key ministers
      minister_finance,
      minister_defense,
      minister_foreign,
      minister_agriculture,
      # Stats
      n_members,
      share_women,
      average_age = age_total
    )

  return(ministers)
}


##############################################################################
# OPTION 3: Scrape Wikipedia for current world cabinets
##############################################################################

scrape_wikipedia_cabinets <- function() {
  library(rvest)

  message("Scraping current world cabinets from Wikipedia...")

  # Get list of current governments
  url <- "https://en.wikipedia.org/wiki/List_of_current_cabinets"
  html <- read_html(url)

  # Extract country cabinet links
  links <- html %>%
    html_nodes("table.wikitable a[href*='Cabinet']") %>%
    html_attr("href")

  cabinet_urls <- paste0("https://en.wikipedia.org", unique(links))

  message(paste("Found", length(cabinet_urls), "cabinet pages to scrape"))

  all_ministers <- data.table()

  for (url in cabinet_urls[1:10]) {  # Start with first 10 for testing
    tryCatch({
      cabinet_html <- read_html(url)

      # Try to extract minister info from tables
      tables <- cabinet_html %>%
        html_nodes("table.wikitable") %>%
        html_table(fill = TRUE)

      if (length(tables) > 0) {
        # Find table with minister info
        for (tbl in tables) {
          if (any(grepl("minister|secretary|portfolio", names(tbl), ignore.case = TRUE))) {
            tbl$source_url <- url
            all_ministers <- rbind(all_ministers, data.table(tbl), fill = TRUE)
          }
        }
      }

      Sys.sleep(1)  # Be nice to Wikipedia

    }, error = function(e) {
      message(paste("Error scraping", url, ":", e$message))
    })
  }

  return(all_ministers)
}


##############################################################################
# MAIN: Run the appropriate function based on what's available
##############################################################################

main <- function() {
  message("=" %>% rep(60) %>% paste(collapse = ""))
  message("World Ministers Data - WhoGov Dataset Processor")
  message("=" %>% rep(60) %>% paste(collapse = ""))
  message("")

  # Try to load WhoGov data
  within_file <- "WhoGov_within_V3.1.csv"
  crosssec_file <- "WhoGov_crosssectional_V3.1.csv"

  if (file.exists(within_file)) {
    ministers <- process_whogov_within(within_file)
    write.csv(ministers, "world_ministers_whogov.csv", row.names = FALSE)
    message(paste("Saved", nrow(ministers), "records to world_ministers_whogov.csv"))
    return(ministers)

  } else if (file.exists(crosssec_file)) {
    cabinets <- process_whogov_crosssectional(crosssec_file)
    write.csv(cabinets, "world_cabinets_whogov.csv", row.names = FALSE)
    message(paste("Saved", nrow(cabinets), "records to world_cabinets_whogov.csv"))
    return(cabinets)

  } else {
    message("")
    message("WhoGov data files not found locally.")
    message("")
    download_from_qog()
    message("")
    message("Alternative: Running Wikipedia scraper...")
    message("")

    tryCatch({
      wiki_ministers <- scrape_wikipedia_cabinets()
      if (nrow(wiki_ministers) > 0) {
        write.csv(wiki_ministers, "world_ministers_wikipedia.csv", row.names = FALSE)
        message(paste("Saved", nrow(wiki_ministers), "records from Wikipedia"))
        return(wiki_ministers)
      }
    }, error = function(e) {
      message(paste("Wikipedia scraping failed:", e$message))
    })
  }

  return(NULL)
}

# Run if executed directly
if (!interactive()) {
  main()
}
