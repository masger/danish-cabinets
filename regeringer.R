library(rvest)
library(tidyverse)
library(curl)
library(data.table)
library(lubridate)
library(prodlim)
library(xml2)

## get list of danish cabinets from wiki
url <- "https://da.wikipedia.org/wiki/Danmarks_regeringer"

overskrift <- read_html(url) %>%
  html_nodes("table") %>%
  html_table(fill = T)

regeringer <- overskrift[[1]]

setDT(regeringer)
#### regeringensside
regeringer[]
url_short <- "https://www.stm.dk/regeringen/regeringer-siden-1848/"

regeringer[, url_add := gsub(pattern = " ", replacement = "-", Regeringsnavn)]
regeringer[, url_add := gsub(pattern = "ø", replacement = "oe", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "æ", replacement = "ae", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "ü", replacement = "ue", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "Frijs", replacement = "krag-juel-vind-frijs", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "neergaard$", replacement = "neergaard-iii", x = url_add, ignore.case = T)]
regeringer[, url_add := gsub(pattern = "M.P.-", replacement = "", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "-ll", replacement = "-ii", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "-l", replacement = "-i", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "h.c.", replacement = "hc", x = url_add, ignore.case = TRUE)]

regeringer[, url_add := gsub(pattern = "Regeringen-Klaus-Berntsen", replacement = "Regeringen-Berntsen", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "Regeringen-Otto-Liebe", replacement = "Regeringen-Liebe", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "Regeringen-Thorvald-Stauning-", replacement = "Regeringen-Stauning-", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "Regeringen-Vilhelm-Buhl-I", replacement = "Regeringen-Buhl-I", x = url_add, ignore.case = F)]
regeringer[, url_add := gsub(pattern = "Regeringen-Hans-Hedtoft", replacement = "Regeringen-Hedtoft", x = url_add, ignore.case = F)]

urls <- paste0(url_short, regeringer$url_add)
url <- urls[30]
url <- urls[66]
url <- "https://www.stm.dk/regeringen/regeringer-siden-1848/Regeringen-Anker-Joergensen-IV/"
url <- "https://da.wikipedia.org/wiki/Regeringen_Lars_L%C3%B8kke_Rasmussen_III"
# url = "https://www.stm.dk/regeringen/regeringer-siden-1848/Regeringen-Madsen-Mygdal/"
dato2 <- "\\d+.\\d+.\\d+|\\d+.\\w+.\\d+"
extract_regering <- function(url) {
  regering <- read_html(utils::URLencode(url))
  xml_find_all(regering, ".//br") %>% xml_add_sibling("p", "zzz")
  xml_find_all(regering, ".//br") %>% xml_remove()

  # hent overskrifter
  overskrift <- regering %>%
    html_nodes(".article-components p") %>%
    html_text()

  # remove all blanks
  overskrift <- overskrift[!overskrift == "zzz"]
  if (url == "https://www.stm.dk/regeringen/regeringer-siden-1848/Regeringen-Andrae") {
    datoer <- matrix(data = c("18.10.1856", "", "13.05.1857", ""), nrow = 2)
  } else {
    #    dato = "([0-9]{2})[- .]([0-9]{2})[- .]([0-9]{4}|)"
    dato2 <- "\\d+.\\d+.\\d+|\\d+.\\w+.\\d+"
    datoer <- str_extract_all(overskrift, pattern = dato2, simplify = T)
  }

  if (!grepl("https://www.stm.dk/regeringen/regeringer-siden-1848/Regeringen-Mette-Frederiksen", x = url, ignore.case = TRUE)) {
    ministre <- str_split(overskrift[[which.max(nchar(overskrift))]], "zzz")
    test <- str_split(ministre[[1]], ",", simplify = TRUE)
  } else {
    ministre <- overskrift[-c(1:4)]
    test <- str_split(ministre, ",", simplify = TRUE)
  }


  not_include <- !grepl(pattern = "Regering:", test[, 1])
  navn <- test[not_include, 1]

  if (ncol(test) > 2) {
    minister <- matrix(apply(test[not_include, -1], 1, function(x) paste(x, collapse = " og ")))
  } else {
    minister <- test[not_include, -1]
  }


  df <- data.table(navn, minister = minister)

  #  df <- data.frame(matrix(unlist(test), nrow=length(test), byrow=T))
  names(df) <- c("navn", "ministerpost")
  df$parti <- str_extract(string = df$navn, pattern = "\\s*\\([^\\)]+\\)")
  df$navn <- str_replace(df$navn, "\\s*\\([^\\)]+\\)", "")


  df$start <- datoer[1, 1]
  df$stop <- datoer[1, 2]
  df$dod <- NA

  datoer <- str_extract_all(df$ministerpost, dato2, simplify = T)
  fra <- str_detect(string = df$ministerpost, pattern = "fra ")
  til <- str_detect(string = df$ministerpost, pattern = "til |†")
  dod <- str_detect(string = df$ministerpost, pattern = "†| død")


  if (ncol(datoer) == 0) {} else {
    if (ncol(datoer) == 1) {
      df$start[fra] <- datoer[, 1][fra]
      df$stop[til] <- datoer[, 1][til]
      df$dod[dod] <- datoer[, 1][dod]
    } else {
      df$start[fra] <- datoer[, 1][fra]
      df$stop[til] <- datoer[, 1][til]
      df$start[datoer[, 1] != "" & !fra & !til] <- datoer[, 1][datoer[, 1] != "" & !fra & !til]
      df$stop[datoer[, 2] != ""] <- datoer[, 2][datoer[, 2] != ""]
      df$dod[dod] <- datoer[, 1][dod]
    }
  }

  df$ministerpost_ny <- str_replace(df$ministerpost, "\\s*\\([^\\)]+\\)", "")


  df$regering <- gsub(pattern = "https://www.stm.dk/regeringen/regeringer-siden-1848/", replacement = "", x = url)
  data.table(df)
}

regeringer_total <- data.table()
for (i in urls[1:78]) {
  df <- try(extract_regering(i))
  if (class(df)[1] == "try-error") {
    print(i)
  } else {
    regeringer_total <- rbind(regeringer_total, df)
  }
}

regeringer_total <- regeringer_total[, .(navn, ministerpost, parti, start, stop, dod, regering)]
# regeringer_total[str_detect(string = start, pattern = "\\w")]
regeringer_total[navn == "H.P. Hanssen (u.p.)" & regering == "Regeringen-Zahle-II"]
regeringer_total[regering == "Regeringen-Stauning-I", stop := "14.12.1926"]
regeringer_total[stop == "07.12.51", stop := "07.12.1851"]
regeringer_total[start == "07.12.51", start := "07.12.1851"]
regeringer_total[stop == "21.05.1992", stop := "21.05.1892"]
regeringer_total[navn == "C. V. Bramsnæs" & stop == "", stop := "31.05.1933"]
regeringer_total[navn == "Carl Lundbye" & start == "13.08.1963", start := "13.08.1863"]
regeringer_total[
  navn == "H.P. Hanssen (u.p.)" & regering == "Regeringen-Zahle-II",
  `:=`(start = "30.09.1916", stop = "16.01.1918")
]
#
regeringer_total[navn == "C.A. Fonnesbech" & stop == "15.03.1968", stop := "15.03.1868"]
# Christopher Hage
regeringer_total[navn == "Christopher Hage" & stop == "20.03-28", stop := "28.04.1916"]
regeringer_total[navn == "H.P. Hansen" & start == "", start := "31.05.1933"]
regeringer_total[navn %like% "Ivar"]

# wiki_tables
url_add_wiki <- gsub(pattern = " ", replacement = "_", regeringer$Regeringsnavn)
wiki_urls <- paste0("https://da.wikipedia.org/wiki/", url_add_wiki)

regeringer_total_wiki <- data.table()
for (i in wiki_urls[66:78]) {
  webpage <- read_html(i)
  print(i)
  data <- html_nodes(webpage, ".wikitable")
  if (length(data) != 0 & i != "https://da.wikipedia.org/wiki/Regeringen_Helle_Thorning-Schmidt_I") {
    table <- html_table(data[[1]], header = FALSE)
    table <- table[1:6]
    table$regering <- gsub(pattern = "https://da.wikipedia.org/wiki/", replacement = "", x = i)

    regeringer_total_wiki <- rbind(table, regeringer_total_wiki)
  }
}

setDT(regeringer_total_wiki)
regeringer_total_wiki <- regeringer_total_wiki[
  X4 != "Minister" &
    !is.na(as.numeric(X1)),
  .(
    navn = X4,
    ministerpost = X2,
    parti = "NA",
    start = X5,
    stop = X6,
    dod = NA,
    regering
  )
]

regeringer_total_wiki[, regering := utils::URLdecode(regering)]
Encoding(regeringer_total_wiki$regering) <- "UTF-8"
regeringer_total_wiki[, regering := str_replace_all(regering, pattern = "_", replacement = "-")]

cols <- names(regeringer_total_wiki)
regeringer_total_wiki[, (cols) := lapply(.SD, str_remove_all, pattern = "\\[.+\\]"),
  .SDcols = cols
]

regeringer_total_wiki[navn == "Peter Hummelgaard" & start == "27. juni 2019", ministerpost := "Beskæftigelsesminister"]
regeringer_total_wiki[navn == "Peter Hummelgaard" & stop == "4. februar 2022", ministerpost := "Beskæftigelsesminister og minister for ligestilling"]
regeringer_total_wiki[navn == "Peter Hummelgaard" & stop == "Nuværende", ministerpost := "Beskæftigelsesminister"]

## not wiki
not_wiki <- regeringer_total[!regering %in% unique(regeringer_total_wiki$regering)]

##
# Ole Espersen
ole <- not_wiki[navn == "Henning Rasmussen" & grepl("Ole", ministerpost)]
ole[, `:=`(
  ministerpost = "Justitsminister",
  start = "20.01.1981",
  stop = "10.09.1982"
)]
not_wiki[navn == "Henning Rasmussen" & grepl("Ole", ministerpost), `:=`(
  ministerpost = "Justitsminister",
  start = "26.10.1979"
)]
# Hans R. Knudsen
hans <- not_wiki[grepl("Hans R. Knudsen", ministerpost)]
hans[, `:=`(
  ministerpost = "Finansminister",
  start = "07.09.1961",
  stop = "03.10.1962",
  parti = "(B)"
)]
not_wiki[grepl("Hans R. Knudsen", ministerpost), `:=`(
  ministerpost = "Finansminister",
  start = "18.11.1960",
  stop = "07.09.1961"
)]


# Ivar Nørgaard
europa <- not_wiki[grepl("Ivar", navn) &
  grepl("tillige minister for europæiske markedsanliggender", ministerpost), ]
europa[, `:=`(
  ministerpost = "europæiske markedsanliggender",
  start = "01.10.1967",
  stop = "02.02.1968",
  parti = "(S)"
)]
not_wiki[
  grepl("Ivar", navn) &
    grepl("tillige minister for europæiske markedsanliggender", ministerpost),
  `:=`(
    ministerpost = "Økonomiminister",
    start = "24.08.1965",
    stop = "02.02.1968"
  )
]

# Viggo Kampmann
viggo <- not_wiki[grepl("fungerende statsminister", ministerpost), ]
viggo[, `:=`(
  ministerpost = "statsminister",
  start = "19.02.1960",
  stop = "21.02.1960",
  parti = "(A)"
)]
not_wiki[
  grepl("fungerende statsminister", ministerpost),
  `:=`(
    ministerpost = "Finansminister",
    start = "28.05.1957",
    stop = "21.02.1960",
    parti = "(A)"
  )
]

### MIMI
mimi <- regeringer_total[navn == "Mimi Jakobsen" &
  ministerpost %like% "Samordningsminister", ]
mimi[, `:=`(
  ministerpost = "Samordnings- og industriminister",
  start = "08.02.1994",
  stop = "27.09.1994"
)]

not_wiki[navn == "Mimi Jakobsen" &
  ministerpost %like% "Samordningsminister", `:=`(
  ministerpost = "Samordningsminister",
  start = "25.01.1993",
  stop = "08.02.1994"
)]

# JYTTE
not_wiki[grepl("fra 01.07.1999", ministerpost), `:=`(
  ministerpost = "By- og boligminister",
  stop = "21.12.2000"
)]
# BERTEL
not_wiki[
  grepl("fra 27.11.2001 – 31.12.2002", ministerpost),
  `:=`(
    ministerpost = "Minister for flygtninge og indvandrere og integration",
    stop = "18.02.2005"
  )
]

# Henriette Kjær
henriette <- not_wiki[grepl("Henriette Kjær", navn), ]
henriette[, `:=`(
  ministerpost = "Minister for familie- og forbrugeranliggender",
  start = "02.08.2004",
  stop = "18.02.2005"
)]

not_wiki[grepl("Henriette Kjær", navn), `:=`(
  ministerpost = "Socialminister og minister for ligestilling",
  start = "27.11.2001",
  stop = "02.08.2004"
)]
# Connie Hedegaard
connie <- not_wiki[grepl("Connie Hedegaard", ministerpost), ]
connie[, `:=`(
  navn = "Connie Hedegaard",
  ministerpost = "Miljøminister",
  start = "02.08.2004",
  stop = "18.02.2005"
)]
not_wiki[grepl("Connie Hedegaard", ministerpost), `:=`(
  ministerpost = "Socialminister og minister for ligestilling",
  start = "02.08.2004",
  stop = "18.02.2005"
)]


not_wiki <- rbind(not_wiki, ole, hans, viggo, europa, mimi, connie, henriette)
not_wiki[!is.na(dod), stop := dod]

datoer <- str_extract_all(not_wiki$ministerpost, dato2, simplify = T)
not_wiki[datoer[, 2] != ""] |> View()

regeringer_total <- rbind(regeringer_total_wiki, not_wiki)
regeringer_total[regering == "Regeringen-Mette-Frederiksen" & stop == "Nuværende", stop := "01.03.2022"]
regeringer_total[, `:=`(
  start = parse_date_time(start, orders = c("dmy", "%d. /%B %Y")),
  stop = parse_date_time(stop, orders = c("dmy", "%d. /%B %Y"))
)]

regeringer_total[, `:=`(
  r_start = min(start),
  r_stop = max(stop),
  time = decimal_date(stop) - decimal_date(start)
),
by = regering
]

regeringer_total[, `:=`(
  p_start = min(start),
  p_stop = max(stop)
),
by = .(regering, navn)
]

regeringer_total[, early := as.numeric(p_stop < r_stop)]
regeringer_total[early == 1, ] |> View()

regeringer_total[, regering_lower := tolower(regering)]
regeringer[, regering_lower := tolower(url_add)]

regeringer_total <- regeringer_total[regeringer, on = .(regering_lower)]
write.csv(regeringer_total, "danish_cabinets.csv")
