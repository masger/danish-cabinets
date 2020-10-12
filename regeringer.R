library(rvest)
library(tidyverse)
library(curl)
library(data.table)
library(lubridate)
library(prodlim)

## get list of danish cabinets from wiki
url = "https://da.wikipedia.org/wiki/Danmarks_regeringer"


overskrift <- read_html(url) %>% 
  html_nodes("table") %>%
  html_table(fill =T)

regeringer = overskrift[[1]]


#### regeringensside

url_short = "https://www.stm.dk/regeringen/regeringer-siden-1848/"

url_add = gsub(pattern = " ", replacement = "-", regeringer$Regeringsnavn[1:78])
url_add = gsub(pattern = "ø", replacement = "oe",x = url_add,ignore.case = F)
url_add = gsub(pattern = "æ", replacement = "ae",x = url_add,ignore.case = F)
url_add = gsub(pattern = "ü", replacement = "ue",x = url_add,ignore.case = F)
url_add = gsub(pattern = "Frijs", replacement = "krag-juel-vind-frijs",x = url_add,ignore.case = F)
url_add = gsub(pattern = "neergaard$", replacement = "neergaard-iii",x = url_add,ignore.case = T)

url_add = gsub(pattern = "M.P.-", replacement = "",x = url_add,ignore.case = T)
url_add = gsub(pattern = "-ll", replacement = "-ii",x = url_add,ignore.case = F)
url_add = gsub(pattern = "-l", replacement = "-i",x = url_add,ignore.case = F)
url_add = gsub(pattern = "h.c.", replacement = "hc",x = url_add,ignore.case = T)

url_add = gsub(pattern = "Regeringen-Klaus-Berntsen", replacement = "Regeringen-Berntsen",x = url_add,ignore.case = F)
url_add = gsub(pattern = "Regeringen-Otto-Liebe", replacement = "Regeringen-Liebe",x = url_add,ignore.case = F)
url_add = gsub(pattern = "Regeringen-Thorvald-Stauning-", replacement = "Regeringen-Stauning-",x = url_add,ignore.case = F)
url_add = gsub(pattern = "Regeringen-Vilhelm-Buhl-I", replacement = "Regeringen-Buhl-I",x = url_add,ignore.case = F)
url_add = gsub(pattern = "Regeringen-Hans-Hedtoft", replacement = "Regeringen-Hedtoft",x = url_add,ignore.case = F)


urls = paste0(url_short,url_add)
#url = "https://www.stm.dk/regeringen/regeringer-siden-1848/Regeringen-Madsen-Mygdal/"
extract_regering = function(url) {
  regering = read_html(url)
  xml_find_all(regering, ".//br") %>% xml_add_sibling("p", "zzz")
  xml_find_all(regering, ".//br") %>% xml_remove()
  
  #hent overskrifter
  overskrift <- regering %>% 
    html_nodes(".article-components p") %>% 
    html_text()
  
  # remove all blanks
  overskrift = overskrift[!overskrift == "zzz"]
  if(url == "https://www.stm.dk/regeringen/regeringer-siden-1848/Regeringen-Andrae") {
    datoer = matrix(data = c("18.10.1856","","13.05.1857",""),nrow = 2)} else {
#    dato = "([0-9]{2})[- .]([0-9]{2})[- .]([0-9]{4}|)"
    dato2 = "\\d+.\\d+.\\d+|\\d+.\\w+.\\d+"
    datoer = str_extract_all(overskrift, pattern = dato2,simplify = T)
  }
  
  ministre = str_split(overskrift[[which.max(nchar(overskrift))]], "zzz")
  test = str_split(ministre[[1]], ",")
  test = test[lengths(test)==2]
  df <- data.frame(matrix(unlist(test), nrow=length(test), byrow=T))
  names(df) = c("navn","ministerpost")
  df$parti = str_extract(string = df$navn, pattern = "\\s*\\([^\\)]+\\)") 
  df$navn = str_replace(df$navn, "\\s*\\([^\\)]+\\)", "")
  
  
  df$start = datoer[1,1]
  df$stop = datoer[1,2]
  
  datoer = str_extract_all(df$ministerpost, dato2,simplify = T)
  fra = str_detect(string = df$ministerpost,pattern = "fra ")
  til = str_detect(string = df$ministerpost,pattern = "til |†")
  if(ncol(datoer) == 0) {} else {
    if(ncol(datoer) == 1) {
      df$start[fra] = datoer[,1][fra]
      df$stop[til] = datoer[,1][til]  
    } else
      {
        df$start[fra] = datoer[,1][fra]
        df$stop[til] = datoer[,1][til]  
        df$start[datoer[,1] != "" & !fra & !til] = datoer[,1][datoer[,1] != "" & !fra & !til]
        df$stop[datoer[,2] != ""] = datoer[,2][datoer[,2] != ""]
      }
  }
  df$ministerpost = str_replace(df$ministerpost, "\\s*\\([^\\)]+\\)", "")
  

  df$regering = gsub(pattern = "https://www.stm.dk/regeringen/regeringer-siden-1848/",replacement = "",x = url)
  data.table(df)
}

regeringer_total = data.table()
for(i in urls[1:77]) {
  df = try(extract_regering(i))
  if(class(df)[1] == "try-error") {print(i)} else {
    regeringer_total = rbind(regeringer_total,df) 
  } 
}

#regeringer_total[str_detect(string = start, pattern = "\\w")]
regeringer_total[regering == "Regeringen-Stauning-I", stop := "14.12.1926"]
regeringer_total[stop == "07.12.51", stop := "07.12.1851"]
regeringer_total[start == "07.12.51", start := "07.12.1851"]
regeringer_total[stop == "21.05.1992", stop := "21.05.1892"]
regeringer_total[navn == "C. V. Bramsnæs" & stop == "", stop := "31.05.1933"]
regeringer_total[navn == "Carl Lundbye" & start == "13.08.1963", start := "13.08.1863"]
regeringer_total[navn == "H.P. Hanssen (u.p.)" & regering == "Regeringen-Zahle-II", 
                 `:=` (start = "30.09.1916", stop = "16.01.1918")]

regeringer_total[navn == "H.P. Hansen" & start == "", start := "31.05.1933"]

regeringer_total[,`:=` (start = parse_date_time(start,orders = c("dmy")),
                        stop = parse_date_time(stop,orders = c("dmy")))]
regeringer_total[, `:=` (r_start = min(start),
                         r_stop = max(stop),
                         time = decimal_date(stop) - decimal_date(start),
                         early = 1*(stop != max(stop))),by = regering]

regeringer_total[time < 0.02,]
regeringer_total[time < 0,]
regeringer_total[regering == "Regeringen-Stauning-II",]
summary(regeringer_total$time)

km0 <- prodlim(Hist(time = time,event = early)~1,data=regeringer_total)
plot(km0)

hyppige = regeringer_total[decimal_date(start) > 1980 & early == 1,.N, by = "ministerpost"][order(-N)][1:10]$ministerpost

ministerpost <- prodlim(Hist(time = time,event = early)~ministerpost,
                        data=regeringer_total[decimal_date(start) > 1945 & ministerpost %in% hyppige,])
plot(ministerpost)

reg_1945 = regeringer_total[decimal_date(start) > 1945]

reg_1945[,.N, by = parti][order(-N)]
reg_1945[early == 1,.N, by = parti][order(-N)]

reg_1945[,n_ministers := length(unique(ministerpost)), by = regering]

write.csv(regeringer_total, "danish_cabinets.csv")

library(survival)
coxph(Surv(time = time,event = early)~ministerpost,
        data=regeringer_total[decimal_date(start) > 1945 & ministerpost %in% hyppige,])

