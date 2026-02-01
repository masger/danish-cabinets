library(data.table)
library(lubridate)
library(stringr)

regeringer_total = setDT(fread("danish_cabinets.csv"))
regeringer_total$V1 = NULL
# clean up minister names
regeringer_total[, .N, by = "ministerpost"][order(-N)]
regeringer_total[str_detect(ministerpost, "-"), ]
regeringer_total[, ministerpost_ny := tolower(str_replace(ministerpost, "-", "minister"))]

min_matrix = str_extract_all(string = regeringer_total$ministerpost_ny,
                             "[:alpha:]+minister|minister for [:alpha:]+",
                             simplify = TRUE)
regeringer_total = cbind(regeringer_total, min_matrix)
cols_min = c("min1", "min2", "min3", "min4")
setnames(regeringer_total, old = paste0("V", 1:4), cols_min)

regeringer_total[, (cols_min) := lapply(.SD, str_trim), .SDcols = cols_min]
regeringer_total[min1 == min2 & min1 != "" , min2 := ""]
regeringer_total[min1 == min3 & min1 != "" , min3 := ""]

regeringer_total[min2 ==  "" & min3 != "",]


# make long
setdiff(names(regeringer_total), cols_min)
regeringer_total_long_stats = melt(
  regeringer_total,
  id.vars = setdiff(names(regeringer_total), c(cols_min)),
  measure.vars = cols_min,
  variable.name = "minister_nr",
  value.name = "ministerpost_nr_post"
)
regeringer_total[navn == "Rasmus Prehn"]
regeringer_total[ministerpost_ny %like% "minister for"]




regeringer_total_long = regeringer_total_long_stats[ministerpost_nr_post != "" &
                                                ministerpost_nr_post != "statsminister"]

library(survival)
library(survminer)
km0 <-
  survival::survfit(Surv(time = time, event = early) ~ 1, data = regeringer_total_long)
ggsurvplot(
  km0,
  risk.table = TRUE,
  xlim = c(0, 4),
  break.time.by = 0.5, 
  ylab = "Sandsynlighed for at være minister",
  xlab = "Tid siden ansættelse"
)


hyppige = regeringer_total_long[decimal_date(start) > 1945 &
                                  ministerpost_nr_post != "" &
                                  ministerpost_nr_post != "statsminister", .N, by = ministerpost_nr_post][order(-N)]

temp = regeringer_total_long[decimal_date(start) > 1945 &
                               ministerpost_nr_post %in% hyppige$ministerpost_nr_post, ]
ministerpost <-
  survfit(Surv(time = time, event = early) ~ ministerpost_nr_post,
          data = temp)


c1 = coxph(Surv(time = time, event = early) ~ ministerpost_nr_post,
           temp)
summary(c1)
b1 = broom::tidy(c1)
ggsurvplot(ministerpost)

library(heaven)

regeringer_total_long = regeringer_total_long[start < stop]
cols <- c("r_stop", "start", "stop")
regeringer_total_long[, (cols) := lapply(.SD, as.Date), .SDcols = cols]
regeringer_total_long[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

seqs = seq(
  min(regeringer_total_long$start, na.rm = TRUE),
  max(regeringer_total_long$stop, na.rm = TRUE),
  30
)

reg_lexis = lexisSeq(
  regeringer_total_long[start > dmy(05111945)],
  invars = c("navn", "start", "stop", "early"),
  varname = NULL,
  splitvector = seqs,
  format = "vector",
  value = "months"
)
reg_lexis[time > 3, ]
reg_lexis[, (cols) := lapply(.SD, as_date, origin = "1970-01-01"), .SDcols = cols]
regeringer_total_long[, (cols) := lapply(.SD, as_date, origin = "1970-01-01"), .SDcols = cols]
reg_lexis[, dur := as.numeric(stop - start)]
reg_lexis[, year := year(start)]
reg_lexis[, fac := factor(navn)]
library(mgcv)
reg_lexis[dur == 0 & early == 1, ]
reg_lexis <- reg_lexis[dur > 0]

g1 = bam(
  early ~ ministerpost_nr_post + s(year, k = 40)+s(fac, bs = "re"),
  data = reg_lexis,
  offset = log(dur),
  family = poisson(link = "log"),
  method = "fREML",
  discrete = TRUE
)

reg_obs = reg_lexis[, .(
  observations = uniqueN(navn),
  events = sum(early),
  pyrs = sum(dur) / 365.25
), by = ministerpost_nr_post]
reg_obs[, inci := events / pyrs]

b1 = broom::tidy(g1, parametric = TRUE,conf.int = TRUE)
setDT(b1)
b1[, ministerpost_nr_post := str_remove(term, "ministerpost_nr_post")]
b1 = reg_obs[b1, on = "ministerpost_nr_post"]
b1$term = NULL
cols = c(
  "observations",
  "events",
  "pyrs",
  "inci",
  "estimate",
  "std.error",
  "statistic",
  "p.value",
  "conf.low",
  "conf.high"
)
cols_exp = c("estimate" ,"conf.low","conf.high")
b1[, (cols_exp) := lapply(.SD, exp), .SDcols = cols_exp]

b1[, (cols) := lapply(.SD, round, digits = 2), .SDcols = cols]
total = b1[order(-observations),]
dput(names(sapply(b1, is.numeric)))

b1[, ]

## rød vs blå
reg_lexis_after = reg_lexis[start > dmy(05111945), ]
reg_lexis_after[,farve := fifelse(grepl("Socialdemokratiet",x = `Parti(er)`), "Rød","Blå")]
temp = reg_lexis_after[farve == "Rød"]
g1 = bam(
  early ~ ministerpost_nr_post + s(year, k = 40)+s(fac, bs = "re"),
  data = temp,
  offset = log(dur),
  family = poisson(link = "log"),
  method = "fREML",
  discrete = TRUE
)

reg_obs = temp[, .(
  observations = uniqueN(navn),
  events = sum(early),
  pyrs = sum(dur) / 365.25
), by = ministerpost_nr_post]
reg_obs[, inci := events / pyrs]

b1 = broom::tidy(g1, parametric = TRUE,conf.int = TRUE)
setDT(b1)
b1[, ministerpost_nr_post := str_remove(term, "ministerpost_nr_post")]
b1 = reg_obs[b1, on = "ministerpost_nr_post"]
b1$term = NULL
cols = c(
  "observations",
  "events",
  "pyrs",
  "inci",
  "estimate",
  "std.error",
  "statistic",
  "p.value",
  "conf.low",
  "conf.high"
)
cols_exp = c("estimate" ,"conf.low","conf.high")
b1[, (cols_exp) := lapply(.SD, exp), .SDcols = cols_exp]

b1[, (cols) := lapply(.SD, round, digits = 2), .SDcols = cols]
red = b1[order(-observations),]

## blue
reg_lexis_after = reg_lexis[start > dmy(05111945), ]
reg_lexis_after[,farve := fifelse(grepl("Socialdemokratiet",x = `Parti(er)`), "Rød","Blå")]
temp = reg_lexis_after[farve == "Blå"]
g1 = bam(
  early ~ ministerpost_nr_post + s(year, k = 40)+s(fac, bs = "re"),
  data = temp,
  offset = log(dur),
  family = poisson(link = "log"),
  method = "fREML",
  discrete = TRUE
)

reg_obs = temp[, .(
  observations = uniqueN(navn),
  events = sum(early),
  pyrs = sum(dur) / 365.25
), by = ministerpost_nr_post]
reg_obs[, inci := events / pyrs]

b1 = broom::tidy(g1, parametric = TRUE,conf.int = TRUE)
setDT(b1)
b1[, ministerpost_nr_post := str_remove(term, "ministerpost_nr_post")]
b1 = reg_obs[b1, on = "ministerpost_nr_post"]
b1$term = NULL
cols = c(
  "observations",
  "events",
  "pyrs",
  "inci",
  "estimate",
  "std.error",
  "statistic",
  "p.value",
  "conf.low",
  "conf.high"
)
cols_exp = c("estimate" ,"conf.low","conf.high")
b1[, (cols_exp) := lapply(.SD, exp), .SDcols = cols_exp]

b1[, (cols) := lapply(.SD, round, digits = 2), .SDcols = cols]

blue = b1[order(-observations),]

dput(names(sapply(b1, is.numeric)))
regeringer_total_long[navn == "Mimi Jakobsen"]
b1[, ]





blue$farve = "Blå"
red$farve = "Rød"
redblue = rbind(blue,red)

totallos = reg_lexis[, .(
  observations = uniqueN(navn),
  events = sum(early),
  pyrs = sum(dur) / 365.25
), by = ministerpost_nr_post]

danger = totallos[events > 5,ministerpost_nr_post]
ggplot(redblue[ministerpost_nr_post %in% danger]) +
  geom_linerange(
    aes(
      x = ministerpost_nr_post,
      y = estimate,
      col = farve,
      ymin = conf.low,
      ymax = conf.high
    ),
    position = position_dodge(width = 1),
    size = 1.5
  ) +
  scale_y_log10() +
  coord_flip()  + 
  ggsci::scale_color_jama() + 
  geom_hline(yintercept = 1)


plot(g1)

reg_1945 = regeringer_total[decimal_date(start) > 1945]

reg_1945[, .N, by = parti][order(-N)]
reg_1945[early == 1, .N, by = parti][order(-N)]

reg_1945[, n_ministers := length(unique(ministerpost)), by = regering]

write.csv(regeringer_total, "danish_cabinets.csv")

library(survival)
coxph(Surv(time = time, event = early) ~ ministerpost,
      data = regeringer_total[decimal_date(start) > 1945 &
                                ministerpost %in% hyppige, ])
