library(reshape2)
library(tidyverse)
#library(seewave)
library(astrochron)

#Load SOI
SOI <- read.table("https://github.com/LinkedEarth/Pyleoclim_util/raw/master/pyleoclim/data/soi_data.csv", sep = ",", header = TRUE, skip = 1)
head(SOI)

#Load NAO
NAO <- read.table('https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table',header = TRUE, fill=TRUE,row.names = NULL)
head(NAO)
names(NAO)[1] <- "Year"

#Reformat NAO
NAO1 <- melt(data=NAO, id.vars='Year')
NAO <- NAO1 %>%
  mutate(datetime = lubridate::make_datetime(as.integer(NAO1$Year), unlist(lapply(NAO1$variable, function(x) which(x==month.abb))), 15)) %>%
  dplyr::select(datetime, value) %>%
  arrange(datetime)
head(NAO)

ggplot(NAO, aes(x=datetime, y=value)) +
  geom_line() +
  labs(title = "North Atlantic Oscillation",
       y="Index",
       x="Year") +
  theme_minimal()

#Evenly sampled version:
# NAO <- NAO %>%
#   mutate(datetime = seq(min(datetime), max(datetime), length.out=75*12)) %>%
#   mutate(value = approx(datetime, value, datetime)$y)
#
# ggplot(NAO, aes(x=datetime, y=value)) +
#   geom_line() +
#   labs(title = "North Atlantic Oscillation (Interpolated)",
#        y="Index",
#        x="Year") +
#   theme_minimal()

#Merge with SOI
SOI <- SOI %>%
  mutate(datetime = as.Date(format(date_decimal(Year), "%Y-%m-%d"))) %>%
  rename(SOI = Value) %>%
  dplyr::select(datetime, SOI)
head(SOI)

newDateDF <- data.frame(datetime = as.Date(round(seq(as.numeric(min(SOI$datetime)), as.numeric(max(SOI$datetime)), length.out=69*12),5)))

SOInewDate <- merge.data.frame(SOI, newDateDF, all = T)

dfAll <- merge.data.frame(NAO, SOInewDate, all = T)

dfAll <- dfAll %>%
  slice(13:2505) %>%
  mutate(NAO = approx(datetime, value, datetime)$y) %>%
  mutate(SOI = approx(datetime, SOI, datetime)$y) %>%
  select(-value) %>%
  slice(which(datetime %in% newDateDF$datetime)) %>%
  drop_na()
head(dfAll)

allLong <- melt(dfAll,id.vars = "datetime")
head(allLong)

ggplot(allLong, aes(x=datetime, y=value, group=variable)) +
  geom_line() +
  facet_wrap(~variable, ncol=1) +
  labs(title = "NAO vs SOI (Interpolated)",
       y="Index",
       x="Year") +
  theme_minimal()

lapply(c("pearson", "spearman", "kendall"), function(x) cor.test(dfAll$NAO, dfAll$SOI, method = x))

#Methods: 1-pearson, 2-spearman, 3-kendall
lapply(c(1,2,3), function(x) surrogateCor(dfAll$NAO,dfAll$SOI,nsim = 10000,cormethod = x, genplot = F, verbose = F))

dfAll$lowpassNAO <- smooth.spline(x = dfAll$datetime, y=dfAll$NAO, spar = 0.2)$y
#dfAll$lowpassNAO <- bwfilter(wave = dfAll[,2], bandpass = TRUE, from = .2, to = .9, f=30)
dfAll$lowpassSOI <- smooth.spline(x = dfAll$datetime, y=dfAll$SOI, spar = 0.2)$y
#dfAll$lowpassSOI <- bwfilter(wave = dfAll[,c(3)], bandpass = TRUE, from = .2, to = .9, f=30)

allLong2 <- melt(dfAll,id.vars = "datetime")
allLong2 <- allLong2 %>%
  mutate(group = ifelse(grepl("SOI", variable), "SOI", "NAO")) %>%
  mutate(type = ifelse(grepl("lowpass", variable), "filtered", "original"))
head(allLong2)

ggplot(allLong2, aes(x=datetime, y=value, group=group, color=type)) +
  geom_line() +
  facet_wrap(~group, ncol=1) +
  labs(title = "NAO vs SOI",
       y="Index",
       x="Year") +
  theme_minimal()

lapply(c("pearson", "spearman", "kendall"), function(x) cor.test(dfAll$lowpassNAO, dfAll$lowpassSOI, method = x))
lapply(c(1,2,3), function(x) surrogateCor(dfAll$lowpassNAO,dfAll$lowpassSOI,nsim = 10000,cormethod = x, genplot = F, verbose = F))







