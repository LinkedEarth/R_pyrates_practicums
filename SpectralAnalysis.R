library(dplyr)
library(lubridate)
library(astrochron)
library(LambertW)

df <- read.table('https://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no=08279500&legacy=&referred_module=sw&period=&begin_date=1889-01-01&end_date=2024-05-20', skip=35, sep="\t")
names(df) <- c('agency', 'site_no', 'datetime','discharge','code')
head(df)


df2 <- df %>%
  dplyr::select(datetime, discharge) %>%
  mutate(datetime = as.Date(datetime))

ggplot(df2, aes(x=discharge)) +
  geom_histogram(binwidth = 1000) +
  ggtitle("Rio Grand Discharge Distribution") +
  theme_light()

ggplot(df2, aes(x=datetime, y=discharge)) +
  geom_line() +
  ggtitle("Rio Grand Discharge") +
  theme_light()

discharge_monthly <- df2 %>%
  group_by(Date = floor_date(ymd(df2$datetime), '1 month')) %>%
  summarise(discharge = mean(discharge, na.rm = TRUE), .groups = 'drop')


ggplot(discharge_monthly, aes(x=Date, y=discharge)) +
  labs(title = "Rio Grande at Embudo, NM (monthly)",
       x="Year (CE)",
       y="dicharge (cf/s)") +
  geom_line() +
  ggtitle("Rio Grand Discharge") +
  theme_light()

missing_vals <- discharge_monthly$Date[which(is.na(discharge_monthly$discharge))]
missing_vals

df3 <- discharge_monthly %>%
  dplyr::filter(Date > max(missing_vals))

hist(as.numeric(diff(df3$Date)),main = "Distribution of Time Steps", xlab = "Days")
df4 <- df2 %>%
  astrochron::linterp(dt=(365.25*1/12),genplot = F) %>%
  dplyr::filter(datetime > max(missing_vals))

ggplot(df4, aes(x=datetime, y=discharge)) +
  labs(title = "Rio Grande at Embudo, NM (30.4375-day period)",
       x="Year (CE)",
       y="dicharge (cf/s)") +
  geom_line() +
  theme_light()

df5 <- df4 %>%
  mutate(datetime = as.numeric(datetime))

par(mar=c(1,1,1,1))
mtm1 <- mtm(df5,output = 1,verbose = F) %>%
  mutate(Period = Frequency*365.25) %>%
  dplyr::select(Period, Power)

ggplot(mtm1, aes(x=Period, y=Power)) +
  labs(title = "Rio Grande discharge spectrum (mtm)") +
  geom_line() +
  theme_light()

df6 <- df5 %>%
  mutate(discharge = Gaussianize(discharge))
#weird quirk of Gaussianize()
#names(df6)[2] <- "discharge"


mtm2 <- mtm(df6,output = 1,verbose = F) %>%
  mutate(Period = Frequency*365.25) %>%
  dplyr::select(Period, Power)

ggplot(mtm2, aes(x=Period, y=Power)) +
  labs(title = "Rio Grande discharge spectrum (mtm)") +
  geom_line() +
  theme_light()

x.ts <- ts(df5[,2], frequency = 12)
stl1 <- stl(x.ts, s.window=12)
stlOut <- as.data.frame(stl1$time.series)
df7 <- data.frame("Date"=df5[,1],
                  "anomaly"=stlOut$trend[1:length(df5$datetime)])
ggplot(df7, aes(x=as.Date(Date), y=anomaly)) +
  labs(title = "Rio Grande discharge anomaly (stl)") +
  geom_line() +
  theme_light()

mtm.stl <- mtm(df7,output = 1,verbose = F) %>%
  mutate(Period = (1/12)/Frequency) %>%
  dplyr::select(Period, Power)

ggplot(mtm.stl %>% filter(Period < 100 & Period > 0.2), aes(x=Period, y=Power)) +
  scale_y_log10() +
  scale_x_reverse(trans = "log10", breaks=c(0.2,0.5,1,2,5,10,20,50,100)) +
  labs(title = "Rio Grande discharge spectrum (mtm)") +
  geom_line() +
  theme_light()




