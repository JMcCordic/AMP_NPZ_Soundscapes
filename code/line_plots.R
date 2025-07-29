library(here)
library(PAMscapes)
library(dbplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(suntools)
library(stringr)

#### ---- loading in deployments --- #####

##first deployment
#nrs11Folder <- ('/Users/isabella.garfield/Documents/GitHub/Mid-Atl-Soundscapes/data/DBs/202306DB03')
#nrsFiles <- list.files(nrs11Folder, pattern='nc$', full.names=TRUE)
#nrsData1 <- loadMultiscapeData(nrs11Folder, timeBin = "1minute")

#second deployment
nrs11Folder2 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_VA/NEFSC_VA_202404_ES01/NC')
nrsFiles2 <- list.files(nrs11Folder2, pattern='nc$', full.names=TRUE)
nrsData2 <- loadMultiscapeData(nrs11Folder2, timeBin = "1hour")

#third deployment
nrs11Folder3 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_VA/NEFSC_VA_202404_ES02/NC')
nrsFiles3 <- list.files(nrs11Folder3, pattern='nc$', full.names=TRUE)
nrsData3 <- loadMultiscapeData(nrs11Folder3, timeBin = "1hour")

#fourth deployment
nrs11Folder4 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_VA/NEFSC_VA_202404_ES03/NC')
nrsFiles4 <- list.files(nrs11Folder4, pattern='nc$', full.names=TRUE)
nrsData4 <- loadMultiscapeData(nrs11Folder4, timeBin = "1hour")

#fifth deploymet
nrs11Folder5 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_VA/NEFSC_VA_202405_CB01/NC')
nrsFiles5<- list.files(nrs11Folder5, pattern='nc$', full.names=TRUE)
nrsData5 <- loadMultiscapeData(nrs11Folder5, timeBin = "1hour")

#### --- joining deployments --- #####

nrsData <- nrsData2 %>%
 full_join(nrsData3) %>%
 full_join(nrsData4) #%>%
# full_join(nrsData4) %>%
# full_join(nrsData5) 
  


#### suntools ####
help(suntools)

location <- matrix(c(-74.83, 37.68), nrow = 1)
date_Time <- as.POSIXct("2023-09-16", tz = "UTC")

# once you have adjusted the values above, all the code below should run smoothly.

Sunrise <- sunriset(crds = location, dateTime = date_Time, direction = "sunrise", POSIXct.out = TRUE)
sunrise_dechour <- Sunrise %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))


Sunset <- sunriset(crds = location, dateTime = date_Time, direction = "sunset", POSIXct.out = TRUE)
sunset_dechour <- Sunset %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))

civil_dawn <- crepuscule(crds = location, dateTime = date_Time, solarDep = 6, direction = "dawn", POSIXct.out = TRUE)
civil_dawn_dechour <- civil_dawn %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))

nautical_dawn <- crepuscule(crds = location, dateTime = date_Time, solarDep = 12, direction = "dawn", POSIXct.out = TRUE)
nautical_dawn_dechour <- nautical_dawn %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))

astronomical_dawn <- crepuscule(crds = location, dateTime = date_Time, solarDep = 18, direction = "dawn", POSIXct.out = TRUE)
astronomical_dawn_dechour <- astronomical_dawn %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))


civil_dusk <- crepuscule(crds = location, dateTime = date_Time, solarDep = 6, direction = "dusk", POSIXct.out = TRUE)
civil_dusk_dechour <- civil_dusk %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))

nautical_dusk <- crepuscule(crds = location, dateTime = date_Time, solarDep = 12, direction = "dusk", POSIXct.out = TRUE)
nautical_dusk_dechour <- nautical_dusk %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))

astronomical_dusk <- crepuscule(crds = location, dateTime = date_Time, solarDep = 18, direction = "dusk", POSIXct.out = TRUE)
astronomical_dusk_dechour <- astronomical_dusk %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))



#### --- Plotting Diel Trends --- ####

#### extract octave levels ####
#skip if using HMD instead of Ol

nrsData.oct <- createOctaveLevel(nrsData5, type='ol') 

# restructuring data to extract specific band during a specific month, ordered by hour of the day to analyze diel patterns #

nrsData.oct.mhd <- nrsData5 %>%
  mutate(hour = substr(format(UTC,"%Y:%m:%d %H:%M:%S"), 12, 13)) %>% #lubridate doesn't recognize hour 00, so this is extracting the characters themselves
  mutate(hour = as.numeric(hour)) %>%
  filter(month(UTC) == 5) %>% #adjust month
  mutate(day = day(UTC)) %>%
  select(UTC, OL_500, day, hour) #adjust HMD or Ol value here

# calculating median, .1 and .9 quantile values for each hour #

nrsData.medoct <- nrsData.oct.mhd %>%
  group_by(hour) %>%
  na.omit(HMD_302) %>% #change HMD or Ol value here
  summarise(
    avg_ol = median(HMD_302), #change HMD or Ol value here
    q1_ol = quantile(HMD_302, 0.1), #0.1 quantile SPL, change HMD or Ol value here
    q9_ol = quantile(HMD_302, 0.9)  #0.9 quantile SPL, change HMD or Ol value here
  ) %>%
  arrange(hour) 



# ------------- Adding multiple deployments to one plot as different lines -------------#

# df 1 -------- #
nrsData.oct2 <- createOctaveLevel(nrsData2, type='ol') 

# restructuring data to extract specific band during a specific month, ordered by hour of the day to analyze diel patterns #

nrsData.oct.mhd2 <- nrsData.oct2 %>%
  mutate(hour = substr(format(UTC,"%Y:%m:%d %H:%M:%S"), 12, 13)) %>% #lubridate doesn't recognize hour 00, so this is extracting the characters themselves
  mutate(hour = as.numeric(hour)) %>%
  filter(month(UTC) == 9) %>% #adjust month
  mutate(day = day(UTC)) %>%
  select(UTC, OL_2000, day, hour) #adjust freq extracted

# calculating median, .1 and .9 quantile values for each hour #

nrsData.medoct2 <- nrsData.oct.mhd2 %>%
  group_by(hour) %>%
  na.omit(OL_2000) %>%
  summarise(
    ES01 = median(OL_2000), #change freq if desired
    q1_ol2 = quantile(OL_2000, 0.1), #0.1 quantile SPL
    q9_ol2 = quantile(OL_2000, 0.9)  #0.9 quantile SPL
  ) %>%
  arrange(hour) 

# df 2 -------- #
nrsData.oct3 <- createOctaveLevel(nrsData3, type='ol') 

# restructuring data to extract specific band during a specific month, ordered by hour of the day to analyze diel patterns #

nrsData.oct.mhd3 <- nrsData.oct3 %>%
  mutate(hour = substr(format(UTC,"%Y:%m:%d %H:%M:%S"), 12, 13)) %>% #lubridate doesn't recognize hour 00, so this is extracting the characters themselves
  mutate(hour = as.numeric(hour)) %>%
  filter(month(UTC) == 9) %>% #adjust month
  filter(day(UTC) >= 16) %>% #adjust day
  mutate(day = day(UTC)) %>%
  select(UTC, OL_2000, day, hour) #adjust freq extracted

# calculating median, .1 and .9 quantile values for each hour #

nrsData.medoct3 <- nrsData.oct.mhd3 %>%
  group_by(hour) %>%
  na.omit(OL_2000) %>%
  summarise(
    ES02 = median(OL_2000), #change freq if desired
    q1_ol3 = quantile(OL_2000, 0.1), #0.1 quantile SPL
    q9_ol3 = quantile(OL_2000, 0.9)  #0.9 quantile SPL
  ) %>%
  arrange(hour) 

# df 3 -------- #
nrsData.oct4 <- createOctaveLevel(nrsData4, type='ol') 

# restructuring data to extract specific band during a specific month, ordered by hour of the day to analyze diel patterns #

nrsData.oct.mhd4 <- nrsData.oct4 %>%
  mutate(hour = substr(format(UTC,"%Y:%m:%d %H:%M:%S"), 12, 13)) %>% #lubridate doesn't recognize hour 00, so this is extracting the characters themselves
  mutate(hour = as.numeric(hour)) %>%
  filter(month(UTC) == 8) %>% #adjust month
 # filter(day(UTC) <= 16) %>% #adjust month
  mutate(day = day(UTC)) %>%
  select(UTC, OL_2000, day, hour) #adjust freq extracted

# calculating median, .1 and .9 quantile values for each hour #

nrsData.medoct4 <- nrsData.oct.mhd4 %>%
  group_by(hour) %>%
  na.omit(OL_2000) %>%
  summarise(
    ES03 = median(OL_2000), #change freq if desired
    q1_ol4 = quantile(OL_2000, 0.1), #0.1 quantile SPL
    q9_ol4 = quantile(OL_2000, 0.9)  #0.9 quantile SPL
  ) %>%
  arrange(hour) 

nrsData.medoct <- nrsData.medoct2 %>%
  full_join(nrsData.medoct3) %>%
  full_join(nrsData.medoct4)

nrsData.medoct_long <- nrsData.medoct %>%
  pivot_longer(cols = c(ES01, ES02, ES03),
               names_to = "site",
               values_to = "avg_ol")






# --- line plot-------- #
# If not using twilight hours, eliminate the geom_vlines in the code below. If using the twilight hours, the geom_vlines are automatically calculated from the suntools calculations earlier in this code. The color order goes from astronomical twilight (darkest), nautical twilight (slightly lighter), civil twilight (even lighter) to sunrise (lightest) and vice versa (sunset = lightest, and gradient descends)

plot2000.med <- ggplot(nrsData.medoct, aes(x = hour, y = avg_ol)) + #group = site if plotting multiple sites
  geom_line() + #aes(color = site), size = 1 if plotting multiple sites
  #geom_point() +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 22, by = 2)) +
  scale_color_manual(values = c("ES01" = "gray20", "ES02" = "gray50", "ES03" = "gray80")) + # if plotting multiple sites
  geom_ribbon(aes(x = hour, ymin = q1_ol, ymax = q9_ol), alpha = 0.2, fill = "gray") +
  geom_vline(xintercept = civil_dusk_dechour$decimal_hour, color = "#F76218", linetype = "solid",size =  1, alpha = .5) +
  geom_vline(xintercept = nautical_dusk_dechour$decimal_hour, color = "#B10065", linetype = "solid",size =  .7, alpha = .5) +
  geom_vline(xintercept = astronomical_dusk_dechour$decimal_hour, color = "#740580", linetype = "solid",size =  .4, alpha = .5) +
  geom_vline(xintercept = civil_dawn_dechour$decimal_hour, color = "#F76218", linetype = "solid",size =  1, alpha = .5) +
  geom_vline(xintercept = nautical_dawn_dechour$decimal_hour, color = "#B10065", linetype = "solid",size =  .7, alpha = .5) +
  geom_vline(xintercept = astronomical_dawn_dechour$decimal_hour, color = "#740580", linetype = "solid",size =  .4, alpha = .5) +
  geom_vline(xintercept = sunrise_dechour$decimal_hour, color = "#F2C447", linetype = "solid", size =  2, alpha = .5) +
  geom_vline(xintercept = sunset_dechour$decimal_hour, color = "#F2C447", linetype = "solid", size = 2, alpha = .5) +
  theme_bw() +
  labs(title="HMD_15014: Diel fluctuations during September 2024 at ES01", x="Hour (UTC)", y = "dB")

plot2000.med



ggsave(here("figs/OL_plots/ES01", "line16000ol.sepES01.png"), plot2000.med, width= 7, height= 4, units= "in")



