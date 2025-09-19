#' -------------------------------------------------------
#' 
#' 
#' Reshaping hourly presence data to merge with PyPAM output
#' --> Run separately for each deployment
#'
#' -------------------------------------------------------


# Load libraries ----------------------------------------------------------

# NOAA-approved tidyverse
tidyverse_short<-c("broom","cli","crayon","dbplyr","dplyr","dtplyr","forcats","ggplot2","googledrive","googlesheets4","hms","httr","jsonlite","lubridate","magrittr","modelr","pillar","purrr","readr","readxl","reprex","rlang","rstudioapi","rvest","stringr","tibble","tidyr","xml2") 
lapply(tidyverse_short, require, character.only = TRUE) 
library(tcltk2)
library(svDialogs)

# source helper file
source("code/AMP_summary_ves_funs.R")


# Load data ---------------------------------------------------------------

# Assign user-defined inputs ----------------------------------------------

# # Since server folders are in a standard structure, use parent folder to get list of all deployments
# dep_names <- list.dirs(tk_choose.dir(caption = "Select parent dir for all deployment folders"), recursive = FALSE, full.names = FALSE)
# 
# # select the deployment(s) to be plotted
# dep_list <- dlg_list(title = "Select deployments to plot", choices = dep_names, multiple = TRUE)$res |>
#   str_sub(start = 16)
# 
# # apply getDeploymentInfo() from AMP_ves_summary_funs.R to each deployment
# #   prompts user for site name, start/end date, and time zones
# dep_info <- dep_list |>
#   map(~getDeploymentInfo(.)) |>
#   set_names(dep_list)
# 
# # For each deployment in the dep_info list, load in hourly presence table
# hp_og_bio <- dep_info |>
#   map(~read_csv(choose.files(caption = paste0({.}$site_id, {.}$dep_id, " Hourly Presence sheet .csv"))))|>
#   # use imap() to get info based on index of each iteration
#   # in this case, we want the name of the list element, designated as ".y"
#   imap(~mutate(., Dep_ID = .y))


deployment_id <- dlgInput("Deployment ID")$res

bio_hp_og <- read_csv(tk_choose.files(caption = "Select bio hourly presence (.csv)"))

# load in vessel hourly presence to join with bio: 2018 - 2022 deps previously compiled for vessels manuscript
hp_ves_all <- read_csv(choose.files(caption = "Select compiled vessel hourly presence .csv"))



# Mutate bio tables -------------------------------------------------------

#' For each hp table in hp_og_bio
#' - Want to end up with common set of hp columns:
#'    - humpback
#'    - pygmy_blue
#'    - minke
#'    - fish_chorus
#'    - sperm_whale
#'    - uk_bal
#'    - dolphin 
#' Can adapt this code from vessel script:
#' new_cols <- c("Maneuver" = 0,"Transit" = 0, "Not_Assigned" = 0)
#' hp_allcols <- hp_og |>
#'   # add columns for Transit and Maneuver if they don't exist
#' map(~add_column(., !!!new_cols[!names(new_cols) %in% names(.)])
#'   ) 

  # rename(
  #   # pbw_baleen = bal_pbw,
  #   # other_baleen = bal_,
  #   Fish_chorus = fish_c,
  #   Dolphin = dol_w
  # ) |>


bio_hp_data <- bio_hp_og |> 
  # rename(Begin_Date = Start.date,
  #        Begin_Hour = Start.hour,
  #        Humpback = HumpbackPresent,
  #        Dolphin = DolphinPresent,
  #        Low_baleen = Baleen,
  #        Mid_baleen = Minke,
  #        Fish_chorus = FishChorusP,
  #        CPA = CPAPresent,
  #        TB = TBPresent,
  #        CPA_M = ManeuverCPAPres,
  #        TA_M = ManeuverTAPres,
  #        TA = TAPresent) |>
  # rename(Begin_Hour = Begin_hour) |>
  mutate(Dep = str_replace(deployment_id, "PARKSAUSTRALIA_", "")) |>
  select(-c(SiteID))



# Reshape data ------------------------------------------------------------

# add columns to summarize presence
hp_data <- bio_hp_data |>
  left_join(hp_ves_all, by = c("Dep", "Begin_Date", "Begin_Hour"))

# ask user for wavfile tz -- svDialogs works decently in container (yay!)

# Time zone of sound files
tz_files <- dlg_list(title = "Original file TZ",choices = c(grep("Etc/", OlsonNames(), value=TRUE),
                                                            grep("/UTC", OlsonNames(), value=TRUE)))$res
# UTC time zone
tz_utc <- "Etc/UTC" 

#############################################################


hp_new <- hp_data |>
  mutate(
    # summarize presence
    # bal_pres = rowSums(across(ends_with("baleen")), na.rm=TRUE),
    bal_pres = ifelse(bal_uk > 0, 1, 0),
    # hbw_pres = ifelse(Humpback > 0, 1, 0),
    # hbw_pres = ifelse(mn_song > 0, 1, 0),
    # fish_c_pres = ifelse(Fish_chorus > 0, 1, 0),
    fish_c_pres = ifelse(fish_c > 0, 1, 0),
    # dol_pres = ifelse(Dolphin > 0, 1, 0),
    dol_pres = ifelse(dol_w > 0, 1, 0),
    # mnk_pres = ifelse(Mid_baleen > 0, 1, 0),
    # mnk_pres = ifelse(minke_sw > 0, 1, 0),
    # spwh_pres = rowSums(across(pm_:Spermwhale_), na.rm = TRUE),
    spwh_pres = ifelse(pm_frg > 0, 1, 0),
    # Total_Vessels = rowSums(across(c(CPA, CPA_M,TA,TA_M,TB, TRANSIT, M)), na.rm=TRUE),
    ves_pres = ifelse(Total_Vessels > 0, 1, 0),
    
    # sound_source = case_when(),
    
    # convert to UTC from wavfile time zone
    # pull hour as time object along with date
    Hour_files = paste0(Begin_Date," ", Begin_Hour, ":00"),
    
    # assign time zone to new Hour column
    Hour_files = force_tz(as_datetime(Hour_files, format = "%Y-%m-%d %H:%M"), tz = tz_files),
    # change to UTC
    Hour_UTC = with_tz(Hour_files, tz = tz_utc),
    # pull out new hour and date in local time
    Begin_Hour_UTC = as.numeric(hour(Hour_UTC)),
    Begin_Date_UTC = date(Hour_UTC),
    # add "UTC" column for date-time so it matches HMD dataset
    UTC = make_datetime(year = year(Begin_Date_UTC),
                        month = month(Begin_Date_UTC),
                        day = day(Begin_Date_UTC),
                        hour = hour(Hour_UTC))
  )


# create Source column to summarize for plotting
#   values = "pbw_only"  "hbw_only" "mnk_only" "dol_only" "fsh_only" "ves_only"
#             "no_source" "cet_ves_mix" "fsh_ves_mix"

hp_source <- hp_new |>
  select(Dep, UTC, ends_with("_pres"))

head(hp_source)


# Export CSV for each deployment --------------------------------------------------------------
write_csv(hp_source, paste0("output/", deployment_id, "_hp_for_HMD.csv"))






