#' -------------------------------------------------------
#' 
#' 
#' Reshaping hourly presence data to merge with PyPAM output
#'
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

# Since server folders are in a standard structure, use parent folder to get list of all deployments
dep_names <- list.dirs(tk_choose.dir(caption = "Select parent dir for all deployment folders"), recursive = FALSE, full.names = FALSE)

# select the deployment(s) to be plotted
dep_list <- dlg_list(title = "Select deployments to plot", choices = dep_names, multiple = TRUE)$res |>
  str_sub(start = 16)

# apply getDeploymentInfo() from AMP_ves_summary_funs.R to each deployment
#   prompts user for site name, start/end date, and time zones
dep_info <- dep_list |>
  map(~getDeploymentInfo(.)) |>
  set_names(dep_list)

# For each deployment in the dep_info list, load in hourly presence table
hp_og_bio <- dep_info |>
  map(~read_csv(choose.files(caption = paste0({.}$site_id, {.}$dep_id, " Hourly Presence sheet .csv"))))|>
  # use imap() to get info based on index of each iteration
  # in this case, we want the name of the list element, designated as ".y"
  imap(~mutate(., Dep_ID = .y))


# load in vessel hourly presence to join with bio: 2018 - 2022 deps previously compiled for vessels manuscript
hp_ves <- read_csv(choose.files(caption = "Select compiled vessel hourly presence .csv"))



# Mutate bio tables -------------------------------------------------------


#### May be more useful in the long run to work on bio selections --> hp script instead
#     to remake HP sheets in a more standardized way from the start, similar to what I 
#     did for vessels. ughhhhhhhhhhhhhh.

#     OR, maybe in the amount of time it would take to do that, I could just make 
#     individual hp tables for each NPZ and just rename everything individually like
#     I did for SMM2024


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

#'    
#'    
#'    





deployment_id <- dlgInput("Deployment ID")$res

bio_hp_data <- read_csv(tk_choose.files(caption = "Select bio hourly presence (.csv)"))
bio_hp_data <- bio_hp_data |>
  # rename(Begin_Hour = Begin_hour) |>
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
rename(
  pbw_baleen = bal_pbw,
  other_baleen = bal_,
  Fish_chorus = fish_c,
  Dolphin = dol_w
) |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y")) # |>
# select(-c(CPA, CPA_M,TA,TA_M,TB))
  
ves_hp_data <- #bio_hp_data |>
  read_csv(tk_choose.files(caption = "Select vessel hourly presence (.csv)")) |>
  rename(Begin_Hour = Begin_hour) |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
  select(SiteID, Begin_Date, Begin_Hour, CPA, CPA_M,TA,TA_M,TB, TRANSIT, M) 

bio_hp_data <- bio_hp_data |> 
  select(-c(SiteID, CPA, CPA_M,TA,TA_M,TB))
  

head(bio_hp_data)
head(ves_hp_data)

##### TO DO:
# reshape vessel HP tables
# make sure date formatted correctly in ves tables
# join to bio table


# Reshape data ------------------------------------------------------------

# add columns to summarize presence
hp_data <- bio_hp_data |>
  left_join(ves_hp_data, by = c("Begin_Date", "Begin_Hour")) |>
  mutate(Dep_ID = deployment_id)

# ask user for wavfile tz -- svDialogs works decently in container (yay!)

# Time zone of sound files
tz_files <- dlg_list(title = "Original file TZ",choices = c(grep("Etc/", OlsonNames(), value=TRUE),
                                                            grep("/UTC", OlsonNames(), value=TRUE)))$res
# UTC time zone
tz_utc <- "Etc/UTC" 

#############################################################


hp_new <- hp_data |>
  mutate(
    # assign deployment ID column
    Dep_ID = deployment_id,
    
    # summarize presence
    bal_pres = rowSums(across(ends_with("baleen")), na.rm=TRUE),
    bal_pres = ifelse(bal_pres > 0, 1, 0),
    # hbw_pres = ifelse(Humpback > 0, 1, 0),
    fish_c_pres = ifelse(Fish_chorus > 0, 1, 0),
    dol_pres = ifelse(Dolphin > 0, 1, 0),
    spwh_pres = rowSums(across(pm_:Spermwhale_), na.rm = TRUE),
    spwh_pres = ifelse(spwh_pres > 0, 1, 0),
    Total_Vessels = rowSums(across(c(CPA, CPA_M,TA,TA_M,TB, TRANSIT, M)), na.rm=TRUE),
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
  select(Dep_ID, UTC, ends_with("_pres"))#|>
  # mutate(
  #   source_bio = case_when(
  #     bal_pres == 1 & hbw_pres == 0 & dol_pres == 0 & fish_c_pres == 0 & ves_pres == 0 ~ "bal_uk_only",
  #     bal_pres == 0 & hbw_pres == 1 & dol_pres == 0 & fish_c_pres == 0 & ves_pres == 0 ~ "hbw_only",
  #     bal_pres == 0 & hbw_pres == 0 & dol_pres == 1 & fish_c_pres == 0 & ves_pres == 0 ~ "dol_only",
  #     bal_pres == 0 & hbw_pres == 0 & dol_pres == 0 & fish_c_pres == 1 & ves_pres == 0 ~ "fish_c_only",
  #     bal_pres == 0 & hbw_pres == 0 & dol_pres == 0 & fish_c_pres == 0 & ves_pres == 1 ~ "ves_only",
  #     bal_pres == 1 & hbw_pres == 1 & dol_pres == 0 & fish_c_pres == 0 & ves_pres == 0 ~ "hbw_bal",
  #     bal_pres == 0 & hbw_pres == 1 & dol_pres == 1 & fish_c_pres == 0 & ves_pres == 0 ~ "hbw_dol",
  #     bal_pres == 0 & hbw_pres == 1 & dol_pres == 0 & fish_c_pres == 1 & ves_pres == 0 ~ "hbw_fish_c",
  #     bal_pres == 0 & hbw_pres == 1 & dol_pres == 0 & fish_c_pres == 1 & ves_pres == 0 ~ "hbw_fish_c",
  #     bal_pres == 0 & hbw_pres == 1 & dol_pres == 1 & fish_c_pres == 1 & ves_pres == 0 ~ "hbw_dol_fish_c",
  #     bal_pres == 0 & hbw_pres == 1 & dol_pres == 0 & fish_c_pres == 0 & ves_pres == 1 ~ "ves_hbw",
  #     bal_pres == 0 & hbw_pres == 0 & dol_pres == 1 & fish_c_pres == 0 & ves_pres == 1 ~ "ves_dol",
  #     bal_pres == 0 & hbw_pres == 0 & dol_pres == 0 & fish_c_pres == 0 & ves_pres == 0 ~ "no_source",
  #     TRUE ~ "mixed_source"
  #   )
  # )

head(hp_source)


write_csv(hp_source, paste0("data/", deployment_id, "_hp_SMM2024.csv"))




# Export CSV for each deployment --------------------------------------------------------------


