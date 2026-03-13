#' ---------------------------------------------------------
#' 
#' 
#' Summarize vessel & bio activity across 9 marine parks
#' 
#' Updated Sept 2025
#' J. McCordic
#' ---------------------------------------------------------


#### Load libraries ####

# NOAA-approved tidyverse
tidyverse_short<-c("broom","cli","crayon","dbplyr","dplyr","dtplyr","forcats","ggplot2","googledrive","googlesheets4","hms","httr","jsonlite","lubridate","magrittr","modelr","pillar","purrr","readr","readxl","reprex","rlang","rstudioapi","rvest","stringr","tibble","tidyr","xml2") 
lapply(tidyverse_short, require, character.only = TRUE) 

# source helper file with various data wrangling functions
source("code/AMP_summary_ves_funs.R")

# set higher digits for using sci notation to get around importing weird date format from Excel
options(scipen = 13) 

#### Load in data ####


# Assign user-defined inputs ----------------------------------------------

# Since server folders are in a standard structure, use parent folder to get list of all deployments
dep_names <- list.dirs(tk_choose.dir(caption = "Select parent dir for all deployment folders"), recursive = FALSE, full.names = FALSE)

# select the deployment(s) to be plotted
dep_list <- dlg_list(title = "Select deployments to plot", choices = dep_names, multiple = TRUE)$res |>
  str_sub(start = 16)

# apply getDeploymentInfo() from AMP_pkgs_funs.R to each deployment
#   prompts user for site name, start/end date, and time zones
dep_info <- dep_list |>
  map(~getDeploymentInfo(.)) |>
  set_names(dep_list)


# Load data ---------------------------------------------------------------

# For each deployment in the dep_info list, load in hourly presence table & compiled selection table
hp_og <- dep_info |>
  map(~read_csv(choose.files(caption = paste0({.}$site_id, {.}$dep_id, " Hourly Presence sheet .csv"))))|>
  # use imap() to get info based on index of each iteration
  # in this case, we want the name of the list element, designated as ".y"
  imap(~mutate(., Dep_ID = .y))



# # For each deployment in the dep_info list, load in compiled selection table
# selns_og <- dep_info |>
#   map(~read_delim(choose.files(caption = paste0({.}$site_id, {.}$dep_id, " Complied seln table .txt"))))|>
#   # use imap() to get info based on index of each iteration
#   # in this case, we want the name of the list element, designated as ".y"
#   imap(~mutate(., Dep_ID = .y)) 
# 
# 
# # For each deployment in the dep_info list, load in I-O excel files
# ins_out_og <- dep_info |>
#   map(~read_xlsx(choose.files(caption = paste0({.}$site_id, {.}$dep_id, " I-O table .xlsx"))))|>
#   # use imap() to get info based on index of each iteration
#   # in this case, we want the name of the list element, designated as ".y"
#   imap(~mutate(., Dep_ID = .y))

# Mutate data ------------------------------------------------------------

#' For each hp table in hp_og_bio
#' - Want to end up with common set of hp columns:
#'    - humpback
#'    - pygmy_blue
#'    - minke
#'    - fish_chorus
#'    - sperm_whale
#'    - uk_bal
#'    - dolphin 



#### Add some columns, update time zone ####

# make named vector to rename columns from old naming format 
#   CG, WP
ren_cols <- c(Begin_Date = Start.date,
              Begin_Hour = Start.hour,
              Humpback = HumpbackPresent,
              Dolphin = DolphinPresent,
              Low_baleen = Baleen,
              Mid_baleen = Minke,
              Fish_chorus = FishChorusP,
              CPA = CPAPresent,
              TB = TBPresent,
              CPA_M = ManeuverCPAPres,
              TA_M = ManeuverTAPres,
              TA = TAPresent)


# making new_cols placeholders to add into original df if they don't exist at all
new_cols <- c("mn_song" = 0,
              "bal_pbw" = 0, 
              "fish_c" = 0,
              "pm_click" = 0,
              "bal_uk" = 0,
              "dol_w" = 0
              )


hp_allcols <- hp_og_bio |>
  # add columns for all bio categories if they don't exist
  map(~add_column(., !!!new_cols[!names(new_cols) %in% names(.)])
  ) 


# Hourly presence
hp_data <- hp_allcols |>
  map(~rename(., 
              any_of(c("TR" = "Transit",
              "M" = "Maneuver",
              "Dep" = "Dep_ID")))) |>
  map(~relocate(., 
                TR, .after = last_col())) |>
  map(~relocate(., 
                Not_Assigned, .after = last_col())) |>
  map(~relocate(., 
                M, .after = last_col())) |>
  map2(.y = dep_info, 
       ~mutate(.x,
               Site_ID = {.y}$site_id, 
               Dep_ID = {.y}$dep_id,
               Total_Vessels = rowSums(across(TR:M)),
               # create y/n column for vessel presence
               ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
               # pull hour as time object along with date
               Hr_time = paste(Begin_Date, Begin_Hour, ":00"),
               # get components of date-time object and assign time zone
               Hr_time = parse_date_time(Hr_time, "ymd H:M", tz = {.y}$tz_files),
               # change to local time zone
               Hr_local = with_tz(Hr_time, tzone = {.y}$tz_local),
               # pull out new hour and date in local time
               Begin_Hour_loc = as.numeric(hour(Hr_local)),
               Begin_Date_loc = date(Hr_local),
               # add weekday column
               Weekday = weekdays(Begin_Date_loc))) |>
  bind_rows()

# Create CSV to make data input easier. Maybe eventually add some logic in script to bypass generating this?
write_csv(hp_data, "output/total_ves_hp_by_site.csv")



# Selection tables --------------------------------------------------------


# reshape selection tables to plot duration
selns_data <- selns_og |>
  map(~rename(., "Dep" = "Dep_ID")) |>
  map2(.y = dep_info, 
       ~mutate(.x, 
               Begin_Date = ymd(Begin_Date),
               Begin_file_date = ymd(Begin_file_date),
               Site_ID = {.y}$site_id, 
               # Dep = {.y}$dep_id,
               # re-code anything with a maneuver as "Maneuver" (e.g., "Maneuver+CPA" = "Maneuver")
               Behavior = gsub(pattern = ".*Maneuver.*", replacement = "Maneuver", x = Behavior),
               # re-code all transit as "Transit" (e.g., "TransitA" = "Transit")
               # important that this happens AFTER the Maneuver line above so that "TransitAManeuver" doesn't get re-coded
               Behavior = gsub(pattern = ".*Transit.*", replacement = "Transit", x = Behavior),
               Behavior = gsub(pattern = "CPAManeuver", replacement = "Maneuver", x = Behavior),
               Behavior = gsub(pattern = "CPA", replacement = "Transit", x = Behavior),
               Behavior = gsub(pattern = " ", replacement = "", x = Behavior),
               DeltaHours = Delta_Time_s/3600,
               # Total_Vessels = rowSums(across(c(TR,M))),
               # pull hour as time object along with date
               Hr_time = paste(Begin_Date, Begin_Clock),
               # get components of date-time object and assign time zone
               Hr_time = parse_date_time(Hr_time, "y/m/d H:M:S", tz = {.y}$tz_files),
               # change to local time zone
               Hr_local = with_tz(Hr_time, tzone = {.y}$tz_local),
               # pull out new hour and date in local time
               Begin_Hour_loc = as.numeric(hour(Hr_local)),
               Begin_Date_loc = date(Hr_local),
               # add weekday column
               Weekday = weekdays(Begin_Date_loc))) |>
  bind_rows()

# write csv for easier input later
write.csv(selns_data, "output/total_ves_selns_data_by_site.csv")


# Bind rows to get df for inside-outside tables
# don't need to do other adjustments for time zone etc bc will join with selns_data which has
# Begin_hour_loc and Begin_date_loc fields

ins_out_data <- ins_out_og |>
  map(~select(., Filename, Date, Selection, used, pins_sm, pins_med, pins_lg, pins_ovrll, Dep_ID)) |>
  map(~mutate(., Date = as.numeric(Date),
              Date = as.character(Date),
              pins_sm = as.numeric(pins_sm),
              pins_med = as.numeric(pins_med),
              pins_lg = as.numeric(pins_lg),
              pins_ovrll = as.numeric(pins_ovrll))) |>
  bind_rows()

# write csv for easier input later
write.csv(ins_out_data, "output/total_ves_ins_out_by_site.csv")

# Small vessels
ins_vessels_small <- ins_out_data |>
  filter(used == 1) |>
  filter(pins_sm >= 0.75) |>
  mutate(begin_file_date = ymd(str_sub(Date, 1,6)),
                      seln_num = as.numeric(gsub(pattern = "S", replacement = "", x = Selection)))|>
  left_join(selns_data, 
            by = c("Dep_ID" = "Dep",
                   "seln_num" = "Selection", 
                   "begin_file_date" = "Begin_file_date"))

# Small or medium vessels
ins_vessels_smmed <- ins_out_data |>
  filter(used == 1) |>
  filter(pins_ovrll >= 0.75) |>
  mutate(begin_file_date = ymd(str_sub(Date, 1,6)),
         seln_num = as.numeric(gsub(pattern = "S", replacement = "", x = Selection)))|>
  left_join(selns_data, 
            by = c("Dep_ID" = "Dep",
                   "seln_num" = "Selection", 
                   "begin_file_date" = "Begin_file_date"))


#### Ins Park tables -- reshape into hp ####

# get counts per site per date per hour for inside vessels
# Small vessels
ins_hp_small <- inside_tables_to_hp(ins_table = ins_vessels_small) |>
  rename("man_inside_small" = "man_inside",
         "trans_inside_small" = "trans_inside",
         "total_inside_small" = "total_inside")
# Small or medium vessels
ins_hp_smmed <- inside_tables_to_hp(ins_table = ins_vessels_smmed)

# join inside vessels to total vessels
total_ins_hp <- hp_data |>
  left_join(ins_hp_small,
            by = c("Dep" = "Dep_ID",
                   "Begin_Date_loc" = "Begin_Date_loc",
                   "Begin_Hour_loc" = "Begin_Hour_loc")) |>
  left_join(ins_hp_smmed,
            by = c("Dep" = "Dep_ID",
                   "Begin_Date_loc" = "Begin_Date_loc",
                   "Begin_Hour_loc" = "Begin_Hour_loc"))


# save CSV with all sites' hourly presence (total & inside NPZ)
write.csv(total_ins_hp, "output/hourly_pres_allsites_local.csv")

