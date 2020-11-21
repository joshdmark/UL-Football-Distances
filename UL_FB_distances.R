library(tidyverse)
library(data.table)
library(cfbscrapR)
library(sqldf)
library(RCurl)
library(geosphere)

## get stadium coordinates
stadium_coordinates_url <- getURL('https://raw.githubusercontent.com/joshdmark/UL-Football-Distances/main/stadium_coordinates.csv')
stadium_coordinates <- read.csv(text = stadium_coordinates_url, stringsAsFactors = FALSE) %>% data.frame()
rm(stadium_coordinates_url) ## remove url 
## fix column name
names(stadium_coordinates)[1] <- 'venue_id'
## make sure all stadium zips have 5 characters
stadium_coordinates$stadium_zip <- stringr::str_pad(string = stadium_coordinates$stadium_zip, side = 'left', width = 5, pad = '0')

## get team_ids
team_ids_url <- getURL('https://raw.githubusercontent.com/joshdmark/UL-Football-Distances/main/team_ids.csv')
team_ids <- read.csv(text = team_ids_url, stringsAsFactors = FALSE) %>% data.frame()
rm(team_ids_url) ## remove url 
## fix column name
names(team_ids)[1] <- 'team_name'
team_ids <- team_ids %>% 
  mutate(team_name = stringr::str_trim(team_name))


## seasons since joining ACC
seasons <- 2014:2020

## loop to get schedule for ACC seasons
full_schedule <- data.frame()
betting_lines <- data.frame()

## begin loop 
for (s in seasons){
  ## print season number 
  print(s)
  
  ## get single season schedule 
  tmp_schedule <- cfb_game_info(year = s, team = 'Louisville') %>% 
    data.frame() %>% 
    mutate(team = 'Louisville', 
           opponent = ifelse(home_team == 'Louisville', away_team, home_team), 
           team_points = ifelse(home_team == 'Louisville', home_points, away_points), 
           opp_points = ifelse(home_team == 'Louisville', away_points, home_points), 
           win_loss = ifelse(team_points > opp_points, 'W', 
                             ifelse(team_points < opp_points, 'L', NA)))
  
  ## combine single season schedule with all seasons 
  full_schedule <- bind_rows(full_schedule, tmp_schedule)
  
  ## get lines
  tmp_lines <- cfbscrapR::cfb_betting_lines(team = 'Louisville', year = s) %>% 
    mutate(over_under = ifelse(is.na(over_under), 0, over_under)) %>% 
    mutate(team = 'Louisville', 
           total_points = home_score + away_score, 
           hit_ou_ind = ifelse(over_under != 0 & (total_points > over_under), 1, 0),
           opponent = ifelse(home_team == 'Louisville', away_team, home_team), 
           team_score = ifelse(home_team == 'Louisville', home_score, away_score),
           opp_score = ifelse(home_team == 'Louisville', away_score, home_score),
           win_ind = ifelse(team_score > opp_score, 1, 0), 
           season_week = paste(season, week, sep = '_')) %>% 
    filter(stringr::str_to_lower(season_type) == 'regular')
  
  ## change columns with "spread" in name to "gambling_line"
  ## "spread" column name causes issues, "spread" is a function name
  names(tmp_lines) <- gsub(pattern = 'spread', replacement = 'gambling_line', x = names(tmp_lines))
  
  ## re-format gambling lines 
  tmp_lines <- tmp_lines %>% 
    mutate(negative_mov = opp_score - team_score, 
           gambling_line = as.numeric(gambling_line), 
           team_gambling_line = ifelse(home_team == 'Louisville', gambling_line, gambling_line*-1), 
           cover_ind = ifelse(negative_mov < team_gambling_line, 1, 0))
  
  ## add to betting_lines df
  betting_lines <- bind_rows(betting_lines, tmp_lines)
}

## remove for space 
rm(s, seasons, tmp_lines, tmp_schedule)

## Cardinal Stadium lat & lon
UL_coordinates <- stadium_coordinates %>% 
  filter(home_team == 'Louisville') %>% 
  select(stadium_lat, stadium_lon)

## Cardinal Stadium Zip
UL_zip <- stadium_coordinates %>% 
  filter(home_team == 'Louisville') %>% 
  select(stadium_zip)

## add stadium coordinates to full_schedule 
full_schedule <- sqldf("select fs.*, sc.stadium_lat, sc.stadium_lon, sc.stadium_zip
             from full_schedule fs 
             join stadium_coordinates sc on fs.venue_id = sc.venue_id") %>% 
  data.frame() %>% 
  mutate(UL_lat = UL_coordinates$stadium_lat, 
         UL_lon = UL_coordinates$stadium_lon, 
         UL_zip = UL_zip$stadium_zip)

## remove for space
rm(UL_coordinates)

## get distances between each stadium
distances <- data.frame()
for (i in 1:nrow(full_schedule)){
  # print(i)
  select_row <- full_schedule[i, ]
  lon1 <- select_row$stadium_lon
  lat1 <- select_row$stadium_lat
  lon2 <- select_row$UL_lon
  lat2 <- select_row$UL_lat
  ## get Haversine distance in METERS
  dist <- distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
  select_row <- select_row %>% cbind(dist_m = dist)
  
  distances <- suppressWarnings(bind_rows(distances, select_row))
  
  ## convert distances to MI, KM
  distances <- distances %>% 
    mutate(dist_km = dist_m * 0.001, 
           dist_mi = dist_m * 0.000621371)
  
  ## remove for space 
  rm(lat1, lat2, lon1, lon2, dist, i, select_row)
}

## change name of distances to full_schedule
full_schedule <- distances 
rm(distances)

## BYE WEEKS
# 2014: 9, 12
# 2015: 6, 14
# 2016: 6, 14
# 2017: 10, 14
# 2018: 8, 14
# 2019: 5, 10
# 2020: 1, 5, 11 
bye_weeks_df <- data.frame(
  season = c(2014, 2014, 2015, 2015, 2016, 2016, 2017, 2017, 2018, 2018, 2019, 2019, 2020, 2020, 2020), 
  week = c(9, 12, 6, 14, 6, 14, 10, 14, 8, 14, 5, 10, 1, 5, 11), 
  opponent = 'BYE WEEK'
)

## add bye weeks
full_schedule <- bind_rows(full_schedule, bye_weeks_df) %>% 
  arrange(season, week)

## add MOV, game_dt 
full_schedule <- full_schedule %>% 
  mutate(MOV = team_points - opp_points, 
         game_dt = lubridate::ymd(stringr::str_sub(start_date, 1, 10)))

## seasons since joining ACC
seasons <- 2014:2020
all_stats <- data.frame() 
## get stats for each season 
for(s in seasons){
  print(s)
  ## get one season stats 
  team_stats <- cfb_game_team_stats(year = s, team = 'Louisville') %>% data.frame()
  ## remove 'allowed' columns 
  team_stats <- team_stats[, !grepl(pattern = 'allowed', x = names(team_stats))]
  ## add season stats to all_stats
  all_stats <- bind_rows(all_stats, team_stats)
}

## clean all_stats
all_stats <- all_stats %>% 
  mutate(down3_eff = third_down_eff, 
         down4_eff = fourth_down_eff,
         penalties_yards_total = total_penalties_yards) %>% 
  separate(third_down_eff, into = c('down3_success', 'down3_attempts'), sep = '-') %>% 
  separate(fourth_down_eff, into = c('down4_success', 'down4_attempts'), sep = '-') %>% 
  separate(total_penalties_yards, into = c('penalties', 'penalty_yardage'), sep = '-')

## add stats to full_schedule
full_schedule <- sqldf("select fs.*, a.*
              from full_schedule fs 
              left join all_stats a on fs.id = a.game_id")

## gambling info for teamrankings games 
gambling_info1 <- betting_lines %>% 
  filter(provider == 'teamrankings') %>% 
  filter(!as.character(game_id) %in% c('400547780', '400756934', '400937484', '401112442'))

## get gambling info for games that teamrankings does not have 
gambling_info2 <- betting_lines %>% 
  filter(provider == 'consensus') %>% 
  filter(as.character(game_id) %in% c('400547780', '400756934', '400937484', '401112442', '401257934'))

## manually add missing data for Indiana State game
## ISU 401013101
ISU_game <- data.frame(game_id = 401013101, 
                       formatted_gambling_line = 'Louisville -42', 
                       over_under = '68.5', 
                       total_points = 38, 
                       hit_ou_ind = 0, 
                       negative_mov = -24, 
                       team_gambling_line = -42, 
                       cover_ind = 0)

## combine all gambling info 
gambling_info <- bind_rows(gambling_info1, gambling_info2, ISU_game)

## add gambling info to full schedule 
full_schedule <- sqldf("select fs.*, 
                          g.formatted_gambling_line, g.over_under, g.total_points, g.hit_ou_ind, g.negative_mov, g.team_gambling_line, g.cover_ind
                          from full_schedule fs 
                          left join gambling_info g on fs.id = g.game_id")

## remove bad UVA game
full_schedule <- full_schedule %>% 
  mutate(remove_game_ind = ifelse(season == 2020 & week == 11 & is.na(id), 1, 0)) %>% 
  filter(remove_game_ind != 1) %>%
  select(-remove_game_ind)

## change BC game to week 14
full_schedule <- full_schedule %>% 
  data.table() %>% 
  .[season == 2020 & opponent == 'Boston College' & substr(start_date, start = 1, stop = 10) == '2020-12-12', 
    week := 14] %>% 
  data.frame()

## write final file for tableau 
fwrite(full_schedule, "C:/Users/joshua.mark/OneDrive - Accenture/Desktop/Sports/UL Football/UL_football_distances.csv")
