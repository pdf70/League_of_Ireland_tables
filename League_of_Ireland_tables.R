# Filename: "League_of_Ireland_tables.R"

# Reads in data from wikipedia of history of all League of Ireland tables
# Note that the format of the input data may change as people change wikipedia entries

# Team colours sourced from https://imagecolorpicker.com/en.

# Retrieve previous work from:
#setwd(output_path) 
#load(file = "league_ireland_tables_raw.Rdata")     # list - "tables"
#load(file="league_ireland_tables.Rdata")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries & directories

# Set directory paths
path = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
input_path = paste(path, "/Input", sep="")
output_path = paste(path, "/R_output", sep="")
setwd(path)

# Specify packages (libraries) that are used
library(lubridate)
library(tidyverse)
library(scales)
library(rvest)    # Reading tables from a web page


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parameters 

# From 1921-22 to 2002-03, then 2003 to 2024
start_yr = seq(1921, 2024, by = 1)
end_yr = start_yr + ifelse(start_yr <= 2002, 1, 0)
seasons_def = paste(start_yr, "-", substr(end_yr,3,4), sep = "")
seasons = case_when(
  start_yr == 1999 ~ paste(start_yr, "-", substr(end_yr,1,4), sep = ""),
  start_yr > 2002 ~ as.character(start_yr),
  TRUE ~ seasons_def)

# Second tier (First Division) from 1985-86
seasons_tier_two = seasons[65:length(seasons)]
# temporary code
#seasons_tier_two = seasons[65:104]

#no_teams_finals = c(rep(4,15), rep(6,length(seasons)-15))
no_teams_finals = 10     # temporary value

# Note: need to update this line each year to value of table number in wikipedia for latest season
wiki_table_no_tier_one = c(2, rep(3,5), 2, 3, rep(2,29), rep(1,48), rep(2,6), rep(3,5), rep(4,8))
wiki_table_no_tier_one[which(seasons %in% c("1985-86", "1986-87"))] = 2
wiki_table_no_tier_one[which(seasons == "2012")] = 4

wiki_table_no_tier_two = c(rep(1,21), rep(2,8), rep(3,6), rep(4,4), 3)
wiki_table_no_tier_two[which(seasons_tier_two %in% c("1994-95", "2002-03"))] = 3

wiki_name_tier_one = c(rep("_League_of_Ireland", 64), 
                       rep("_League_of_Ireland_Premier_Division", length(seasons)-64))
wiki_name_tier_two = c(rep("_League_of_Ireland_First_Division", length(seasons_tier_two)))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 
make_graph_ire = function(team_abbrev) {
  data_for_graph = league_ireland_tables %>% 
    filter(abbrev == team_abbrev)
  
  max_teams_in_season = max(data_for_graph$count_teams)
  start_yr = min(data_for_graph$season)
  end_yr = max(data_for_graph$season)
  min_yr = min(data_for_graph$yr_start)
  max_yr = max(data_for_graph$yr_start)
  graph_range = max_yr - min_yr
  league_name = "League of Ireland"
  team_name = data_for_graph$current_name[1]
  colour_main = data_for_graph$team_colours[1]
  line_colour = colour_main
  
  #Breaks for background rectangles, other formatting
  # Update these values whenever the no. of teams in the league changes
  rects = data.frame(xstart = c(-Inf, 1922.5, 1923.5, 1930.5, 1932.5, 1935.5, 1940.5, 1941.5, 1943.5,
                                1948.5, 1951.5, 1962.5, 1963.5, 1969.5, 1977.5, 1982.5, 1984.5,
                                1985.5, 2011.5, 2012.5, 2022.5, 2023.5),
                     xend = c(1922.5, 1923.5, 1930.5, 1932.5, 1935.5, 1940.5, 1941.5, 1943.5,
                              1948.5, 1951.5, 1962.5, 1963.5, 1969.5, 1977.5, 1982.5, 1984.5, 
                              1985.5, 2011.5, 2012.5, 2022.5, 2023.5, Inf),
                     ystart = c(rep(22,22)), 
                     yend = c(8, 12, 10, 12, 10, 12, 11, 10, 8, 10, 12, 10, 12, 14, 16, 14, 16, 22, 21, 20, 19, 20))
  x_intercepts = data_for_graph$yr_start[(data_for_graph$yr_start %% ifelse(graph_range > 60, 10, 5)) == 0]
  x_intercepts = x_intercepts[!(x_intercepts ==max_yr)]
  
  # Graph of overall league position
  graph_1 = ggplot(data_for_graph, aes(x = yr_start, y = overall_pos, group=loi_stint)) +
    geom_line(linewidth=1.15, colour = line_colour) +
#    geom_point(aes(colour=as.factor(champion), size = as.factor(champion))) +
    geom_point(aes(colour=as.factor(cup_winner), size = as.factor(cup_winner))) +
    scale_colour_manual(values = c(data_for_graph$second_colour[1], data_for_graph$champ_colour[1])) +
    scale_size_manual(values = c(2,4)) +
    
    # axes
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = Inf, ymax = yend+0.1),  # 0.1 for margin
              fill = "white", alpha = 1.0, inherit.aes = FALSE) +
    scale_y_continuous(trans = "reverse", expand = c(0,0.1), breaks= pretty_breaks()) +
    scale_x_continuous(breaks= pretty_breaks()) +
    coord_cartesian(xlim = c(min_yr, max_yr), ylim = c(max_teams_in_season, 1)) +
    geom_vline(xintercept=x_intercepts,  linetype="dotted") +
    theme(panel.border = element_rect(fill=NA)) +
    
    # titles
    ggtitle(paste("Position of", team_name, "in", league_name, "from", start_yr, "to", end_yr)) + 
    theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
    labs(x="Year", y="Position") +
    theme(axis.title = element_text(face = "bold")) +
    theme(plot.margin=unit(c(0.5,1,1.5,1.2),"cm")) +
    theme(legend.position = "none") +
    
    # horizontal lines for number of finals teams
    # geom_step doesn't show years when teams were not in highest division
    {if(min_yr<2002)geom_segment(aes(x = min(yr_start), xend = min(max_yr,2001.5), y = 12.5, yend = 12.5), linetype="solid", colour = "black", linewidth = 1)} +
    {if((min_yr<2002)&(max_yr>=2002))geom_segment(aes(x = 2001.5, xend = 2001.5, y = 12.5, yend = 10.5), linetype="solid", colour = "black", linewidth = 1)} +
    {if((max_yr>=2002)&(min_yr<2005))geom_segment(aes(x = 2001.5, xend = 2004.5, y = 10.5, yend = 10.5), linetype="solid", colour = "black", linewidth = 1)} +
    {if((min_yr<2005)&(max_yr>=2005))geom_segment(aes(x = 2004.5, xend = 2004.5, y = 10.5, yend = 12.5), linetype="solid", colour = "black", linewidth = 1)} +
    {if((max_yr>=2005)&(min_yr<2009))geom_segment(aes(x = 2004.5, xend = 2008.5, y = 12.5, yend = 12.5), linetype="solid", colour = "black", linewidth = 1)} +
    {if((min_yr<2009)&(max_yr>=2009))geom_segment(aes(x = 2008.5, xend = 2008.5, y = 12.5, yend = 10.5), linetype="solid", colour = "black", linewidth = 1)} +
    {if((max_yr>=2009)&(min_yr<2012))geom_segment(aes(x = 2008.5, xend = 2011.5, y = 10.5, yend = 10.5), linetype="solid", colour = "black", linewidth = 1)} +
    {if((min_yr<2012)&(max_yr>=2012))geom_segment(aes(x = 2011.5, xend = 2011.5, y = 10.5, yend = 12.5), linetype="solid", colour = "black", linewidth = 1)} +
    {if((max_yr>=2012)&(min_yr<2018))geom_segment(aes(x = 2011.5, xend = 2017.5, y = 12.5, yend = 12.5), linetype="solid", colour = "black", linewidth = 1)} +
    {if((min_yr<2018)&(max_yr>=2018))geom_segment(aes(x = 2017.5, xend = 2017.5, y = 12.5, yend = 10.5), linetype="solid", colour = "black", linewidth = 1)} +
    {if(max_yr>=2018)geom_segment(aes(x = max(2017.5,min_yr), xend = max(yr_start), y = 10.5, yend = 10.5), linetype="solid", colour = "black", linewidth = 1)}

  graph_1
}

make_graph_ire("WAN")
make_graph_ire("BOH")
make_graph_ire("ROV")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in external data
# read all league tables in one loop
# to read a league table manually, see code at bottom, e.g. read_html("https://en.wikipedia.org/wiki/1959-60_League_of_Ireland")
tables = list()

for (j in 1:length(seasons)) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons[j], wiki_name_tier_one[j], sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j]] <- tables_wiki[[wiki_table_no_tier_one[j]]]  %>% # added to my list
    mutate(season_no = j, season = seasons[j], tier = 1)
  
  if (j%%5==0) print(paste("season = ", seasons[j], sep="")) 
}

# Repeat for 2nd tier (First Division from 1985)
for (j in (1:length(seasons_tier_two))) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons_tier_two[j], wiki_name_tier_two[j], sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j + length(seasons)]] <- tables_wiki[[wiki_table_no_tier_two[j]]]  %>% # added to my list
    mutate(season_no = j + + length(seasons), season = seasons_tier_two[j], tier = 2)
  
  if (j%%5==0) print(paste("season tier 2 = ", seasons_tier_two[j], sep="")) 
}


# Review headers in each of the tables - need consistency of names for combining tables
headers_all = c()
for (j in 1) {
  header_fmt1 = colnames(tables[[j]])
  headers_all = rbind(header_fmt1, headers_all)
}
for (j in 36) {
  header_fmt2 = colnames(tables[[j]])
  headers_all = rbind(header_fmt2, headers_all)
}

header_fmt1 = colnames(tables[[1]])
header_fmt2 = colnames(tables[[36]])

for (j in c(1:35, 45:46)) {
  colnames(tables[[j]]) = header_fmt1              # original format
}
for (j in c(36:44, 47:(length(seasons)+length(seasons_tier_two)))) {
  colnames(tables[[j]]) = header_fmt2              # latest format
}

# convert from list to data frame
tables_all_fmt1 = do.call(rbind, lapply(tables[c(1:35, 45:46)], as.data.frame))
tables_all_fmt2 = do.call(rbind, lapply(tables[c(36:44, 47:(length(seasons)+length(seasons_tier_two)))], as.data.frame))

tables_all_fmt1_adj = tables_all_fmt1 %>%
  mutate(Qualification = "None") %>%
  select(Pos:Pts, Qualification, season_no:tier)

tables_all_fmt2_adj = tables_all_fmt2 

tables_all = rbind(tables_all_fmt1_adj, tables_all_fmt2_adj) %>%
  arrange(tier, season_no, Pos) %>%
  filter(!(is.na(Pld)))
tables_all[1013,3:10] = 0  # 2006 Dublin City - results expunged

# Temporary code - Add in tier, if only appending tables.
#tables_all = tables_all %>%
#  mutate(tier = ifelse(season_no <= length(seasons), 1, 2))

# read in cup winners
table_cup = read_html("https://en.wikipedia.org/wiki/FAI_Cup") %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)


table_other = table_cup[[2]] %>%
  select(c(Season:Winner, "Runner-up")) %>%
  mutate(Season = str_replace(Season, "2002 \\(Interim\\)", "2002-03"),
         Season = str_replace(Season, "-", "-")) %>%
  rename("Runners_up" = "Runner-up")
table_other$Winner = ifelse(table_other$Winner == "St James's Gate", "St. James's Gate", table_other$Winner)
table_other$Winner = ifelse(table_other$Winner == "Limerick United", "Limerick", table_other$Winner)
table_other$Winner = ifelse(table_other$Winner == "UCD", "University College Dublin", table_other$Winner)

table_other$Runners_up = ifelse(table_other$Runners_up == "St James's Gate", "St. James's Gate", table_other$Runners_up)
table_other$Runners_up = ifelse(table_other$Runners_up == "Evergreen United", "Cork Celtic", table_other$Runners_up)
table_other$Runners_up = ifelse(table_other$Runners_up == "Drogheda", "Drogheda United", table_other$Runners_up)
table_other$Runners_up = ifelse(table_other$Runners_up == "Waterford United", "Waterford", table_other$Runners_up)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files
setwd(input_path)
league_ireland_teams = read_csv("league_ireland_teams.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations
league_ireland_tables = tables_all %>% 
  mutate(Team = str_replace(Team, "\\[.*\\]", ""),            # remove text inside square brackets
         Pos = case_when(
           season == "1992-93" & Team == "Cork City" ~ 1,
           season == "1992-93" & Team == "Bohemians" ~ 2,
           season == "1992-93" & Team == "Shelbourne" ~ 3,
           TRUE ~ Pos),
         champion = ifelse(Pos == 1 & tier == 1, 1, 0),
         runners_up = ifelse(Pos == 2 & tier == 1, 1, 0),
         relegated = ifelse(substr(Team, nchar(Team) - 2, nchar(Team)) == "(R)", 1, 0),
         Team = str_replace(Team, " \\(C\\)", ""),            # to get consistency in team name
         Team = str_replace(Team, " \\(R\\)", ""),  
         Team = str_replace(Team, "\\(R\\)", ""),  
         Team = str_replace(Team, " \\(C, R\\)", ""),
         Team = str_replace(Team, " \\(C, P\\)", ""),
         Team = str_replace(Team, " \\(O\\)", ""), 
         Team = str_replace(Team, "\\(P\\)", ""),
         Team = str_replace(Team, " \\(Q\\)", ""),
         Team = str_replace(Team, " \\(O, P\\)", ""),
         Team = str_replace(Team, " A.F.C.", ""),
         Team = str_replace(Team, " F.C.", ""),
         Pts = as.numeric(str_replace(Pts, "\\[.*\\]", "")),
         yr_start = as.numeric(substr(season, 1, 4)),
         yr_end = yr_start + 1,
         pts_per_win = case_when(
           yr_end <= 1981 ~ 2,
           yr_end == 1982 ~ 4,
           yr_end == 1983 ~ 3,
           yr_end >= 1984 & yr_end <= 1993 ~ 2,
           TRUE ~ 3),
         pts_per_draw = 1,
         pts_deducted = Pts - (pts_per_win * W + pts_per_draw * D),
         max_avail_pts = Pld * pts_per_win,
         pts_achieved_perc = Pts / max_avail_pts,
         home_away_pts = ifelse(season == "1981-82", Qualification, ""),
         div_split = ifelse(Pos > 6 & season %in% c("1992-93", "1993-94") & tier == 1, 2, 1),
         goal_diff = GF - GA,
         GD_prefix = substr(GD,1,1),
         GD_sign = case_when(
           GD_prefix == "+" ~ 1,
           GD_prefix == "-" ~ -1,
           GD_prefix == "0" ~ 1,
           TRUE ~ -1),
         GD_numeric = ifelse(GD_prefix == "0", 0, as.numeric(substr(GD,2,nchar(GD)))) * GD_sign,
         GD_check = GD_numeric - goal_diff, 
         goal_ratio = GF / GA,
         goal_ratio_for_tiebreak = ifelse(season_no <= 53, goal_ratio, 1),   # goal ratio up to 1973-74, goal diff thereafter
         goals_per_game = round(GF / Pld, 2),
         no_teams_finals_chart = no_teams_finals,
         tier_one = ifelse(tier == 1, 1, 0),
         tier_two = ifelse(tier == 2, 1, 0),
         Team = case_when(                      # Overwrite team names where it is based on year
           Team == "Cork City" & yr_end < 1940 ~ "Cork City 1938",
           Team == "Cork United" & yr_end > 1978 ~ "Cork Alberts",
           TRUE ~ Team)) %>%
  separate(home_away_pts, into = c("AW", "HW", "AD", "HD"), sep = ";") %>%
  group_by(season, tier) %>%
  mutate(count_teams_tier = n()) %>%
  ungroup() %>%
  group_by(season) %>%
  mutate(count_teams = n()) %>%
  ungroup() %>%
  mutate(wooden_spoon = ifelse(Pos == count_teams_tier, 1, 0),
         count_teams_tier_one = count_teams - count_teams_tier,
         overall_pos = Pos + ifelse(tier == 1, 0, count_teams_tier_one)) %>%
  select(Pos:runners_up, count_teams_tier:overall_pos, yr_start:tier_two, AW:HD) %>%
  arrange(season_no, Pos) %>%
  mutate(Team = trimws(Team))

# Create a table of team names, including history & past team name changes
teams = as_tibble(unique(league_ireland_tables$Team))
colnames(teams) = c("previous_name")
teams = teams %>% 
  mutate(current_name = previous_name)
teams$current_name = ifelse(teams$previous_name == "Dundalk GNR", "Dundalk", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "St James's Gate", "St. James's Gate", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Fordsons", "Cork", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Evergreen United", "Cork Celtic", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Drogheda", "Drogheda United", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Bohemian", "Bohemians", teams$current_name)
teams$current_name = ifelse(teams$previous_name %in% c("Home Farm Drumcondra", "Home Farm Everton"), "Home Farm", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Galway Rovers", "Galway United", teams$current_name)
teams$current_name = ifelse(teams$previous_name %in% c("Albert Rovers", "Cork Albert"), "Cork Alberts", teams$current_name)
teams$current_name = ifelse(teams$previous_name %in% c("Limerick United", "Limerick City", "Limerick 37"), "Limerick", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Waterford United", "Waterford", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "UCD", "University College Dublin", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Wexford Youths", "Wexford", teams$current_name)
teams$current_name = ifelse(teams$previous_name %in% c("Newcastle United", "Newcastlewest"), "Newcastle West", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "E.M.F.A.", "Kilkenny City", teams$current_name)
teams$current_name = ifelse(teams$previous_name %in% c("St Francis", "Fingal-St. Francis"), "St. Francis", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Home Farm Fingal", "Dublin City", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Cork City FORAS Co-op", "Cork City", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "SD Galway", "Salthill Devon", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Galway", "Galway United", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Cobh Ramblers", "Cobh Ramblers", teams$current_name)
teams$current_name = ifelse(teams$previous_name == "Shamrock Rovers II", "Shamrock Rovers B", teams$current_name)

teams_all = left_join(teams, league_ireland_teams, by = c("current_name" = "current_name"))

league_ireland_tables_all = left_join(league_ireland_tables, teams_all, by = c("Team" = "previous_name")) %>%
  # Join with other data
  left_join(table_other, by = c("season" = "Season")) %>%
  mutate(cup_winner = ifelse(current_name == Winner, 1, 0),
         cup_runner_up = ifelse(current_name == Runners_up, 1, 0))

# Add additional information of previous season's finishing position (using overall position)
league_ireland_tables = league_ireland_tables_all %>%
  arrange(current_name, yr_end) %>%   
  mutate(prev_pos = ifelse(current_name == lag(current_name), lag(overall_pos), NA)) %>%
  mutate(next_pos = ifelse(current_name == lead(current_name), lead(overall_pos), NA)) %>%
  arrange(current_name, yr_end) %>%  
  mutate(prev_tier = ifelse(current_name == lag(current_name), lag(tier), NA)) %>%
  arrange(season_no, overall_pos) %>%  
  mutate(pos_diff = ifelse(is.na(prev_pos), NA, -(overall_pos - prev_pos)),
         pos_abs_diff = abs(pos_diff),
         tier_diff = ifelse(is.na(prev_tier), NA, -(tier - prev_tier)),   # -1 => relegated & 1 => promoted in previous year
         promoted = ifelse(tier_diff == 1, 1, 0),
         relegated = ifelse(tier_diff == -1, 1, 0),
         # Need team abbrev, year of relegation for teams that returned later
         releg_yr_1 = case_when(
           abbrev == "ATH" ~ 1928,
           abbrev == "BRI" ~ 1932,
           abbrev == "COB" ~ 2009,
           abbrev == "GAL" ~ 2012,
           abbrev == "ROVB" ~ 2015,
           abbrev == "SHEL" ~ 1934,
           abbrev == "SLI" ~ 1940,
           abbrev == "STJAM" ~ 1944,
           abbrev == "WAT" ~ 1932,
           TRUE ~ 2099),
         releg_yr_2 = case_when(
           abbrev == "BRI" ~ 1943,
           abbrev == "SLI" ~ 1962,
           abbrev == "STJAM" ~ 1996,
           abbrev == "WAT" ~ 1941,
           TRUE ~ 2099),
         loi_stint = case_when(
           yr_end > releg_yr_2 ~ 3,
           yr_end > releg_yr_1 ~ 2,
           TRUE ~ 1)) %>%
  group_by(current_name) %>%
  mutate(cum_champions = cumsum(champion),
         streak_champion = c(ave(c(0, champion), cumsum(c(0, champion) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_champion = c(ave(c(0, champion), cumsum(c(0, champion) > 0), FUN = seq_along) - 1)[-1],
         cum_runners_up = cumsum(runners_up),
         streak_runners_up = c(ave(c(0, runners_up), cumsum(c(0, runners_up) == 0), FUN = seq_along) - 1)[-1],
         cum_tier_one = cumsum(tier_one),
         streak_tier_one = c(ave(c(0, tier_one), cumsum(c(0, tier_one) == 0), FUN = seq_along) - 1)[-1],
         cum_tier_two = cumsum(tier_two),
         streak_tier_two = c(ave(c(0, tier_two), cumsum(c(0, tier_two) == 0), FUN = seq_along) - 1)[-1]) %>%
  ungroup() %>%
  mutate(row_number = row_number())


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis of League of Ireland tables data
# Make all-time league table
league_ireland_all_time_league_table = group_by(league_ireland_tables, current_name) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_Ded = sum(pts_deducted),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts),
            pts_per_game = round(sum(Pts) / sum(Pld), 2),
            win_perc = round(Total_W / Total_Pld * 100, 2),
            count_champions = sum(champion),
            count_runners_up = sum(runners_up),
            best_overall = min(overall_pos),
            count_promoted = sum(promoted, na.rm = TRUE),
            count_spoon = sum(wooden_spoon),
            count_relegated = sum(relegated, na.rm = TRUE),
            count_cup_winners = sum(cup_winner),
            count_cup_runners_up = sum(cup_runner_up),
            first_season = min(season),
            last_season = max(season)) %>%
  arrange(desc(Total_Pts), desc(Total_W), desc(Total_GD), desc(Total_GF))

league_ireland_all_time_league_table_by_tier = group_by(league_ireland_tables, current_name, tier) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_Ded = sum(pts_deducted),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts),
            pts_per_game = round(sum(Pts) / sum(Pld), 2),
            win_perc = round(Total_W / Total_Pld * 100, 2),
            count_champions = sum(champion),
            count_runners_up = sum(runners_up),
            count_1st = sum(Pos == 1),
            count_2nd = sum(Pos == 2),
            count_3rd = sum(Pos == 3),
            count_4th = sum(Pos == 4),
            count_5th = sum(Pos == 5),
            count_6th = sum(Pos == 6),
            best = min(Pos),
            count_promoted = sum(promoted, na.rm = TRUE),
            count_spoon = sum(wooden_spoon),
            count_relegated = sum(relegated, na.rm = TRUE),
            count_cup_winners = sum(cup_winner),
            count_cup_runners_up = sum(cup_runner_up),
            first_season = min(season),
            last_season = max(season)) %>%
  arrange(tier, desc(Total_Pts), desc(Total_W), desc(Total_GD), desc(Total_GF))

# totals by season
season_totals_tier = group_by(league_ireland_tables, season, yr_end, tier) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts),
            max_ave_goals_scored_team = max(goals_per_game, na.rm = TRUE),
            min_ave_goals_scored_team = min(goals_per_game, na.rm = TRUE)) %>%
  mutate(ave_goals_scored_game = round(Total_GF / (0.5 * Total_Pld), 1))

season_totals = group_by(league_ireland_tables, season, yr_end) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts),
            count_cup_winners = sum(cup_winner),
            count_cup_runners_up = sum(cup_runner_up),
            max_ave_goals_scored_team = max(goals_per_game, na.rm = TRUE),
            min_ave_goals_scored_team = min(goals_per_game, na.rm = TRUE)) %>%
  mutate(ave_goals_scored_game = round(Total_GF / (0.5 * Total_Pld), 1))

title_race_totals = group_by(league_ireland_tables, season, yr_end, tier) %>%
  summarise(count = n(),
            Total_Pts_1 = sum(Pts[Pos == 1]),
            Total_Pts_2 = sum(Pts[Pos == 2]),
            Total_GD_1 = sum(goal_diff[Pos == 1]),
            Total_GD_2 = sum(goal_diff[Pos == 2]),
            Total_GF_1 = sum(GF[Pos == 1]),
            Total_GF_2 = sum(GF[Pos == 2])) %>%
  mutate(margin_pts = Total_Pts_1 - Total_Pts_2,
         margin_GD = Total_GD_1 - Total_GD_2,
         margin_GF = Total_GF_1 - Total_GF_2)

# totals by club
club_records = group_by(league_ireland_tables, current_name) %>%
  summarise(highest_GF = max(GF),
            lowest_GF = min(GF),
            highest_GA = max(GA),
            lowest_GA = min(GA),
            highest_Pts = max(Pts),
            lowest_Pts = min(Pts))

team_streaks = group_by(league_ireland_tables, current_name) %>%
  summarise(count = n(),
            max_streak_champion = max(streak_champion),
            max_streak_missed_champion = max(streak_missed_champion),
            max_streak_runners_up = max(streak_runners_up),
            max_streak_tier_one = max(streak_tier_one),
            max_streak_tier_two = max(streak_tier_two)) %>%
  arrange(current_name)

# Records for each team in a season
highest_GF_team = club_records %>%
  left_join(league_ireland_tables, by = c("current_name" = "current_name",
                                         "highest_GF" = "GF")) %>%
  select(current_name, highest_GF, Pld, season, tier)

lowest_GF_team = club_records %>%
  left_join(league_ireland_tables, by = c("current_name" = "current_name",
                                         "lowest_GF" = "GF")) %>%
  select(current_name, lowest_GF, Pld, season, tier)

highest_GA_team = club_records %>%
  left_join(league_ireland_tables, by = c("current_name" = "current_name",
                                         "highest_GA" = "GA")) %>%
  select(current_name, highest_GA, Pld, season, tier)

lowest_GA_team = club_records %>%
  left_join(league_ireland_tables, by = c("current_name" = "current_name",
                                         "lowest_GA" = "GA")) %>%
  select(current_name, lowest_GA, Pld, season, tier)

highest_Pts_team = club_records %>%
  left_join(league_ireland_tables, by = c("current_name" = "current_name",
                                         "highest_Pts" = "Pts")) %>%
  select(current_name, highest_Pts, Pld, season, tier)

lowest_Pts_team = club_records %>%
  left_join(league_ireland_tables, by = c("current_name" = "current_name",
                                         "lowest_Pts" = "Pts")) %>%
  select(current_name, lowest_Pts, Pld, season, tier)

# Records for a single season - not adjusted for no. of games
# most & least points
most_pts_season = arrange(league_ireland_tables, desc(Pts)) %>%
  select(season, tier, Team, Pld, Pts)
head(most_pts_season, 5)

least_pts_season = arrange(league_ireland_tables, Pts) %>%
  filter(Pld > 0) %>%
  select(season, tier, Team, Pld, Pts)
head(least_pts_season, 5)

# most & least wins
most_wins_season = arrange(league_ireland_tables, desc(W)) %>%
  select(season, tier, Team, Pld, W)
head(most_wins_season, 5)

least_wins_season = arrange(league_ireland_tables, W) %>%
  filter(Pld > 0) %>%
  select(season, tier, Team, Pld, W) %>%
  filter(W == 0)
least_wins_season

# most & least losses
most_losses_season = arrange(league_ireland_tables, desc(L)) %>%
  select(season, tier, Team, Pld, L)
head(most_losses_season, 5)

least_losses_season = arrange(league_ireland_tables, L) %>%
  filter(Pld > 0) %>%
  select(season, tier, Team, Pld, L)
head(least_losses_season, 5)

# most & least draws
most_draws_season = arrange(league_ireland_tables, desc(D)) %>%
  select(season, tier, Team, Pld, D)
head(most_draws_season, 5)

least_draws_season = arrange(league_ireland_tables, D) %>%
  filter(Pld > 0) %>%
  select(season, tier, Team, Pld, D)
head(least_draws_season, 5)

# most & least goals scored
most_goals_season = arrange(league_ireland_tables, desc(GF)) %>%
  select(season, tier, Team, Pld, GF)
head(most_goals_season, 5)

least_goals_season = arrange(league_ireland_tables, GF) %>%
  filter(Pld > 0) %>%
  select(season, tier, Team, Pld, GF)
head(least_goals_season, 5)

# most & least goals conceded
most_goals_against_season = arrange(league_ireland_tables, desc(GA)) %>%
  select(season, tier, Team, Pld, GA)
head(most_goals_against_season, 5)

least_goals_against_season = arrange(league_ireland_tables, GA) %>%
  filter(Pld > 0) %>%
  select(season, tier, Team, Pld, GA)
head(least_goals_against_season, 5)

# best & worst goal difference
best_goals_diff_season = arrange(league_ireland_tables, desc(goal_diff)) %>%
  select(season, tier, Team, Pld, goal_diff)
head(best_goals_diff_season, 5)

worst_goals_diff_season = arrange(league_ireland_tables, goal_diff) %>%
  select(season, tier, Team, Pld, goal_diff)
head(worst_goals_diff_season, 5)

most_promotions = arrange(league_ireland_all_time_league_table, desc(count_promoted)) %>%
  select(current_name, count_promoted)
head(most_promotions, 5)

most_relegations = arrange(league_ireland_all_time_league_table, desc(count_relegated)) %>%
  select(current_name, count_relegated)
head(most_relegations, 5)

# highest & lowest points achieved percentage
highest_pts_perc_season = arrange(league_ireland_tables, desc(pts_achieved_perc)) %>%
  select(season, tier, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(highest_pts_perc_season, 5)

lowest_pts_perc_season = arrange(league_ireland_tables, pts_achieved_perc) %>%
  select(season, tier, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(lowest_pts_perc_season, 5)

# most points to not win the league
most_pts_not_champions_season = arrange(league_ireland_tables, desc(Pts)) %>%
  filter(!(Pos == 1)) %>%
  select(season, tier, Team, Pld, Pts) 
head(most_pts_not_champions_season, 5)

# least points to win the league
least_pts_champions_season = arrange(league_ireland_tables, Pts) %>%
  filter(Pos == 1) %>%
  select(season, tier, Team, Pld, Pts)
head(least_pts_champions_season, 5)

# biggest & smallest winning margin in league
most_winning_margin_season = title_race_totals %>%
  arrange(desc(margin_pts), desc(margin_GD), desc(margin_GF)) %>%
  left_join(league_ireland_tables, by = c("season" = "season", "tier" = "tier")) %>%
  filter(Pos == 1) %>%
  select(season, tier, Team, margin_pts, margin_GD, margin_GF)
head(most_winning_margin_season, 5)

least_winning_margin_season = title_race_totals %>%
  arrange(margin_pts, margin_GD, margin_GF) %>%
  left_join(league_ireland_tables, by = c("season" = "season", "tier" = "tier")) %>%
  filter(Pos == 1) %>%
  select(season, tier, Team, margin_pts, margin_GD, margin_GF)
head(least_winning_margin_season, 5)

# highest movement in final position
highest_mvmt_up_season = arrange(league_ireland_tables, desc(pos_diff)) %>%
  select(season, Team, Pos, tier, prev_pos, prev_tier, pos_diff)
head(highest_mvmt_up_season, 5)

highest_mvmt_down_season = arrange(league_ireland_tables, pos_diff) %>%
  select(season, Team, Pos, tier, prev_pos, prev_tier, pos_diff)
head(highest_mvmt_down_season, 5)

# lowest position to champion in one season
prev_pos_champion = league_ireland_tables %>%
  filter(champion == 1) %>%
  select(season, Team, prev_pos) %>%
  arrange(desc(prev_pos), season)
head(prev_pos_champion, 5)

# lowest position after being champion in one season
next_pos_champion = league_ireland_tables %>%
  filter(champion == 1) %>%
  select(season, Team, next_pos) %>%
  arrange(desc(next_pos), season)
head(next_pos_champion, 5)


# volatility of position from year to year
pos_changes = league_ireland_tables %>%
  group_by(current_name) %>%
  summarise(count_seasons = n(),
            total_pos_diff = sum(pos_abs_diff, na.rm = TRUE)) %>%
  mutate(ave_mvmt = total_pos_diff / (count_seasons - 1)) %>%
  arrange(desc(ave_mvmt))
pos_changes


# Longest streaks
longest_streaks_champion = arrange(league_ireland_tables, desc(streak_champion)) %>%
  select(season, Team, streak_champion)
head(longest_streaks_champion, 5)

longest_streaks_missed_champion = arrange(league_ireland_tables, desc(streak_missed_champion)) %>%
  select(season, Team, streak_missed_champion)
head(longest_streaks_missed_champion, 5)

longest_streaks_runners_up = arrange(league_ireland_tables, desc(streak_runners_up)) %>%
  select(season, Team, streak_runners_up)
head(longest_streaks_runners_up, 5)

longest_streaks_tier_one = arrange(league_ireland_tables, desc(streak_tier_one)) %>%
  select(season, Team, streak_tier_one)
head(longest_streaks_tier_one, 5)

longest_streaks_tier_two = arrange(league_ireland_tables, desc(streak_tier_two)) %>%
  select(season, Team, streak_tier_two)
head(longest_streaks_tier_two, 5)

# CUp winners tier 2
cup_winners_tier_two = league_ireland_tables %>%
  filter(cup_winner == 1, tier == 2) %>%
  select(Team, season, tier, Pos)

# list of all team abbreviations
teams_unique = unique(league_ireland_tables$abbrev)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# checks on data for consistency
error_check_pts = league_ireland_tables %>% 
  filter(!Pts == (pts_per_win * W + pts_per_draw * D)) %>%
  filter(!(season == "1925-26" & abbrev %in% c("BOH", "CORK") & abs(pts_deducted) == 2)) %>%
  filter(!(season == "1941-42" & abbrev %in% c("STJAM", "LIM") & abs(pts_deducted) == 2)) %>%
  filter(!(season == "1956-57" & abbrev == "SHEL" & pts_deducted == -2)) %>%
  filter(!(season == "1961-62" & abbrev %in% c("CORKHIB", "LIM") & abs(pts_deducted) == 1)) %>%
  filter(!(season == "1971-72" & abbrev %in% c("ATH", "SHEL") & abs(pts_deducted) == 2)) %>%
  filter(!(season == "1972-73" & abbrev %in% c("LIM", "SLI") & abs(pts_deducted) == 2)) %>%
  filter(!(season == "1978-79" & abbrev == "LIM" & pts_deducted == 1)) %>%
  filter(!(season == "1978-79" & abbrev %in% c("DRO", "FIN", "THU") & abs(pts_deducted) == 2)) %>%
  filter(!(season == "1978-79" & abbrev == "CORKCEL" & pts_deducted == -3)) %>%
  filter(!(season == "1981-82")) %>%  # experimental season pts away win 4, home win 3, away draw 2, home draw 1
  filter(!(season == "1991-92" & abbrev == "DRO" & pts_deducted == -1)) %>%
  filter(!(season == "1995-96" & abbrev == "CORKCIT" & pts_deducted == -3)) %>%
  filter(!(season == "2001-02" & abbrev == "STPAT" & pts_deducted == -15)) %>%
  filter(!(season == "2001-02" & abbrev == "WAT" & pts_deducted == -3)) %>%
  filter(!(season == "2003" & abbrev == "MON" & pts_deducted == -3)) %>%
  filter(!(season == "2004" & abbrev %in% c("ATH", "MON") & pts_deducted == -3)) %>%
  filter(!(season == "2005" & abbrev == "ROV" & pts_deducted == -8)) %>%
  filter(!(season == "2006" & abbrev %in% c("BOH", "ROV") & pts_deducted == -3)) %>%
  filter(!(season == "2007" & abbrev == "LON" & pts_deducted == -6)) %>%
  filter(!(season == "2008" & abbrev %in% c("CORKCIT", "DRO") & pts_deducted == -10)) %>%
  filter(!(season == "2019" & abbrev == "LIM" & pts_deducted == -26)) 
  
error_check_pld = league_ireland_tables %>%
  filter(!Pld == (W + D + L))

error_check_results = season_totals_tier %>%
  filter(!Total_W == Total_L)

error_check_gd_season = season_totals_tier %>%
  filter(!Total_GD == 0)

error_check_gd = league_ireland_tables %>%
  filter(!(GD_check == 0))

error_check_pos = group_by(league_ireland_tables, season, tier) %>%
  summarise(count = n(),
            sum_pos = sum(Pos)) %>%
  mutate(exp_sum_pos = count * (count + 1) / 2,
         pos_diff = sum_pos - exp_sum_pos) %>%   # error if calculated difference (pos_diff) is not zero
  filter(!(pos_diff == 0))

error_sorted_pos = league_ireland_tables %>%
  arrange(season_no, div_split, desc(Pts), desc(goal_ratio_for_tiebreak), desc(goal_diff), desc(GF)) %>%
  mutate(sorted_row_number = row_number(),
         row_no_diff = row_number - sorted_row_number) %>%
  filter(!(row_no_diff == 0)) %>%
  filter(!(season == "1961-62" & Pos <= 2)) %>%  # decided by play-off final
  filter(!(season == "1992-93" & tier == 1 & Pos <= 3))      # decided by play-off series

check_identical_pos = league_ireland_tables %>%
  group_by(season, tier, div_split, Pts, goal_diff, GF) %>%
  summarise(count_seasons = n()) %>%
  filter(count_seasons > 1)

check_cup_winners = season_totals %>%
  filter(count_cup_winners == 0) %>%
  left_join(table_other, by = c("season" = "Season")) %>%
  select(season, Winner)

check_cup_runner_up = season_totals %>%
  filter(count_cup_runners_up == 0) %>%
  left_join(table_other, by = c("season" = "Season")) %>%
  select(season, Runners_up)

check_team_abbrev = league_ireland_tables %>%
  filter(is.na(abbrev))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# run function to produce graph for a specific team
make_graph_ire("ATH")      # Athlone Town
make_graph_ire("BOH")      # Bohemians
make_graph_ire("UNK")      # Bray Unknowns
make_graph_ire("WAN")      # Bray Wanderers
make_graph_ire("BRI")      # Brideville - should see value for 1935
make_graph_ire("BRO")      # Brooklyn
make_graph_ire("CAB")      # Cabinteely
make_graph_ire("COB")      # Cobh Ramblers
make_graph_ire("CORK")     # Cork
make_graph_ire("CORKALB")  # Cork Alberts
make_graph_ire("CORKATH")  # Cork Athletic
make_graph_ire("CORKBOH")  # Cork Bohemians
make_graph_ire("CORKCEL")  # Cork Celtic
make_graph_ire("CORKCIT")  # Cork City
make_graph_ire("CORK38")   # Cork City 1938
make_graph_ire("CORKHIB")  # Cork Hibernians
make_graph_ire("CORKUTD")  # Cork United
make_graph_ire("DER")      # Derry City
make_graph_ire("DOL")      # Dolphin
make_graph_ire("DRO")      # Drogheda United
make_graph_ire("DRU")      # Drumcondra
make_graph_ire("DUBCIT")   # Dublin City
make_graph_ire("DUBUTD")   # Dublin United
make_graph_ire("DUN")      # Dundalk
make_graph_ire("FIN")      # Finn Harps
make_graph_ire("FRA")      # Frankfort
make_graph_ire("GAL")      # Galway United
make_graph_ire("HOM")      # Home Farm
make_graph_ire("JAC")      # Jacobs
make_graph_ire("KER")      # Kerry
make_graph_ire("KILD")     # Kildare County
make_graph_ire("KILK")     # Kilkenny City
make_graph_ire("LIM")      # Limerick
make_graph_ire("LON")      # Longford Town
make_graph_ire("MER")      # Mervue United
make_graph_ire("MID")      # Midland Athletic
make_graph_ire("MON")      # Monaghan United
make_graph_ire("NEW")      # Newcastlewest
make_graph_ire("OLY")      # Olympia
make_graph_ire("PIO")      # Pioneers
make_graph_ire("RAT")      # Rathmines Athletic
make_graph_ire("RED")      # Reds United
make_graph_ire("SAL")      # Salthill Devon
make_graph_ire("ROV")      # Shamrock Rovers
make_graph_ire("ROVB")     # Shamrock Rovers B
make_graph_ire("SHEL")     # Shelbourne
make_graph_ire("SHELUTD")  # Shelbourne United
make_graph_ire("SLI")      # Sligo Rovers
make_graph_ire("SPO")      # Sporting Fingal
make_graph_ire("STPAT")    # St Patrick's Athletic
make_graph_ire("STFRA")    # St. Francis
make_graph_ire("STJAM")    # St. James's Gate
make_graph_ire("THU")      # Thurles Town
make_graph_ire("TRA")      # Transport
make_graph_ire("TRE")      # Treaty United
make_graph_ire("UCD")      # University College Dublin
make_graph_ire("WAT")      # Waterford
make_graph_ire("WEX")      # Wexford
make_graph_ire("YMC")      # YMCA


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export file to csv format
names(league_ireland_all_time_league_table) <- gsub(x = names(league_ireland_all_time_league_table), pattern = "_", replacement = " ") 
names(league_ireland_all_time_league_table_by_tier) <- gsub(x = names(league_ireland_all_time_league_table_by_tier), pattern = "_", replacement = " ") 

setwd(output_path)
save(tables, file = "league_ireland_tables_raw.Rdata")
save(league_ireland_tables, file = "league_ireland_tables.Rdata")
write.csv(league_ireland_tables, file = "league_ireland_tables_full.csv")
write.csv(league_ireland_all_time_league_table, file = "league_ireland_all_time_league_table.csv")
write.csv(league_ireland_all_time_league_table_by_tier, file = "league_ireland_all_time_league_table_by_tier.csv")
write.csv(season_totals, file = "league_ireland_season_totals.csv")
setwd(path) 

# export single graph
#setwd(output_path)
#ggsave("graph_ggsave.pdf")
#setwd(path)

# export multiple graphs
for (i in 1:length(teams_unique)) {
  make_graph_ire(teams_unique[i])
  setwd(output_path)
  #  ggsave(paste("graph_alm_", teams_unique[i], ".pdf", sep=""))
  ggsave(paste("performance_chart_alm_", teams_unique[i], ".png", sep=""))
  ggsave(paste("performance_chart_alm_", teams_unique[i], ".svg", sep=""))
}
setwd(path)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End


# To do:



# Graph
# add in yr_for_xaxis to data later - base off NZ code
# start line when First division commenced in 1985-86
# remove gold for champions
# cutoff_year - from NZ code; for teams with split graphs



# Future:
# Remove circular definition of league_ireland_tables
# Known issue - names of cup winners & runners-up for teams which were not in the league at the time


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Test
tab_no = 3
#table = read_html("https://en.wikipedia.org/wiki/1974-75_League_of_Ireland")
#table = read_html("https://en.wikipedia.org/wiki/2024_League_of_Ireland_Premier_Division")
table = read_html("https://en.wikipedia.org/wiki/2002-03_League_of_Ireland_First_Division")
tables_all_test <- table %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)
table_yyyymm = tables_all_test[[tab_no]]
table_yyyymm



