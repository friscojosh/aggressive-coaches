library(tidyverse)

# I'm going to yse Ben Baldwin's open source go for it model to do this analysis.
# the model itself is xgboost based and is defined in R/_got_for_it_model.R

source('R/helpers.R')
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
source('R/season_numbers_functions.R')

# this is computationally intensive! We're talking 10 minutes each
# with 16 cores pegged at 100%. I saved the results as .csv.

# go_for_it_2019 <- get_season(2019) %>%  mutate(season = 2019)
# go_for_it_2018 <- get_season(2018) %>%  mutate(season = 2018)
# go_for_it_2017 <- get_season(2017) %>%  mutate(season = 2017)
# go_for_it_2016 <- get_season(2016) %>%  mutate(season = 2016)
# go_for_it_2015 <- get_season(2015) %>%  mutate(season = 2015)
# go_for_it_2014 <- get_season(2014) %>%  mutate(season = 2014)
# go_for_it_2013 <- get_season(2013) %>%  mutate(season = 2013)
# go_for_it_2012 <- get_season(2012) %>%  mutate(season = 2012)
# go_for_it_2011 <- get_season(2011) %>%  mutate(season = 2011)
# go_for_it_2010 <- get_season(2010) %>%  mutate(season = 2010)

# write each to csv

write_to_csv <- function(dataframe) {
  name <- deparse(substitute(dataframe))
  write_csv(dataframe, paste0(name, ".csv"))
}

# write_to_csv(go_for_it_2019)
# write_to_csv(go_for_it_2018)
# write_to_csv(go_for_it_2017)
# write_to_csv(go_for_it_2016)
# write_to_csv(go_for_it_2015)
# write_to_csv(go_for_it_2014)
# write_to_csv(go_for_it_2013)
# write_to_csv(go_for_it_2012)
# write_to_csv(go_for_it_2011)
# write_to_csv(go_for_it_2010)

# we'll keep this one unstored as a csv since the data may change.

go_for_it_2020 <- get_current_season(2020) %>%
  mutate(season = 2020) %>%
  dplyr::select(-play_id, -url)

# this will speed things up for fact check.

go_for_it_2019 <- read_csv("go_for_it_2019.csv") %>%  mutate(season = 2019) 
go_for_it_2018 <- read_csv("go_for_it_2018.csv") %>%  mutate(season = 2018) 
go_for_it_2017 <- read_csv("go_for_it_2017.csv") %>%  mutate(season = 2017) 
go_for_it_2016 <- read_csv("go_for_it_2016.csv") %>%  mutate(season = 2016) 
go_for_it_2015 <- read_csv("go_for_it_2015.csv") %>%  mutate(season = 2015) 
go_for_it_2014 <- read_csv("go_for_it_2014.csv") %>%  mutate(season = 2014)
go_for_it_2013 <- read_csv("go_for_it_2013.csv") %>%  mutate(season = 2013) 
go_for_it_2012 <- read_csv("go_for_it_2012.csv") %>%  mutate(season = 2012)
go_for_it_2011 <- read_csv("go_for_it_2011.csv") %>%  mutate(season = 2011)
go_for_it_2010 <- read_csv("go_for_it_2010.csv") %>%  mutate(season = 2010) 

go_for_it_all <- go_for_it_2020 %>% bind_rows(go_for_it_2019, go_for_it_2018,
                                              go_for_it_2017, go_for_it_2016,
                                              go_for_it_2015, go_for_it_2014,
                                              go_for_it_2013, go_for_it_2012,
                                              go_for_it_2011, go_for_it_2010)

go_for_it <- go_for_it_all %>%
  
  # calculate the number of seconds left for each play. not used.

    mutate(second_left = case_when(
    qtr == 1 ~ ((3 * 900) + mins * 60 + seconds),
    qtr == 2 ~ ((2 * 900) + mins * 60 + seconds),
    qtr == 3 ~ ((1 * 900) + mins * 60 + seconds),
    qtr >= 4 ~ mins * 60 + seconds,
    TRUE ~ 0,
  )) %>%
  
  # only consider plays where the win probability boost by going
  # is half a percent or higher to account for some model error,
  # and filter so that the probability of winning the game
  # at that point in time is 20% or greater so we eliminate desperation calls.
  
  filter(go_boost >= 0.5,
         prior_wp >= .2)

# get regular season go for it plays

team_regular_season <- go_for_it %>%
  ungroup() %>%
  mutate(game_id = as.factor(game_id)) %>%
  filter(week <= 17) %>% 
  group_by(posteam, season) %>% 
  summarize(n = n(),
            go = sum(go),
            went_for_it_rate = ifelse(sum(go) > 0, sum(go) / n, 0)
  ) %>% ungroup()

team_postseason <- go_for_it %>%
  ungroup() %>%
  mutate(game_id = as.factor(game_id)) %>%
  filter(week > 17) %>% 
  group_by(posteam, season) %>% 
  summarize(n = n(),
            go = sum(go),
            went_for_it_rate = ifelse(sum(go) > 0, sum(go) / n, 0)
  ) %>% ungroup()

playoff_teams <- team_regular_season %>%
  inner_join(team_postseason, by = c("posteam", "season"), suffix = c(".reg", ".post")) %>%
  mutate(difference = went_for_it_rate.post - went_for_it_rate.reg)

# this isn't really useful since the N is so small. Let's look at the six NFL coaches
# with tenures on the same team stretching back to at least 2013.
# NE, PIT, NO, BAL, SEA, KC
# FC: https://en.wikipedia.org/wiki/List_of_current_National_Football_League_head_coaches

stable_head_coaches <- playoff_teams %>%
  filter(posteam %in% c("NE", "PIT", "NO", "BAL", "SEA", "KC"),
         season >= 2013) %>%
  group_by(posteam) %>%
  summarize(n.reg = sum(n.reg),
            go.reg = sum(go.reg),
            n.post = sum(n.post),
            go.post = sum(go.post),
            went_for_it_rate.reg = ifelse(sum(go.reg) > 0, sum(go.reg) / n.reg, 0),
            went_for_it_rate.post = ifelse(sum(go.post) > 0, sum(go.post) / n.post, 0)
  ) %>% ungroup() %>%
  mutate(difference = went_for_it_rate.post - went_for_it_rate.reg)

# write to csv for chart

write_csv(stable_head_coaches, "538/stable_hc.csv")

# leaguewide numbers

league_regular_season <- go_for_it %>%
  ungroup() %>%
  mutate(game_id = as.factor(game_id)) %>%
  filter(week <= 17) %>% 
  summarize(n = n(),
            go = sum(go),
            went_for_it_rate = ifelse(sum(go) > 0, sum(go) / n, 0)
  )

league_postseason <- go_for_it %>%
  ungroup() %>%
  mutate(game_id = as.factor(game_id)) %>%
  filter(week > 17) %>% 
  summarize(n = n(),
            go = sum(go),
            went_for_it_rate = ifelse(sum(go) > 0, sum(go) / n, 0)
  )
