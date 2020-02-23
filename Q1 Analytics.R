library(dplyr)

df = readxl::read_xlsx("Adops & Data Scientist Sample Data.xlsx", sheet = 1)

# Q1
BDV = df %>% 
  filter(country_id == "BDV") # filter the subset to work on 

BDV_count = BDV %>%
  group_by(site_id) %>% 
  summarise(unique_user = n_distinct(user_id)) %>% # get numbers of unique user id for each site
  arrange(desc(unique_user)) %>% 
  filter(row_number() == 1)  # sort by desceding order and select the biggest observation

# Q2
df$ts = as.POSIXct(df$ts) # convert time variable into DateTime object
visit_time = df %>%
  filter(ts > "2019-02-03 00:00:00" & ts < "2019-02-04 23:59:59") %>% # filter target time period
  group_by(user_id, site_id) %>% # for each user, each site
  count() %>%                    # count number of site visits
  filter(n > 10) # choose users with more than 10 visits

# Q3
last_visit = df %>%
  group_by(user_id) %>% 
  arrange(ts) %>%
  summarise(last_site = last(site_id)) %>% # get last visited site for each user id 
  count(last_site) %>% # for each site, calculate number of last visits
  top_n(3, n) # select top 3 sites

# Q4
user_visits = df %>%
  group_by(user_id) %>%
  arrange(ts) %>% # sort by time 
  summarise(first_visit = first(site_id),
         last_visit = last(site_id)) %>% # find first and last visited site
  mutate(same = ifelse(first_visit == last_visit, 1, 0)) %>% # check if first and last are same
  filter(same == 1) %>% count() # count same visits


