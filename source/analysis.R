# load packages
library("tidyverse")

# load dataset
incarceration_data <- read.csv("../incarceration_trends.csv")

# what states don't we have any data on in this dataset?
# also have almost no data on AK
state_data <- incarceration_data %>% 
  filter(year == 2000)%>% 
  group_by(state) %>% 
  summarize(state_jail_pop = sum(total_jail_pop, na.rm = T)) %>% 
  filter(state_jail_pop == min(state_jail_pop))

# what state has the most people incarcerated in 2018?
most_jail_pop_2018 <- incarceration_data %>% 
  filter(year == 2018)%>% 
  group_by(state) %>% 
  summarize(state_jail_pop = sum(total_jail_pop, na.rm = T)) %>% 
  filter(state_jail_pop == max(state_jail_pop))

# what state had the highest population of 
# incarcerated people VS total population in 2018?
highest_jail_pro_2018 <- incarceration_data %>% 
  filter(year == 2018)%>% 
  group_by(state) %>% 
  summarize(state_jail_pop = sum(total_jail_pop, na.rm = T), 
            state_pop = sum(total_pop_15to64, na.rm = T), 
            prop_in_jail = state_jail_pop/state_pop)%>% 
  filter(prop_in_jail == max(prop_in_jail))

# In 2018, what % of america was black in 2018?
amt_black_2018 <- incarceration_data %>% 
  filter(year == 2018) %>% 
  summarize(country_pop = sum(total_pop_15to64),
            black_pop = sum(black_pop_15to64),
            per_black = black_pop / country_pop)

# what % of people in LA are black? 
LA_rate_black <- incarceration_data %>% 
  filter(year == 2018, state == 'LA') %>% 
  summarize(state_pop = sum(total_pop_15to64),
            black_pop = sum(black_pop_15to64),
            per_black = black_pop / state_pop)

# what % of the incarcerated population of LA are black?
LA_rate_black_incarcerated <- incarceration_data %>% 
  filter(year == 2018, state == 'LA')%>% 
  summarize(LA_state_jail_pop = sum(total_jail_pop, na.rm=T),
            LA_black_jail_pop = sum(black_jail_pop, na.rm=T),
            per_incarcerated_black = LA_black_jail_pop / LA_state_jail_pop)

# what % of the incarcerated population are black?
nation_rate_black_incarcerated <- incarceration_data %>% 
  filter(year == 2018)%>% 
  summarize(LA_state_jail_pop = sum(total_jail_pop, na.rm=T),
            LA_black_jail_pop = sum(black_jail_pop, na.rm=T),
            per_incarcerated_black = LA_black_jail_pop / LA_state_jail_pop)

########### time graphs 

# % prison population by race over time?
# seems like until 1985 data on race isn't reported in this dataset
pop_race_by_year <- incarceration_data %>% 
  group_by(year) %>% 
  summarize(nat_state_jail_pop = sum(total_jail_pop, na.rm=T),
            nat_black_jail_pop = sum(black_jail_pop, na.rm=T),
            nat_aapi_jail_pop = sum(aapi_jail_pop, na.rm=T),
            nat_latinx_jail_pop = sum(latinx_jail_pop, na.rm=T),
            nat_native_jail_pop = sum(native_jail_pop, na.rm=T),
            nat_white_jail_pop = sum(white_jail_pop, na.rm=T),
            nat_other_jail_pop = sum(other_race_jail_pop, na.rm=T),
            per_incarcerated_black = nat_black_jail_pop / nat_state_jail_pop,
            per_incarcerated_aapi = nat_aapi_jail_pop / nat_state_jail_pop,
            per_incarcerated_latinx = nat_latinx_jail_pop / nat_state_jail_pop,
            per_incarcerated_native = nat_native_jail_pop / nat_state_jail_pop,
            per_incarcerated_white = nat_white_jail_pop / nat_state_jail_pop,
            per_incarcerated_other = nat_other_jail_pop / nat_state_jail_pop) %>% 
  select(year, per_incarcerated_black, per_incarcerated_aapi, per_incarcerated_latinx, 
         per_incarcerated_native, per_incarcerated_white, per_incarcerated_other)

# take the wide analysis and put it into a usable long format
long_pop <- gather(
  pop_race_by_year,
  key = race,
  value = percent,
  -year
)

# plot of prison population makeup over time
long_plot <- ggplot(long_pop, mapping = aes(x = year, y = percent, group = race, color = race)) +
  geom_line(mapping = aes(x = year, y = percent))+
  scale_x_continuous(limits = c(1985, NA))+
  labs(x = "year", y = "percent of prison population")+
  ggtitle("Percent prison population makeup by race")

# % of population by race over time
pop_race_by_year_n <- incarceration_data %>% 
  group_by(year) %>% 
  summarize(nat_state_pop = sum(total_pop_15to64, na.rm=T),
            nat_black_pop = sum(black_pop_15to64, na.rm=T),
            nat_aapi_pop = sum(aapi_pop_15to64, na.rm=T),
            nat_latinx_pop = sum(latinx_pop_15to64, na.rm=T),
            nat_native_pop = sum(native_pop_15to64, na.rm=T),
            nat_white_pop = sum(white_pop_15to64, na.rm=T),
            per_black = nat_black_pop / nat_state_pop,
            per_aapi = nat_aapi_pop / nat_state_pop,
            per_latinx = nat_latinx_pop / nat_state_pop,
            per_native = nat_native_pop / nat_state_pop,
            per_white = nat_white_pop / nat_state_pop) %>% 
  select(year, per_black, per_aapi, per_latinx, per_native, per_white)

# change the analysis of US race makeup over time to a usable long format
long_pop_n <- gather(
  pop_race_by_year_n,
  key = race,
  value = percent,
  -year
)

# plot US population demographics over time
long_plot_n <- ggplot(long_pop_n, mapping = aes(x = year, y = percent, group = race, color = race)) +
  geom_line(mapping = aes(x = year, y = percent))+
  scale_x_continuous(limits = c(1990, NA))+
  labs(x = "year", y = "percent of national population")+
  ggtitle("Percent national population makeup by race")


# the proportion of white people in prison vs the total population of whites
# tells us the rate of incarceration. 
# this is why coliberation is needed!
white_incarceration_rate <- incarceration_data %>% 
  group_by(year) %>% 
  summarize(per_nat_white = sum(white_jail_pop, na.rm=T)/ sum(white_pop_15to64, na.rm=T))

# number of white people in prison vs white population by year/incarceration rate
white_incarceration_rate_graph <- ggplot(white_incarceration_rate, aes(x=year, y=per_nat_white))+
  geom_line()+
  scale_x_continuous(limits = c(1990, NA))+
  labs(x = "year", y = "incarceration rate")+
  ggtitle("Incarceration rate of whites from 1990 to 2018")

############ making the scatter

# black population vs incarceration rates
# black incarceration by county
black_incarceration <- incarceration_data %>% 
  filter(year == 2018)%>% 
  mutate(per_county_black_rate = black_jail_pop / black_pop_15to64) %>% 
  select(per_county_black_rate, county_name, black_pop_15to64, state)

# requires a little zooming in but the graph does show a real increase in 
# incarseration rates the lower a population gets
rate_vs_pop <- ggplot(black_incarceration, aes(x=black_pop_15to64, y=per_county_black_rate))+
  geom_point()+
  scale_y_continuous(limits = c(0, .2))+
  scale_x_continuous(limits = c(0, 100000))+
  geom_smooth(color = "red")+
  labs(x = "Black Population Per County", y = "black incarceration rate")+
  ggtitle("2018 Black Incarceration rate vs Black Population")


########### making the map

# black incarceration rate by state in 2018
black_incarceration <- incarceration_data %>% 
  filter(year == 2018)%>% 
  group_by(state) %>% 
  summarize(per_nat_black = sum(black_jail_pop, na.rm=T) / sum(black_pop_15to64, na.rm=T)) 

# get states actual names
state_names <- read.csv("../stateNames.csv") %>% 
  rename(full_state = `ï..State`, state = Code) %>% 
  select(full_state, state)

# set the state names to be usable
blk_incar_sn <- merge(black_incarceration, state_names) %>% 
  select(-state)
blk_incar_sn$full_state <- tolower(blk_incar_sn$full_state)

# pull the incarceration date into the map data
state_shape <- map_data("state") %>% 
  rename(full_state = region) %>% 
  left_join(blk_incar_sn, by="full_state")

# make a theme for the map
map_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# create the map of incarceration rates
Black_Incarceration_Rate_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = per_nat_black),
    color = "white", 
    size = .1
  ) +
  coord_map() + 
  scale_fill_continuous(low = "white", high = "black") +
  labs(fill = "Black Incarceration Rate by State")+
  map_theme
