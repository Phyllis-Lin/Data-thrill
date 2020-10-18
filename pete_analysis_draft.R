require(magick)
require(ggplot2)
require(ggimage)
library(ggtext)
library(png)
library(ggpubr)
library(ggflags)
library(rworldmap)
library(ggthemes)




# Get Image for City Hotel

portugal <- image_read(path = "https://images.fineartamerica.com/images/artworkimages/mediumlarge/3/rua-augusta-arch-lisbon-portugal-black-and-white-carol-japp.jpg")

por <- image_charcoal(portugal)
magick::image_write(image = por, format = "png", path = here::here("tidy/city-hotel.png"))


# Get Image for Resort Hotel

lisb <- image_read(path = "https://static.outdoorvisit.com/photos/regular/4/b/4b9333fb-ccc3-47f7-aaab-b850783c5a80.jpg")

img <- image_charcoal(lisb)  # make it chackroal

lisbon <- image_fill(img, "lightblue", point = "+50+400", fuzz = 4 ) # and give it some color


# Work with tibble format

hotel_new <- hotel %>%
  as_tibble()

## Small change iso3c country code of China is wrong it should be chn not ch
## and turn country column from iso3c format to country.name format

hotel_new <- hotel_new %>%
  mutate(country = str_replace(country, pattern = "cn", replacement = "chn"),
         country = countrycode::countrycode(sourcevar = country,
                                            origin = "iso3c",
                                            destination = "country.name")
  )


# This plot shows the Number of bookings made by country and
#  the fill indicates the percentage of those bookings that actualy arrived to the hotel

### This goes to the report

hotel_new %>%
  mutate(country = as.factor(country)) %>%
  count(country, is_canceled) %>%
  group_by(country) %>%
  mutate(total_bookings = sum(n),
         perc_cancel = case_when(
           is_canceled == 1 ~ n/total_bookings
         ),
         perc_arrived = case_when(
           is_canceled == 0 ~ n/total_bookings
         )
  ) %>%
  select(country, total_bookings, contains("perc")) %>%
  pivot_longer(
    cols = contains("perc"),
    names_to = "outcome",
    values_to = "percentage"
  ) %>%
  arrange(desc(total_bookings)) %>%
  na.omit() %>%
  ungroup() %>%
  filter(country != "PRT" & country!= "NULL") %>%
  slice_head(n = 20) %>%
  mutate(code = tolower(countrycode::countrycode(sourcevar = country,
                                                 origin =  "country.name",
                                                 destination = "iso2c"))) %>%
  filter(outcome == "perc_arrived") %>%
  ggplot(aes(fct_reorder(country, -total_bookings), total_bookings, fill = percentage)) +
  geom_col() +
  ggflags::geom_flag(y = -1 ,aes(country = code), size = 6, show.legend = FALSE) +
  scale_country() +
  ggthemes::theme_solarized_2(light = FALSE) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =8),
    axis.title.y = element_textbox_simple(
      width = NULL,
      orientation = "left-rotated",
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(8, "pt"),
      fill = "azure1"
    )
  ) +
  scale_fill_continuous_tableau() +
  labs(
    x = "",
    y = "Total Bookings"
  )










# here I explore adr by country
hotel %>%
  select(country,arrival_date_year, arrival_date_month, stays_in_week_nights, stays_in_weekend_nights, adults, children, babies, adr) %>%
  group_by(country, arrival_date_year, arrival_date_month, stays_in_week_nights, stays_in_weekend_nights,
           adults, children, babies) %>%
  summarise(country_avg_adr = mean(adr)) %>%
  filter(country_avg_adr !=0) %>%
  filter(adults == 2,children == 0 & babies == 0 ) %>%
  arrange(arrival_date_month, stays_in_week_nights, stays_in_weekend_nights) %>%
  View()




library(patchwork)

# Shows how many bookings each country has made for either resort or hotel
# and how many of those were canceled or proceeded normally
bookings_table <- hotel_new %>%
  filter(!country == "NULL") %>%      # In some observations we don't know the country
  count(country, hotel, is_canceled)

# Let's derive some stats:
# 1. Top-10 countries with most bookings

bookings_by_countries <- bookings_table %>%
  group_by(country) %>%
  summarise(total_bookings = sum(n, na.rm = TRUE)) %>%
  arrange(desc(total_bookings))

bookings_by_countries %>%
  head(10)

## I show this in the first figure no need to add it again

#As we expected Portugal is first the rest countreis are all in Europe except of Brazil

#Now let's calculate the proportion of this bookings in the total

bookings_by_countries %>%
  mutate(booking_prop = total_bookings/sum(total_bookings, na.rm = TRUE) * 100)

# The percent in the total bookings is more interesting

# As wee see out of all bookings 40% of those come from within Portugal

# 2.  Top-10 countries with most bookings by hotel type

# Now lets see if the above proportions will be simillar among hotel types
## at the moment we don't look at the is_cancelled variable

hotel_book_prop <- bookings_table %>%
  group_by(country, hotel) %>%
  summarise(hotel_bookings =  sum(n, na.rm = TRUE)) %>%
# we have total bookings bookings by country and now we calc. the proportion
  group_by(hotel) %>%
  mutate(hotel_booking_prop = hotel_bookings/sum(hotel_bookings, na.rm = TRUE)) %>%
  arrange(hotel, desc(hotel_booking_prop))

# Now that we have the table let's compare the proportions of the first 10 countries for
#each hotel type

resort_prop <- hotel_book_prop %>%
  filter(hotel == "Resort Hotel") %>%
  head(10)


city_prop <- hotel_book_prop %>%
  filter(hotel == "City Hotel") %>%
  head(10)

# The majority of the countries are the same

common_countries <-   resort_prop %>%
    filter(country %in%  city_prop$country) %>%
    pull(country)

diff_countries <-   resort_prop %>%
  filter(!country %in%  city_prop$country) %>%
  pull(country)

sample_countries <- c(common_countries, diff_countries)



# IRL is more interested in the resort hotel

# Proportion of visits in the Resort Hotel





#Lets use some figures
 resort_prop %>%
  ggplot(aes(fct_reorder(country, hotel_booking_prop), hotel_booking_prop )) +
  background_image(lisbon) +
  geom_segment(aes(xend=country, yend=0), color="#5D7783", lwd = 1.6) +
  geom_point( size = 5, color="#DC493D", shape =19) +
  coord_flip() +
  labs(
    y = "Proportion of Resort Hotel Bookings"
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 8),
    plot.title.position = "plot",
    axis.text = element_text(size = 10)
  ) +
   ggtitle("Proportion of Bookings by Hotel") +


# Proportion of visits in the Resort Hotel
city_prop %>%
  ggplot(aes(fct_reorder(country, hotel_booking_prop), hotel_booking_prop )) +
  background_image(por) +
  geom_segment( aes(xend=country, yend=0), color="#5D7783", lwd = 1.6) +
  geom_point( size=5, color="#DC493D", shape =19, ) +
  coord_flip() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 7.5),
    axis.text = element_text(size = 10)
  ) +
  labs(
    y = "Proportion of City Hotel Bookings"
  )




# The graph also helps us to see that the countries are arranged differently.
#e.g. GBR is second in Resort's but 4th in City hotels,
# simmilarly for France and look at Ireland 4th in Resort but not in city top-10


# Explore this plots monthly in next questions and later use is_canceled

bookings_table_per_month <- hotel_new %>%
  filter(!country == "NULL") %>%
  count(country, hotel, arrival_date_month)

bookings_by_month <- bookings_table_per_month %>%
  group_by(arrival_date_month) %>%
  summarise(total_bookings = sum(n, na.rm = TRUE)) %>%
  arrange(desc(total_bookings))

bookings_by_month %>%
  head(10)



bookings_by_month %>%
  mutate(month_booking_prop = total_bookings/sum(total_bookings, na.rm = TRUE) * 100)

## A nice table summary to add

# Comment on the results


bookings_table_per_month$arrival_date_month <- factor(bookings_table_per_month$arrival_date_month,
                                                      levels = c(
                                                        "January",   "February",  "March",     "April",
                                                        "May",       "June",      "July",      "August",
                                                        "September", "October",   "November",  "December")
)



month_hotel_book_prop <- bookings_table_per_month %>%
  group_by(country, hotel, arrival_date_month) %>%
  summarise(hotel_bookings =  sum(n, na.rm = TRUE)) %>%
  # we have total bookings bookings by country and now we calc. the proportion
  group_by(arrival_date_month) %>%
  mutate(hotel_booking_prop = hotel_bookings/sum(hotel_bookings, na.rm = TRUE)) %>%
  arrange(hotel, desc(hotel_booking_prop))

# I can visually the trend in bookings for each country(potential for Shiny)


# Also goes as third figure

month_hotel_book_prop %>%
  filter(country %in% c("Portugal", "United Kingdom","France", "Spain", "Germany"))  %>%
  ggplot(aes(arrival_date_month, hotel_booking_prop, group =country, color = country )) +
  geom_line(lwd = 1.4) +
  facet_wrap(~hotel) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
  scale_y_log10() +
  scale_color_tableau()


# Portugals avoid city hotels during winter while foreing countries prefere them, but
# still the majority of bookings is from Portugal


## PLan animation of bookings


library(ggflags)
library(ggrepel)

#fig 4


# Shiny potential !! user can select month, year and countries to visualize

set.seed(1010)
bookings_table_per_month %>%
  filter(n >100,
         arrival_date_month == "August") %>%
  mutate(country = as.factor(country)) %>%
  mutate(code = tolower(countrycode::countrycode(sourcevar = country,
                                                 origin =  "country.name",
                                                 destination = "iso2c"))) %>%
  ggplot(aes(x=arrival_date_month, y=n, country = code, label = country, size = n)) +
  geom_flag(position = position_jitter(width = 0.3, height = 0.1)) +
  scale_country() +
  scale_size(range = c(5, 12)) +
  facet_wrap(~hotel, ncol = 1) +
  guides(size = FALSE) +
  ggthemes::theme_solarized_2(light = FALSE)
  #ggthemes::theme_hc(style = "darkunica")


library(zoo)
library(gganimate)
library(transformr)

# Question: Which Countries Book trips to  Portugal more often and how the number of bookings
# changes over time

bookings_number <- hotel_new %>%
  filter(!country == "NULL") %>%      # In some observations we don't know the country
  count(country, hotel) %>%
  arrange(desc(n))

city <- bookings_number %>%
  filter(hotel == "City Hotel")

resort <- bookings_number %>%
  filter(hotel == "Resort Hotel")

## I can also use the common countries

## Now let's take this top-100 countries and add to them some time elemnt

# To do this let's combine the arrival day month and year in a new variable

hotel <- hotel_new %>%
  mutate(Date = lubridate::as_date(
    str_c(arrival_date_year,
          arrival_date_month,
          arrival_date_day_of_month,
          sep = "/"))
  )


## Let's count the number of bookings for the to-10 countries by year



tbl <- hotel %>%
  filter(country %in% c(sample_countries)) %>%
  mutate(year_month = tsibble::yearmonth(Date),
         country = tolower(countrycode::countrycode(sourcevar = country,
                                                    origin =  "country.name",
                                                    destination = "iso2c"))) %>%
  count(country, year_month)  %>%
  mutate(year_month = as.Date(year_month))




text_to_fig <- tbl %>%
  dplyr::filter(year_month == min(year_month) | year_month == max(year_month))

anim <-   ggplot(tbl, aes(year_month, n, group = country, country = country, color = country )) +
  geom_line() +
  geom_text(text_to_fig, mapping = aes(label = country), show.legend = FALSE) +
  theme_bw() +
  scale_y_log10() +
  geom_flag() +
  scale_country() +
  transition_time(year_month)


#devtools::install_github("thomasp85/transformr")

anim




library(ggridges)
hotels %>%
  filter(country %in% c("GBR"))  %>%
  ggplot(aes(adr, meal, fill = as.factor(children))) +
  geom_density_ridges(scale = 4) +
  scale_fill_westeros(palette = "Stark")


library(ggplot2)
library(ggpol)




perc <- hotels %>%
  filter(arrival_date_year == 2017, arrival_date_month == "January") %>%
  count(country) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  mutate(percernt = round(n/sum(n) * 100))

  ggplot(perc) +
  geom_parliament(aes(seats = percernt, fill = country), color = "black") +
  scale_fill_viridis_d(labels = perc$country) +
  coord_fixed() +
  theme_void()




  head(titanic_long)
  #>   Survived Freq alluvium Demographic stratum
  #> 1       No    0        1       Class     1st
  #> 2       No    0        2       Class     2nd
  #> 3       No   35        3       Class     3rd
  #> 4       No    0        4       Class    Crew
  #> 5       No    0        5       Class     1st
  #> 6       No    0        6       Class     2nd


  install.packages("ggalluvial")
  library(ggalluvial)

  hotels %>%
    count(hotel, reserved_room_type, adults, meal, previous_cancellations, name = "Freq") %>%
    ggplot(
           aes(axis1 = hotel, axis2 = reserved_room_type, axis3 = meal,
               y = Freq)) +
    scale_x_discrete(limits = c("Hotel", "Room Type", "Meal Type"), expand = c(.2, .05)) +
    xlab("Demographic") +
    geom_alluvium(aes(fill = as.factor(adults))) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +


## When i should book to find cheap

install.packages("calendR")
  library(calendR)


book_day <- hotels %>%
  filter(arrival_date_year == 2017) %>%
  filter(children == 0 & babies == 0) %>%
  filter(stays_in_week_nights == 5) %>%
  select(reservation_status_date, adr) %>%
  arrange((adr)) %>%
  filter(adr != 0) %>%
  head(10) %>%
  as_tibble() %>%
  mutate(day = as.Date(reservation_status_date) - as.Date(as.character("2017-01-01"), format="%Y-%m-%d")) %>%
  filter(day > 0)





calendR(year = 2017,
        start = "M",
        special.days = book_day$day,
        special.col = "green",            # Color of the specified days
        low.col = "white")


install.packages("tvthemes")
library(tvthemes)



# remotes::install_github("tylermorganwall/rayshader")
# library(rayshader)
# library(ggplot2)
# library(tidyverse)
#
# gg = ggplot(diamonds, aes(x, depth)) +
#   stat_density_2d(aes(fill = stat(nlevel)),
#                   geom = "polygon",
#                   n = 100,bins = 10,contour = TRUE) +
#   facet_wrap(clarity~.) +
#   scale_fill_viridis_c(option = "A")
# plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250)
#
#
#






install.packages("ggalluvial")
library(ggalluvial)

most_visits <- hotels %>%
  count(country, name = "bookings", sort = TRUE)  %>%
  as_tibble() %>%
  head(10) %>%
  pull(country)



hotels %>%
  filter(country %in% most_visits) %>%
  mutate(total_youth = children + babies,
         stay_length = stays_in_week_nights + stays_in_weekend_nights ) %>%
  count(country, hotel, stay_length, total_youth, meal, is_canceled, name = "Freq") %>%
  ggplot(
    aes(axis1 = country, axis2 = hotel, axis3 = stay_length, axis4 = total_youth,
        axis5 = meal, y = Freq)) +
  #scale_x_discrete(limits = c("Hotel", "Hotel", "Meal Type"), expand = c(.2, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = as.factor(is_canceled))) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))







devtools::install_github('Ather-Energy/ggTimeSeries')
library(ggTimeSeries)

set.seed(1)
dfData = data.frame(x = 1:100, y = cumsum(rnorm(100)))

# base plot
p1 = ggplot_waterfall(
  dtData = dfData,
  'x',
  'y'
)

# adding some formatting
p1 +
  xlab(NULL) +
  ylab(NULL)



hotel_new %>%
  mutate(Date = lubridate::as_date(
    str_c(arrival_date_year,
          arrival_date_month,
          arrival_date_day_of_month,
          sep = "/")),
    year_month = as.Date(tsibble::yearmonth(Date))) %>%
  count(hotel, year_month) %>%
  ggplot_waterfall(
    'year_month',
    'n'
  ) +
  facet_wrap(~hotel) +
  ggthemes::theme_solarized_2(light = FALSE) +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  )




visitors <- hotel_new %>%
  mutate(Date = lubridate::as_date(
    str_c(arrival_date_year,
          arrival_date_month,
          arrival_date_day_of_month,
          sep = "/"))) %>%
  count(hotel, arrival_date_year, Date) %>%
  mutate(above_average = case_when(
    n > mean(n) ~ 1,
    n <= mean(n) ~ 0
  ))

library(patchwork)

visitors %>%
  filter(hotel == "Resort Hotel") %>%
ggplot_calendar_heatmap(
  'Date',
  'above_average',
  ) +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_continuous(low = 'red', high = 'green') +
  facet_wrap(~arrival_date_year, ncol = 1)

  visitors %>%
  filter(hotel == "City Hotel") %>%
  ggplot_calendar_heatmap(
    'Date',
    'above_average',
  ) +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_continuous(low = 'red', high = 'green') +
  facet_wrap(~arrival_date_year, ncol = 1)




hotel_new %>%
  mutate(total_youth = children + babies,
         stay_length = stays_in_week_nights + stays_in_weekend_nights ) %>%
  mutate(family_children = case_when(
    total_youth == 0 ~ "None",
    total_youth == 1 ~ "One",
    total_youth == 2 ~ "Two",
    total_youth == 3 ~ "Three",
    total_youth > 3 ~ "More than three",
  )) %>%
  ggplot(aes(stay_length, color = family_children)) +
  geom_density()


hotel_new %>%
  mutate(total_youth = children + babies,
         stay_length = stays_in_week_nights + stays_in_weekend_nights ) %>%
  filter(country %in% c("Portugal", "France", "Spain", "United Kingdom"))
  ggplot(aes(stay_length, fill = country)) +
  geom_density()







