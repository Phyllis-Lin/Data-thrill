library(calendR)
library(ggalluvial)
library(ggflags)
library(ggpubr)
library(ggrepel)
library(ggtext)
library(ggthemes)
library(ggTimeSeries)
library(hrbrthemes)
library(leaflet)
library(magrittr)
library(patchwork)
library(png)
library(RColorBrewer)
library(rworldmap)
library(transformr)
library(tvthemes)
library(waffle)
library(zoo)
require(ggimage)
require(magick)


#
# # Get Image for City Hotel and write it in images folder
#
#   image_read(path = "https://images.fineartamerica.com/images/artworkimages/mediumlarge/3/rua-augusta-arch-lisbon-portugal-black-and-white-carol-japp.jpg") %>%
#   image_charcoal() %>%
#   magick::image_write(format = "png", path = here::here("tidy/images/city-hotel.png"))


# # Get Image for Resort Hotel and write it in images folder
#
#  image_read(path = "https://static.outdoorvisit.com/photos/regular/4/b/4b9333fb-ccc3-47f7-aaab-b850783c5a80.jpg") %>%
#  image_charcoal() %>%  # make it chackroal
#  image_fill("lightblue", point = "+50+400", fuzz = 4 ) %>%  #  give it some color
#  magick::image_write(format = "png", path = here::here("tidy/images/resort-hotel.png"))


# Work with tibble format


final %>%
  mutate(Country = as.factor(Country)) %>%
  count(Country, is_canceled) %>%
  group_by(Country) %>%
  mutate(total_bookings = sum(n),
         perc_cancel = case_when(
           is_canceled == 1 ~ n/total_bookings
         ),
         perc_arrived = case_when(
           is_canceled == 0 ~ n/total_bookings
         )
  ) %>%
  select(Country, total_bookings, contains("perc")) %>%
  pivot_longer(
    cols = contains("perc"),
    names_to = "outcome",
    values_to = "percentage"
  ) %>%
  arrange(desc(total_bookings)) %>%
  na.omit() %>%
  ungroup() %>%
  #filter(country != "PRT" & country!= "NULL") %>%
  slice_head(n = 20) %>%
  mutate(code = tolower(countrycode::countrycode(sourcevar = Country,
                                                 origin =  "country.name",
                                                 destination = "iso2c"))) %>%
  filter(outcome == "perc_arrived") %>%
  ggplot(aes(fct_reorder(Country, -total_bookings), total_bookings, fill = percentage)) +
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
      fill = "azure1",
    ),
    legend.title = element_textbox_simple(
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(8, "pt"),
      fill = "azure1",
    ),
    plot.title = element_markdown(face = "bold",
                                  size = 14),
    plot.subtitle = element_markdown(),
    plot.title.position = "plot",
    panel.background = element_blank()
  ) +
  scale_fill_distiller() +
  labs(
    x = "",
    y = "Total Bookings",
    title = "Top - 10 Countries by number of bookings",
    subtitle = "The fill of the columns indicates the proportion<br> of bookings
    that was **not** canceled")





















# Shows how many bookings each country has made for either resort or hotel
# and how many of those were canceled or proceeded normally
bookings_table <- final %>%
  filter(!country == "NULL") %>%      # In some observations we don't know the country
  count(country, hotel, is_canceled)


# Let's derive some stats:
# 1. Top-10 countries with most bookings

bookings_by_countries <- bookings_table %>%
  group_by(country) %>%
  summarise(total_bookings = sum(n, na.rm = TRUE)) %>%
  mutate(booking_prop = total_bookings/sum(total_bookings, na.rm = TRUE) * 100) %>%
  arrange(desc(total_bookings))


## I show this in the first figure no need to add it again

#As we expected Portugal is first the rest countreis are all in Europe except of Brazil

#Now let's calculate the proportion of this bookings in the total




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

city <- readPNG("tidy/images/city-hotel.png")
resort <- readPNG("tidy/images/resort-hotel.png")


#Lets use some figures
  resort_prop %>%
  ggplot(aes(fct_reorder(country, hotel_booking_prop), hotel_booking_prop )) +
  background_image(resort) +
  geom_segment(aes(xend=country, yend=0), color="#5D7783", lwd = 1.6) +
  geom_point( size = 2, color="#DC493D", shape =19) +
  coord_flip() +
  ggthemes::theme_solarized() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 10),
    plot.title.position = "plot",
    axis.text.y = element_text(
      face = "italic",
      colour = "black",
      size = 10
    )
  ) +
    labs(
      title = "Proportion of Bookings by Country fot the Resort Hotel",
      y = "Proportion"
    )




# Proportion of visits in the Resort Hotel
  city_prop %>%
  ggplot(aes(fct_reorder(country, hotel_booking_prop), hotel_booking_prop )) +
  background_image(city) +
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
  ) +
  ggthemes::theme_solarized()




# The graph also helps us to see that the countries are arranged differently.
#e.g. GBR is second in Resort's but 4th in City hotels,
# simmilarly for France and look at Ireland 4th in Resort but not in city top-10


# Explore this plots monthly in next questions and later use is_canceled

bookings_table_per_month <- final %>%
  filter(!country == "NULL") %>%
  count(country, hotel, arrival_date_month)

bookings_by_month <- bookings_table_per_month %>%
  group_by(arrival_date_month) %>%
  summarise(total_bookings = sum(n, na.rm = TRUE)) %>%
  arrange(desc(total_bookings))

bookings_by_month %>%
  head(10)

# Bussiest month as a table

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
  dplyr::filter(country %in% c(sample_countries)) %>%
  dplyr::mutate(year_month = tsibble::yearmonth(Date),
         country = tolower(countrycode::countrycode(sourcevar = country,
                                                    origin =  "country.name",
                                                    destination = "iso2c"))) %>%
  count(country, year_month)  %>%
  mutate(year_month = as.Date(year_month))




text_to_fig <- tbl %>%
  dplyr::filter(year_month == min(year_month) | year_month == max(year_month))

anim <-   ggplot(tbl, aes(year_month, n, country = country, )) +
  geom_point()+
  geom_text(text_to_fig, mapping = aes(label = country), show.legend = FALSE) +
  theme_bw() +
  scale_y_log10() +
  geom_flag() +
  scale_country() +
  transition_time(year_month)


#devtools::install_github("thomasp85/transformr")

animate(anim, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
anim_save("output.gif")

















## When i should book to find cheap

install.packages("calendR")



book_day <- final %>%
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





calendar <- calendR(year = 2017,
        start = "M",
        special.days = book_day$day,
        special.col = "green",            # Color of the specified days
        low.col = "white",
        weeknames.size = 3,
        day.size = 2,
        orientation ="p",
        title = "",
        subtitle = "") +
  tvthemes::theme_avatar() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Cheapest Day to book"
  )


















#devtools::install_github('Ather-Energy/ggTimeSeries')



hotel_new %>%
  mutate(Date = lubridate::as_date(
    str_c(arrival_date_year,
          arrival_date_month,
          arrival_date_day_of_month,
          sep = "/")),
    year_month = as.Date(tsibble::yearmonth(Date))) %>%
  count(hotel, country, year_month) %>%
  ggplot_waterfall(cXColumnName = 'year_month',
                   cYColumnName = 'n'
  ) +
  facet_wrap(~hotel) +
  ggthemes::theme_solarized_2(light = FALSE) +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  )




visitors <- final %>%
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









### Time for map !!

## try to add 3D

  to_map <- final %>%
    mutate(Country = as.factor(Country)) %>%
    count(Country, is_canceled) %>%
    group_by(Country) %>%
    mutate(total_bookings = sum(n),
           perc_cancel = case_when(
             is_canceled == 1 ~ n/total_bookings
           ),
           perc_arrived = case_when(
             is_canceled == 0 ~ n/total_bookings
           )
    ) %>%
    select(Country, total_bookings, contains("perc")) %>%
    pivot_longer(
      cols = contains("perc"),
      names_to = "outcome",
      values_to = "percentage"
    ) %>%
    na.omit() %>%
    ungroup() %>%
    filter(outcome == "perc_cancel")






  mapCountry<- maps::map("world", fill = TRUE, plot = FALSE)


  match(mapCountry$names, final$Country)

  pal_fun <- colorQuantile(rev(brewer.pal(3,"RdYlGn")), NULL, n = 7,)


  color_perc <- to_map$percentage[match(mapCountry$names, to_map$Country)]


  map <- leaflet(mapCountry) %>% # create a blank canvas
    addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
    addPolygons( # draw polygons on top of the base map (tile)
      stroke = FALSE,
      smoothFactor = 0.2,
      fillOpacity = 1,
      color = ~pal_fun(color_perc) # use the rate of each state to find the correct color
    )

  map





pictogram <- final %>%
  count(hotel, arrival_date_month, adults, children, babies) %>%
  pivot_longer(cols = c("adults", "children", "babies"),
               names_to = "group",
               values_to = "number") %>%
  group_by(hotel, arrival_date_month, group) %>%
  summarise(total  = sum(number)) %>%
  filter(arrival_date_month == "October",
         hotel == "Resort Hotel") %>%
  arrange(group) %>%
  ggplot(aes(label = group, values = total)) +
  geom_pictogram(n_rows = 10, aes(colour = group), flip = TRUE, make_proportional = TRUE) +
  scale_color_manual(
    name = NULL,
    values = c("#a40000", "#c68958", "#ae6056"),
    labels = c("Adult", "Baby", "Child")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c( "female", "baby-carriage", "child"),
    labels = c("Adult", "Baby", "Child")
  ) +
  coord_equal() +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75))







## Logistic regression



reg_data <- final %>%
  mutate(total_youth = children + babies,
         stay_length = stays_in_week_nights + stays_in_weekend_nights,
         family = case_when(
           total_youth == 0 ~ "Without children",
           total_youth > 0 ~ "With children"),
         visitor_type = case_when(
           stay_length <= 2 ~ "max 2 days",
           stay_length > 2 & stay_length <= 5  ~ "max 5 days",
           stay_length > 5 & stay_length <= 7  ~ "max 7 days",
           stay_length > 7 ~ "more than 7 days")
  )


mod <- glm(is_canceled ~ stay_length + total_youth, data = reg_data)


mod_aug <-mod %>%
  broom::augment(type.predict = "response") %>%
  mutate(y_hat = .fitted)


log <- ggplot(mod_aug, aes(x = stay_length, y = y_hat)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("Probability of cancelling booking", limits = c(0, 1))



