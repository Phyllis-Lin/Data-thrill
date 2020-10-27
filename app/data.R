## ----setup, include=FALSE-----------------------------------------------------------
# Make sure you have the latest versions of rmarkdown and bookdown installed
library(ggplot2)
library(countrycode)
library(lubridate)
library(naniar)


## -----------------------------------------------------------------------------------
library(tidyverse)
library(calendR)
library(gridExtra)
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



final <- readRDS("final.rds")


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

city <- readPNG("www/CopyOfcity-hotel.png")
resort <- readPNG("www/CopyOfresort-hotel.png")


## -----------------------------------------------------------------------------------
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



## -----------------------------------------------------------------------------------
visitors <- final %>%
  mutate(Date = lubridate::as_date(
    str_c(arrival_date_year,
          arrival_date_month,
          arrival_date_day_of_month,
          sep = "/"))) %>%
  group_by(hotel) %>%
  count(arrival_date_year, Date) %>%
  mutate(above_average = case_when(
    n > mean(n) ~ 1,
    n <= mean(n) ~ 0
  ))



## -----------------------------------------------------------------------------------
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



## -----------------------------------------------------------------------------------
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


save(final,  file = "final.rda")




## -----------------------------------------------------------------------------------
final$arrival_date_month <- factor(final$arrival_date_month,
                                                      levels = c(
                                                        "January",   "February",  "March",     "April",
                                                        "May",       "June",      "July",      "August",
                                                        "September", "October",   "November",  "December")
)


## -----------------------------------------------------------------------------------
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






## -----------------------------------------------------------------------------------
dt1 <-final %>%
  select(hotel,customer_type) %>%
  count(customer_type)

# dt1 = dt1[order(dt1$n, decreasing = TRUE),]
# myLabel = as.vector(dt1$customer_type)
# #myLabel = paste(myLabel, "(", round(dt$n / sum(dt$n) * 100, 2), "%)", sep = "")

#
# pp = ggplot(dt, aes(x = "", y = n, fill = customer_type)) +
#   geom_bar(stat = "identity", width = 0.5) +
#   coord_polar(theta = "y") +
#   theme_bw() +
#   labs(x = "", y = "", title = "") +
#   theme(axis.ticks = element_blank()) +
#   #theme(legend.position = "none") +
#   theme(legend.title = element_blank(), legend.position = "left")+
#   #geom_text(aes(y = n/20 + c(0, cumsum(n)[-length(n)]), x = sum(n)/20, label = myLabel), size = 2)+
#   #geom_text(vjust=0.3,hjust=0.4)+
#   theme(axis.text.x = element_blank()) +
#   theme(panel.grid=element_blank()) +
#   theme(panel.border=element_blank())+
#   labs(title = "the presentage of customer type")
# pp
#
#



## ----the density plot by number of customers----------------------------------------
age1<- final%>%
  select(hotel,adults,children,babies) %>%
  filter(hotel=="Resort Hotel") %>%
  mutate(all=adults+children+babies)

q1<-ggplot(age1, aes(x = all))+
  geom_density(fill="green")+
  xlim(0,5)+
  xlab("the number of customers in resort-hotel") +
  labs(title = "the density plot in resort-hotel")

age2<- final%>%
  select(hotel,adults,children,babies) %>%
  filter(hotel=="City Hotel") %>%
  mutate(all=adults+children+babies)

q2<-ggplot(age2, aes(x = all))+
  geom_density(fill="red")+
  xlim(0,5)+
  xlab("the number of customers in city-hotel") +
  labs(title = "the density plot in city-hotel")




## ----leadtime-----------------------------------------------------------------------
ltime <- final %>%
  select(hotel,lead_time)

q3<-ggplot(ltime, aes(x =lead_time))+
  geom_density(aes(color=hotel),alpha=0.4)+
  geom_vline(xintercept = mean(ltime$lead_time),color="blue", linetype="dashed")+
  labs(title = "the density of booking hotel days in advance")+
  xlab("the number of day booking hotel in advance")



## -----------------------------------------------------------------------------------
night <- final  %>%
  select(hotel,stays_in_week_nights)

n<-ggplot(night, aes(x =stays_in_week_nights))+
  geom_density(aes(color=stays_in_week_nights),fill="red3",alpha=0.4)+
  geom_vline(xintercept = mean(night$stays_in_week_nights),color="blue", linetype="dashed")+
  xlim(0,20)+
  labs(title = "Number of days to stay in the hotel on weekdays")

night1 <- final %>%
  select(hotel,stays_in_weekend_nights)

n1<-ggplot(night1, aes(x =stays_in_weekend_nights))+
  geom_density(aes(color=stays_in_weekend_nights),fill="green3",alpha=0.4)+
  geom_vline(xintercept = mean(night1$stays_in_weekend_nights),color="blue", linetype="dashed")+
  xlim(0,10)+
  labs(title = "Number of days to stay in the hotel on weekdays")




## -----------------------------------------------------------------------------------
month <- final %>%
  select(hotel,arrival_date_month,arrival_date_year) %>%
  filter(hotel=="Resort Hotel")

mr<-ggplot(month, aes(x =arrival_date_month))+
  geom_density(aes(color=arrival_date_year),fill="greenyellow",alpha=0.4)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "the customer density plot by month in resort-hotel")+
  geom_vline(xintercept = mean(month$arrival_date_month, na.rm = TRUE),color="blue", linetype="dashed")+
  facet_wrap(~arrival_date_year)



month2 <- final %>%
  select(hotel,arrival_date_month,arrival_date_year) %>%
  filter(hotel=="City Hotel")

mc<-ggplot(month2, aes(x =arrival_date_month))+
  geom_density(aes(color=arrival_date_year),fill="red3",alpha=0.3)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "the customer density plot by month in city-hotel")+
  geom_vline(xintercept = mean(month$arrival_date_month, na.rm = TRUE),color="blue", linetype="dashed")+
  facet_wrap(~arrival_date_year)




## -----------------------------------------------------------------------------------
library(plotly)
month_2016 <- final %>%
  filter(arrival_date_year == 2016) %>%
  group_by(arrival_date_day_of_month, arrival_date_month, hotel) %>%
  count(Count=n())

g1 <- ggplot(month_2016,
       aes(x=arrival_date_day_of_month, y=Count, color=arrival_date_month)) +
  xlab("Arrival day") +
  geom_col() +
theme(axis.text.x = element_text())





## -----------------------------------------------------------------------------------
month_2016$arrival_date_month <- factor (month_2016$arrival_date_month,
                     levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

month_book <- ggplot(month_2016,
       aes(x=arrival_date_month, y=Count, fill=hotel)) +
  geom_bar(stat = "identity",
position = "stack") +
  xlab("Month") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



## -----------------------------------------------------------------------------------
# library(kableExtra)
# library(dplyr)
# Top10Country <- final %>%
#   count(Country, sort = T) %>%
#   head(10) %>%
#   kable(caption = "Top10 coutries reserved")
# Top10Country


## -----------------------------------------------------------------------------------
month_countries <- final %>%
  filter(arrival_date_year == 2016,
         Country %in%c("Portugal", "United Kingdom", "Australia", "China")) %>%
  group_by(arrival_date_month, hotel, Country) %>%
  count(Count=n())
month_countries$arrival_date_month <- factor (month_countries$arrival_date_month,
                     levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

month_country <- ggplot(month_countries,
       aes(x=arrival_date_month, y=Count, fill=hotel)) +
  geom_bar(stat = "identity",
position = "stack") +
  xlab("Month") +
  facet_wrap(~Country, scales = "free_y") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ----Q8_1---------------------------------------------------------------------------

a <- final%>%
  select(hotel,Country,is_repeated_guest)%>%
  count(hotel,Country,is_repeated_guest)%>%
  pivot_wider(names_from="is_repeated_guest",values_from=n)%>%
  rename(not_repeated_guest="0",repeated_guest="1")%>%
  filter(!is.na(repeated_guest))%>%
  mutate(prop=round(repeated_guest/(not_repeated_guest+repeated_guest),digits=3))%>%
  arrange(desc(prop))%>%
  ungroup()%>%
  slice_head(n =20)

prop_c <- a%>%
  filter(hotel=="City Hotel")%>%
  ggplot(aes(x=reorder(Country,prop),y=prop))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  theme_minimal()+
  labs(title = "Repeated guest proportion of City Hotel from July 2015 to August 2017",
       x = "",
       y="proportion")+
  theme(plot.title=element_text(size=7))+
  geom_text(aes(label=prop),hjust = -0.008, size = 2)+
  scale_y_continuous(breaks=seq(0,1,0.02))


prop_r <- a%>%
  filter(hotel=="Resort Hotel")%>%
  ggplot(aes(x=reorder(Country,prop),y=prop))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  theme_minimal()+
  labs(title = "Repeated guest proportion of Resort Hotel from July 2015 to August 2017",
       x = "",
       y="proportion")+
  theme(plot.title=element_text(size=7))+
  geom_text(aes(label=prop),hjust = -0.008, size = 2)+
  scale_y_continuous(breaks=seq(0,1,0.2))




## ----Q12_1--------------------------------------------------------------------------
library(lubridate)
plot_interval<- final%>%
  filter(is_canceled==1)%>%
  select(hotel,is_canceled,reservation_status,reservation_status_date,arrival_time)%>%
  filter(reservation_status=="Canceled")%>%
  mutate(reservation_status_date=ymd(reservation_status_date),
         arrival_time=ymd(arrival_time))%>%
  mutate(interval=arrival_time-reservation_status_date)

interval <- plot_interval%>%
  ggplot(aes(x=interval))+
  geom_bar()+
  scale_x_continuous(breaks=seq(0,600,50))+
  theme(axis.text.x=element_text(angle=90))+
  labs(x="interval(day)")+
  facet_wrap(~hotel)


## ----Q12_2--------------------------------------------------------------------------
interval_c <- plot_interval%>%
  filter(hotel=="City Hotel")%>%
  ggplot(aes(x = interval,
             y = ..density..)) +
  geom_density(fill = "white") +
  geom_vline(xintercept = mean(plot_interval$interval),
             colour = "red")+
  labs(x="interval(day)")

interval_r <- plot_interval%>%
  filter(hotel=="Resort Hotel")%>%
  ggplot(aes(x = interval,
             y = ..density..)) +
  geom_density(fill = "white") +
  geom_vline(xintercept = mean(plot_interval$interval),
             colour = "red")+
  labs(x="interval(day)")




booking <- final %>% dplyr::select(lead_time,
                                   adr,
                                   is_canceled,
                                   reserved_room_type,
                                   assigned_room_type,
                                   arr_date_month, Country) %>%
  filter(is_canceled == 0)



type <- booking %>%
  filter(!reserved_room_type == assigned_room_type) %>%
  group_by(lead_time, arr_date_month,reserved_room_type,assigned_room_type, Country) %>%
  count( name = "Number")



interval_c <- plot_interval%>%
  filter(hotel=="City Hotel")

interval_r <- plot_interval%>%
  filter(hotel=="Resort Hotel")

plot_combined <- plot_interval%>%
  ggplot(aes(interval,fill=as.factor(hotel)))+
  geom_density(alpha=0.3)+
  geom_vline(xintercept = mean(interval_c$interval),
             colour = "red")+
  geom_vline(xintercept = mean(interval_r$interval),
             colour = "blue")+
  labs(x="interval(day)")

plot_combined
