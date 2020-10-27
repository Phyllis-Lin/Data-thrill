library(shinydashboard)
library(shiny)
library(plotly)

source("data.R")

header <- dashboardHeader(title = "Hotel Bookings",
  dropdownMenu(
    type = "messages",
    messageItem(
      from = HTML('<span style="color:maroon;font-size:17px;">Data Thrills</span>'),
      message =  HTML('<span style="color:green;font-size:14px;">Code available on this GitHub repo:</span>'),
      href = "https://github.com/Phyllis-Lin/Data-thrill",
      icon =  icon(name = "github",
                   class = "fa-3x fa-pull-left")
    )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem (text = "Topic",
              tabName = "dashboard",
              icon = icon(name = "home")
    ),
    menuItem(text = "Methodology",
             tabName = "inputs",
             icon = icon("sitemap")),

    menuItem(text = "Data Wrangling",
             tabName = "wrangle",
             icon = icon("tools")),
    menuItem(text = "Analysis",
             tabName = "analysis",
             icon = icon("chart-bar")),
    menuItem(text = "Authors",
             tabName = "autohors",
             icon = icon("user-friends")),
    menuItem(text = "About",
             tabName = "about",
             icon = icon("question")),
    menuItem(text = "Reproducibility",
             tabName = "repro",
             icon = icon("redo" ,"fa-spin")),
    menuItem(text = "References",
             tabName = "refer",
             icon = icon("book"))
  )
)




body <- dashboardBody(
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "my_style.css"
    )
  ),
  # Create a tabBox
  tabItems(
    tabItem(
      tabName = "dashboard",
      tabBox(tabPanel("Motivation", "",),
             tabPanel("Research Questions",
                      fluidPage(
                        fluidRow(width = 12,
                                 HTML('<ul>
  <li>Which day of the month is most common and which season (or month) has more
customers in the whole year?</li>
  <li>Comparing the most reserved season by different hotel types with different countries.</li>
  <li>How many nights for booking and which day in week is most popular with customer type
and adult, children and babies.</li>
<li>How the duration of your expected stay in the hotel and the number of people affect the
probability of canceling the booking.</li>
<li>Is the variable average day price influenced by the number of the day you stay and the
type of the room you reserved?</li>
<li>Which countries visit Portugal more often and how the number of bookings from that
countries changes over time </li>
<li>Which countries cancel their booking more often</li>
<li>Which country’s people most like to repeat to reserve these hotels?</li>
<li>How soon travellers book the hotel?</li>
<li> Which hotel is more expensive for the same type of
rooms and in which months?</li>
<li>What is the probability to cancel your booking if you have already cancelled before?</li>
<li>How close to the arrival data is the booking being cancelled?</li>
<li>The percentage of booking and the cancellation customer &amp; the new customer rate and
regular customer rate.</li>
</ul>')

                        )
                      )

                      ),
             tabPanel("Dataset", ""),
        tabPanel("Source", "",
                 fluidPage(
                   fluidRow(
                     tags$iframe(
                       src = 'https://www.sciencedirect.com/science/article/pii/S2352340918315191#f0010',
                       width = '150%', height = '800px',
                       frameborder = 0, scrolling = 'auto'
                     )
                   ),
                 ))



      )
    ),
    tabItem(tabName = "inputs",
            fluidRow(
              box(
                width = 10,
                title = "Cleaning Process",
                "Mpla mpal mpla "
                )
              )
            ),
tabItem(tabName = "wrangle",
        fluidRow(
          box(
            width = 10,
            title = "Data cleaning",
            h4("Likely, the dataset used in this analysis was in a very tidy format already, so only minor adjustments were required to get the data in a more convenient format.  We used the geocode package to extract the full name of the countries from their iso3c code. Furthermore, we used the `naniar` package to identify the percentage of missing values in our dataset. In addition, we used `lubridate` to convert some of our variables to Date objects. Luckily, the missing values were a very small proportion of our data, so we decided to remove them."),
            br()))),
    tabItem(tabName = "analysis",
    navbarPage(footer =  HTML('<span style="color:black;font-size: 20px;
                            font-weight:bold;">
                            <a href="https://mida.numbat.space" target="_blank">
                            ETC5510: Introduction to Data Analysis</a><span>'),

                       tags$style(HTML("
        .navbar { background-color: pink;}
        .navbar-default .navbar-nav > li > a {color:navy;}")
                       ),

        tabPanel("Hotel Analysis", icon = icon("hotel"),
                 tags$div("In this segment of our analysis we explore the datset to answer
                          questions that provide valuable insight to hotel owners. Emphasis is
                          given in identifiying the clinet's profiles that choose between the City and
                          the Resort Hotel. Moreover, we derive the country of origin of
                          these guests so that hotel owners can gain an insight on where they should target their
                          marketing campaigns. Finally, a valuable piece of information for hotel owners
                          would be to know the probability of a guest to cancel its booking.
                          We will attempt to answer this on the following sections of this
                          analysis.",
                          style = "color:black;font-size: 18px;font-weight:normal;"
                 ),
                 br(),
                 fluidRow(
                 infoBox(
                   value = n_distinct(final$country),
                   title = "Number of countries in the dataset",
                   icon = icon("globe"),
                   color = "purple"
                 ),
                 infoBox(
                   value = 38886,
                   title = "Bookings data about the Resort Hotel",
                   icon = icon("umbrella-beach"),
                   color = "purple"
                 ),
                 infoBox(
                   value = 78730,
                   title = "Bookings data about the City Hotel",
                   icon = icon("hotel"),
                   color = "purple"
                 ),
                 infoBox(
                   value = paste(41, "%"),
                   title = "Portugals Proportion in the total bookings",
                   icon = icon("exclamation"),
                   color = "purple"
                 )
                 ),
                 br(),
                 fluidRow(
                   column(width = 12,
                          tags$div("At first we provide relative information about from which
                                   countries the majority of the customers comes from.",
                                   style = "color:black;font-size: 18px;font-weight:normal;")
                          ),
                   br(),
                   br(),
                   br(),
                   fluidRow(
                   column(width = 6,
                          plotOutput("resort", width = "700px", height = "600px")
                          ),
                   column(width = 6,
                          plotOutput("city", width = "700px", height = "600px")
                   )
                   ),
                   br(),
                   tags$div("As we can see in both types of hotels the majority of customers
                            comes from within Portugal. It's also important to understand which type of hotel
                            customers from different countries seem to prefer. For example, while customers from UK are
                            responsible for around 20% of the bookings in the resort hotel the same number for the city
                            hotel is les than 10%.",
                            style = "color:black;font-size: 18px;font-weight:normal;"),
                   br(),
                   tags$div("For both types of hotels, bookings from within Portugal amount for
                            more than 40% of their total business. The rest of the countries that
                            visit hotels in Portugal more frequently are the following:",
                            style = "color:black;font-size: 18px;font-weight:normal;"),
                 ),
                     fluidRow(width = 12,
                     plotOutput("visitors", width = "900px", height = "700px")
                     ),

                 br(),
                 fluidRow(
                   column(width = 12,
                   tags$div("Moving on, we shift our focus on from which countries customers are more prone to cancel their
                            booking. The below choropleth map indicates countries with high number of
                            cancelations. Custoemr from countries with darker color have historically canceld a larger proportion of their bookings
                            , than those in green",
                            style = "color:black;font-size: 18px;font-weight:normal;"),
                 ),
                 fluidRow(width = 12,
                        leafletOutput("map", height = "700px")
                        ),
                 br(),
                 fluidRow(width = 12,
                          column(width = 4,
                                 box(
                                   selectInput("country",
                                               "Country:",
                                               c("All",
                                                 unique(as.character(type$Country))),
                                               selected = "China"),
                                   selectInput("mon",
                                               "Arrive date month:",
                                               c("All",
                                                 unique(as.character(type$arr_date_month))),
                                               selected = "10")
                                 )
                          ),
                          column(width = 8,
                                 plotOutput("polar", width = "800px", height = "700px")
                          )),
                 ),
                 br(),
                 tags$div("Finally, we implemented a logistic regression model to find the
                          probability of a customer canceling its booking based on the
                          expected length of stay.",
                          style = "color:black;font-size: 18px;font-weight:normal;"),
                 fluidRow(width = 6,
                          plotOutput("log", height = "500px", width = "500px")
                          ),
                 br(),
                 tags$div("Based on the graph the more days the customer is expected to stay
                          at the hotel the higher are the chances that the booking is goint to
                          get canceled.",
                          style = "color:black;font-size: 18px;font-weight:normal;"),
                 fluidPage(
                   fluidRow(
                     tags$div("It can be seen from this figture that no matter what kind of hotel, the number of people staying in two is the most, followed by the hotel where one person stays, and the number of people staying in a family is the least.
Therefore, it is recommended that the hotel, in terms of room type arrangements, should be mainly double rooms or couple rooms. Secondly, arrange single suites. This can maximize the hotel's space utilization and maximize revenue.",
style = "color:black;font-size: 20px;font-weight:normal;"),
br(),
plotOutput("density",height = "600px"),
br(),
tags$div("The picture describes how long people tend to book a hotel in advance. The blue line is his average value (one hundred days in advance). The picture shows that hotel guests are mostly temporary hotels, while customers in city hotels Book more in advance.
Therefore, our suggestion is that for resort hotels, they should be more inclined to offline promotion, such as large physical billboards, luxurious exterior decorations, and shiny lights.
For city hotels, online advertising should be promoted, because people book more in advance, so online advertising and some coupons can attract customers more accurately.",
style = "color:black;font-size: 20px;font-weight:normal;"),
plotOutput("density2"),

br(),
tags$div("This graph shows the frequency of people staying in hotels on weekends and weekdays. On weekdays, more people choose to stay for 1 to 3 nights. The average number is 2 nights. On weekends, people also choose to stay for 1 night. The number of people is more, followed by 2 nights. All in all, the proportion of customers renewing on Sunday is 2/1, while the proportion of customers renewing on weekdays is 2/5.
Therefore, we recommend that hotels do more promotional activities during workdays to increase hotel occupancy rates and renewal rates during workdays.",
style = "color:black;font-size: 20px;font-weight:normal;"),
plotOutput("density3"))
                 ),
br(),
fluidPage(
  fluidRow(
    tags$div("From the bar plots and density plot, people would clearly know that how close to the arrival data that customers have the most probability to cancel their current booking.",
             style = "color:black;font-size: 20px;font-weight:normal;"),
    br(),
    tags$div("From the bar plots, it clearly shows that both of the two hotels have the similar distribution about the interval which represents the day between the original arrival day and the reservation cancelled day. People could find that to the City Hotel, most of the interval is between 0 to 270 days while to the Resort Hotel, that is between 0 to 150 days. ",
             style = "color:black;font-size: 20px;font-weight:normal;"),
    br(),
    plotOutput("int"),
    tags$div("The density plot shows a similar right skewed shape. The red line represents the average interval days of the City Hotel and the blue line represents the average interval days of the Resort Hotel. So it may help people to seize the opportunity to book this two hotels again 80 to 90 days before they want to stay if they do not reserve rooms before.",
             style = "color:black;font-size: 20px;font-weight:normal;"),
    br(),
    plotOutput("int2"))
)

        ),
        tabPanel("Customer Analysis",
                 fluidRow(
                   column(width = 12,
                          tags$div("When I should book my holidays ?",style = "color:#2a752e;font-size: 25px;font-weight:normal;"),
                          HTML('<img src="https://media2.giphy.com/media/CM2G0NiOzjWjC/200w.gif?cid=ecf05e47kgciljmy76rnvv9kto6p8oygzx1koyz7dtq37ya1&rid=200w.gif" alt="Italian Trulli" width="300" height="200">')
                   ),
                   br(),
                   fluidPage(
                   fluidRow(
                     tags$div("In this part of our analysis we explore the datset from a customer's point of view.
                              As so, we will try to find patterns that can help customers book their
                              dream holidays. We begin by oferring an overview of when you would expect to find
                            yourself in a hotel full of people or settle for a more relaxed
                            time. For those that avoid crowds at all costs aim at days in yellow color.
                            The following is a colored-calendar based on our historical data indicating the busiest
                            days of the month for each availabe year.
                            Based on the figure we can conclude thah for both hotels the period between
                            April and August has the most days with above average occupancy.
                            Finally, on the opposite side, January was the month where both hotels experienced
                            fewer bookings than usual.",
                            style = "color:black;font-size: 18px;font-weight:normal;"),
                   ),
                   br(),
                   column(offset = 1, width = 6,
                   plotOutput("busy", width = "1000px", height = "900px")
                   )
                   ),
                   br(),
                   tags$div("After establishing when a cusotmer might want to plan her holidays based on her preferences, it's time to secure
                            a cheap bargain regarding accomodation.",
                            style = "color:#2a752e;font-size: 19px;font-weight:normal;"),
                   br(),
                   tags$div("In the following interactive figure the user can select her conditions and based
on those the calendar will indicate days at which customers with simillar conditions
managed to book a room at a below than average price.
Although this pattern might not hold true in the future, it can still reveal days where the hotels announce
discounts as part of a marketing campaign. With such information the customers can be aware on when to
check the hote's website for possible promotions.",
                            style = "color:black;font-size: 18px;font-weight:normal;"),
                   br(),
tags$div("Note that if all days are in green it means that your conditions did not match
         with simillar conditions in our dataset and as so there is no ideal day that somebody had
         previously managed a cheaper booking.",
style = "color:black;font-size: 18px;font-weight:bold;"),
                   column(width = 4,
                          box(
                            selectInput("cal_month", "Select Month",
                                        choices = sort(unique(final$arrival_date_month)),
                                        selected = "August"),
                            selectInput("cal_adults", "Select number of adults", choices = c(1:10),
                                        selected = 1),
                            sliderInput("cal_nights", "Select Stay Length", min = 0, max = 100, value = 3),
                            selectInput("cal_child", "Select number of children/babies", choices = c(0:10),
                                        selected = 0),
                            selectInput("cal_baby", "Select number of babies", choices = c(0:10),
                                        selected = 0)
                          )
                          ),
                   column(width = 8,
                   plotOutput("cheap", width = "800px", height = "700px")
                   )
                   ),
                 br(),
                 tags$div("Managing to secure cheap accommodation is undoubtedly a huge succes, but what if you arrive in a hotel full
of crying children. Based on the type of holidays that the customer is looking for such aspect might
seriously tamper her mood.",
                          style = "color:#2a752e;font-size: 19px;font-weight:normal;"),
                 column(width = 6,
                 HTML('<img src="https://media2.giphy.com/media/10tIjpzIu8fe0/giphy.gif?cid=ecf05e47c9r4pxlsy283ca3716a1cxi5hq6lnsrpee4v2vsa&rid=giphy.gif">'
                      )
                 ),
tags$div("
As so we have created this interactive figure, where the user can select the
month and the hotel type and a figure indicates the proportion of age groups
that occupied the hotel the previous years.",
style = "color:black;font-size: 18px;font-weight:normal;"),
fluidRow(
                 box(
                   selectInput("child_month", "Select Month",
                               choices = sort(unique(final$arrival_date_month)),
                               selected = "January"),
                   selectInput("child_hotel", "Select Hotel",
                               choices = sort(unique(final$hotel)),
                               selected = "Resort Hotel")
                 ),
                 column(width = 6,
                 plotOutput("picto", width = "600px", height = "700px")
                 )
),
fluidPage(
  fluidRow(
    tags$div("It can be seen from this chart that no matter what kind of hotel it is, April, August, and December are the peak periods for hotels.
For hotels, we recommend that more people be hired during this period to cope with the peak of passenger flow, so as not to reduce service quality, so as to bring a good experience to customers and increase the image of the hotel.
For customers, we recommend avoiding travel during these three months, and being able to travel off peaks and enjoy better hotel services..",
style = "color:black;font-size: 20px;font-weight:normal;"),
br(),
plotOutput("density4"))),
fluidPage(
  fluidRow(
    tags$div("People can find which country’s people most like to repeat to reserve the same hotel, the City Hotel or the Resort Hotel when they come to Portugal from the bar plots. They show the top 10 rank about the proportion of repeated guests for the Resort Hotel and the City Hotel from 2015 to 2017. It is obvious that the proportion of the Resort Hotel was much higher than that of the City Hotel.",
             style = "color:black;font-size: 20px;font-weight:normal;"),
    br(),
    tags$div("The main reason for that may have something to do with the location of this two hotels. The City Hotel is located in the center of the country and there are more hotels in that place, so there are more options for customers to pick one hotel they like, while the Resort Hotel is located in the suburb, and there are not many options for customers to choose different hotels. So the proportion of repeated customers are higher in the Resort Hotel.",
             style = "color:black;font-size: 20px;font-weight:normal;"),
    plotOutput("grid"))),
br(),
tags$div("Click each day's rectangle to show details",
         style = "color:black;font-size: 20px;font-weight:normal;"
),
br(),
fluidRow(
  plotlyOutput("day"),
  tags$div("In 2016, 26th every month have more customers and 26th every month have more customers as we can see from the plot",
           style = "color:black;font-size: 20px;font-weight:normal;"
  ),
  br(),
  plotOutput("month_book"),
  tags$div("October has most arrival customers in whole year, then is April and May. City hotel reserved much more than resort hotel.",
           style = "color:black;font-size: 20px;font-weight:normal;"
  ),
  br(),
  plotOutput("month_country"),
  tags$div("We choose Portugal and United Kingdom which are top two countries of booking and compared with Australia and China found that different countries ‘customer prefer to go these hotels in different seasons. For example, Chinese people prefer to go on February, September and October because Spring Festival in February and National Day in 1st October which are two biggest vacation in China. Portuguese people go on April, May and June most my because the Easter Day holiday. As we can see there is almost zero customer reserve resort hotel from Australia in December. Because during that time Australia is summer and around with pacific Oceans, citizens can enjoy the resort hotels in their country. Furthermore, we can found that English people prefer reserve resort hotel and Chinese people more like city hotel.",
           style = "color:black;font-size: 20px;font-weight:normal;"
  )
)

),




        inverse = TRUE)),

tabItem(tabName = "about",
        fluidRow(
          box(
            width = 10,
            title = "About our project",
            "This project is jointly completed by our team data thrills. The project analyzed hotel data from tidy Tuesday.",
            br(),
            "This report mainly analyzes from two aspects. On the one hand, it is from the user's perspective. For example, it is better to provide suggestions to customers when it is better to check in, and to book the time in advance. On the other hand, it analyzes from the perspective of the hotel, such as the peak of hotel occupancy, room type reservation, daily activity, customer source, etc., to provide suggestions for the hotel.",
            br(),
            h5("Built with",
               img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
               "by",
               img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
               ".")
          ))),

    tabItem(tabName = "repro",
            fluidPage(
              tags$iframe(
                src = './about.html',
                          width = '100%', height = '800px',
                          frameborder = 0, scrolling = 'auto'
              )
            )
    ),
tabItem(tabName = "refer",
        fluidPage(
          tags$iframe(
            src = './citations.html',
            width = '100%', height = '800px',
            frameborder = 0, scrolling = 'auto'
          )
        )
)

  )
)



ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin = "purple"
)


server <- function(input, output) {


  output$resort <- renderPlot(
    resort_prop %>%
      ggplot(aes(fct_reorder(country, hotel_booking_prop), hotel_booking_prop )) +
      background_image(resort) +
      geom_segment(aes(xend=country, yend=0), color="#5D7783", lwd = 2) +
      geom_point( size = 4, color="#DC493D", shape =19) +
      coord_flip() +
      ggthemes::theme_solarized() +
      theme(
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15),
        plot.title.position = "plot",
        axis.text.y = element_text(
          face = "italic",
          colour = "black",
          size = 16
        ),
        axis.text.x = element_text(
          face = "italic",
          colour = "black",
          size = 16
        ),
        title = element_text(colour = "black",
                             size = 18),
        axis.title.x.bottom = element_text(colour = "black",
                                           size = 18)
      ) +
      labs(
        title = "Proportion of Bookings by Top-10 Countries fot the Resort Hotel",
        y = "Proportion"
      )

  )

  output$city <- renderPlot(
    city_prop %>%
    ggplot(aes(fct_reorder(country, hotel_booking_prop), hotel_booking_prop )) +
    background_image(city) +
    geom_segment( aes(xend=country, yend=0), color="#5D7783", lwd = 1.6) +
    geom_point( size=5, color="#DC493D", shape =19, ) +
    coord_flip() +
      ggthemes::theme_solarized() +
      theme(
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15),
        plot.title.position = "plot",
        axis.text.y = element_text(
          face = "italic",
          colour = "black",
          size = 16
        ),
        axis.text.x = element_text(
          face = "italic",
          colour = "black",
          size = 16
        ),
        title = element_text(colour = "black",
                              size = 18),
        axis.title.x.bottom = element_text(colour = "black",
                                           size = 18)
      ) +
    labs(
      y = "Proportion of City Hotel Bookings",
      title = "Proportion of Bookingsby Top-10 Countries for the City Hotel"
      )
  )


  output$visitors <- renderPlot(

    final %>%
      mutate(Country = as.factor(Country)) %>%
      filter(Country != "Portugal") %>%
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
      ggflags::geom_flag(y = -1 ,aes(country = code), size = 15, show.legend = FALSE) +
      scale_country() +
      ggthemes::theme_solarized_2(light = FALSE) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 15),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_textbox_simple(
          size = 16,
          width = NULL,
          orientation = "left-rotated",
          padding = margin(4, 4, 4, 4),
          margin = margin(4, 0, 0, 0),
          linetype = 1,
          r = grid::unit(8, "pt"),
          fill = "azure1",
        ),
        legend.title = element_textbox_simple(
          size = 16,
          width = NULL,
          padding = margin(4, 4, 4, 4),
          margin = margin(4, 0, 0, 0),
          linetype = 1,
          r = grid::unit(8, "pt"),
          fill = "azure1",
        ),
        plot.title = element_markdown(face = "bold",
                                      size = 20),
        plot.subtitle = element_markdown(size = 18),
        plot.title.position = "plot",
        panel.background = element_blank(),
        legend.text=element_text(size=17)
      ) +
      scale_fill_distiller() +
      labs(
        x = "",
        y = "Total Bookings",
        title = "Top - 10 Countries by number of bookings",
        subtitle = "The fill of the columns indicates the proportion of bookings
    that was **not** canceled")





  )



  output$map <- renderLeaflet(


    map
  )


  output$busy <- renderPlot({

    p1 <- visitors %>%
      filter(hotel == "Resort Hotel") %>%
      ggplot_calendar_heatmap(
        'Date',
        'above_average',
      ) +
      scale_fill_continuous(low = '#fffba1', high = '#83497f') +
      facet_wrap(~arrival_date_year, ncol = 1) +
      labs(
        title = 'Days with above average occupancy for the Resort Hotel',
        subtitle = "The purple color indicates days with above the average occupancy",
        x = "",
        y = ""
      ) +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#effaf1"),
        axis.text.y = element_markdown(size = 8,
                                       face = "italic"),
        axis.text.x = element_markdown(size = 12,
                                       face = "italic"),
        plot.title.position = "plot",
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12)
      )



    p2 <-   visitors %>%
      filter(hotel == "City Hotel") %>%
      ggplot_calendar_heatmap(
        'Date',
        'above_average',
      ) +
      xlab(NULL) +
      ylab(NULL) +
      scale_fill_continuous(low = '#fffba1', high = '#83497f') +
      facet_wrap(~arrival_date_year, ncol = 1) +
      labs(
        title = 'Days with above average occupancy for the City hotel',
        subtitle = "The purple color indicates days with above the average occupancy",
        x = "",
        y = ""
      ) +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#effaf1"),
        axis.text.y = element_markdown(size = 8,
                                       face = "italic"),
        axis.text.x = element_markdown(size = 12,
                                       face = "italic"),
        plot.title.position = "plot",
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12)
      )



    p1 / p2




  })


  output$cheap <- renderPlot({

    book_day <- final %>%
      filter(arrival_date_month == input$cal_month) %>%
      filter(children == input$cal_child & babies == input$cal_baby) %>%
      filter(adults == input$cal_adults) %>%
      filter(stays_in_week_nights == input$cal_nights) %>%
      select(reservation_status_date, adr) %>%
      arrange((adr)) %>%
      filter(adr != 0) %>%
      head(3) %>%
      as_tibble() %>%
      mutate(day = as.Date(reservation_status_date) - as.Date(as.character("2017-01-01"), format="%Y-%m-%d")) %>%
      filter(day > 0)

      calendar <- calendR(year = 2021,
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


    calendar
})


output$picto <- renderPlot({

  final %>%
    count(hotel, arrival_date_month, adults, children, babies) %>%
    pivot_longer(cols = c("adults", "children", "babies"),
                 names_to = "group",
                 values_to = "number") %>%
    group_by(hotel, arrival_date_month, group) %>%
    summarise(total  = sum(number)) %>%
    filter(arrival_date_month == input$child_month &
           hotel == input$child_hotel) %>%
    arrange(group) %>%
    ggplot(aes(label = group, values = total)) +
    geom_pictogram(n_rows = 10, aes(colour = group), flip = TRUE, make_proportional = TRUE) +
    scale_color_manual(
      name = NULL,
      values = c("darkgreen", "pink", "orange"),
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
    theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75)) +
    theme(
      panel.background = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(
      title = paste("Proportion of Age Groups in", input$child_month, "for",  input$child_hotel)
    )



})



output$log <- renderPlot(

  log
)

output$day <- renderPlotly(


  ggplotly(g1)
)


output$month_book <- renderPlot(

  month_book
)


output$month_country <- renderPlot(

  month_country
)


output$grid <- renderPlot(

  grid.arrange(prop_c,prop_r,ncol=2)
)


output$int <- renderPlot(

 interval
)


output$int2 <- renderPlot(

  plot_combined
)




output$density <- renderPlot(
  grid.arrange(q2, q1, ncol = 2)
)

output$density2 <-  renderPlot(

  q3
)


output$density3 <-  renderPlot(

  grid.arrange(n, n1, ncol = 1)

)

output$density4 <-  renderPlot(

  grid.arrange(mr, mc, ncol = 1)

)

output$polar <- renderPlot({
  p <- type %>% filter( arr_date_month == input$mon,
                        Country == input$country) %>%
    ggplot(aes(x= reserved_room_type , y = log(Number*2), fill = assigned_room_type)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    coord_polar() +
    facet_wrap(~arr_date_month)
  p
})


}

shinyApp(ui, server)


