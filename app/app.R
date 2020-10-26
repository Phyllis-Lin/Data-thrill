library(shinydashboard)
library(shiny)



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
             icon = icon("redo" ,"fa-spin"))
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
    tabItem(tabName = "wrangle"),
    tabItem(tabName = "analysis",
    navbarPage(footer =  HTML('<span style="color:black;font-size: 20px;
                            font-weight:bold;">
                            <a href="https://mida.numbat.space" target="_blank">
                            ETC5510: Introduction to Data Analysis</a><span>'),

                       tags$style(HTML("
        .navbar { background-color: pink;}
        .navbar-default .navbar-nav > li > a {color:navy;}
        table.dataTable  {color:black; font-size:18px; background-color:#ffecef}")
                       ),

        tabPanel("Hotel Analysis", icon = icon("hotel"),
                 tags$div("In this segment of our analysis we explore the datset to answer
                          questions that provide valuable insight to hotel owners. Emphasis is
                          given to identify the clinet's profile that choose between the City and
                          the Resort Hotel. Moreover, we explore the country of origin of
                          these guests so that hotel owners can identify where they should target thir
                          marketing campaigns. Finally, a valuable piece of information for hotel owners
                          would be to know the probability of a guest to cancel its booking.
                          We will attempt to answer this on the following sections of this
                          analysis.",
                          style = "color:black;font-size: 20px;font-weight:normal;"
                 ),
                 br(),
                 fluidRow(
                   column(width = 12,
                          tags$div("At first we provide relative information about which
                                   countries the majority of the customers comes from.",
                                   style = "color:black;font-size: 20px;font-weight:normal;")
                          ),
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
                            comes from within Portugal.",
                            style = "color:black;font-size: 20px;font-weight:normal;"),
                   br(),
                   tags$div("For both types of hotels bookings from within Portugal amount for
                            more than 40% of their total business. The rest of the countries that
                            visit hotels in Portugal more frequently are the following:",
                            style = "color:black;font-size: 20px;font-weight:normal;"),
                   fluidPage(
                     fluidRow(width = 12,
                     plotOutput("visitors", width = "800px", height = "500px")
                     )
                   )
                 ),
                 br(),
                 fluidRow(
                   column(width = 6,
                   tags$div("Finally, we shift our focus on from which countries customers are more prone to canceling their
                            booking. The below choropleth map indicates countries with high number of
                            cancelations.",
                            style = "color:black;font-size: 20px;font-weight:normal;"),
                 ),
                 fluidRow(width = 12,
                        leafletOutput("map", height = "500px")
                        )
                 ),
                 fluidRow(width = 12,
                          plotOutput("log", height = "600px")

                 )

        ),
        tabPanel("Customer Analysis",
                 fluidRow(
                   column(width = 12,
                          HTML('<h2>When I should book my holidays?<h2/>'),
                          HTML('<img src="https://media2.giphy.com/media/CM2G0NiOzjWjC/200w.gif?cid=ecf05e47kgciljmy76rnvv9kto6p8oygzx1koyz7dtq37ya1&rid=200w.gif" alt="Italian Trulli" width="300" height="200">')
                   ),
                   fluidPage(
                   fluidRow(
                     tags$div("In this part of our analysis we explore our datset from the point of view of
                              a customer. As so, we will try to find patterns that can help customers book their
                              dream holidays.",
                            style = "color:black;font-size: 20px;font-weight:normal;"),
                   ),
                   tags$div("We begin by oferring an overview of when you would expect to find
                            yourself in a hotel full of people or settle for some a relaxed
                            time. For those that avoid crowds at all costs aim at days in red color.
                            The following is colored-calendar based on our historical data indicating the busiest
                            days of the years.",
                            style = "color:black;font-size: 20px;font-weight:normal;"),
                   br(),
                   plotOutput("busy")
                   ),
                   br(),
                   tags$div("Now that you know when to plan your holidays it's time to find some
                            cheap accomodation.",
                            style = "color:black;font-size: 20px;font-weight:normal;"),
                   br(),
                   column(width = 4,
                          box(
                            selectInput("month", "Select Month", choices = sort(unique(hotel$arrival_date_month))),
                            selectInput("adults", "Select number of adults", choices = c(1:10)),
                            sliderInput("days", "Select Stay Length", min = 0, max = 100, value = 3),
                            selectInput("children", "Select number of children/babies", choices = c(1:10)),
                            selectInput("room", "Select Room Type", choices = sort(unique(hotel$reserved_room_type))),
                            selectInput("meal", "Select Meal Type", choices = sort(unique(hotel$meal))),

                          )
                          ),
                   column(width = 8,
                   plotOutput("cheap", width = "800px", height = "700px")
                   )
                   ),
                 br(),
                 tags$div("Cheap tickets is a huge succes, but what if you arrive in a hotel full
                          of crying children.",
                          style = "color:black;font-size: 20px;font-weight:normal;"),
                 column(width = 6,
                 HTML('<img src="https://media2.giphy.com/media/10tIjpzIu8fe0/giphy.gif?cid=ecf05e47c9r4pxlsy283ca3716a1cxi5hq6lnsrpee4v2vsa&rid=giphy.gif">'
                      ),
                 box(
                   selectInput("month", "Select Month", choices = sort(unique(hotel$arrival_date_month)))
                 )
                 ),
                 column(width = 6,
                 plotOutput("picto", width = "800px", height = "700px")
                 )

        ),
        tabPanel("yuqi",
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
        tabPanel("yiwen",
                 fluidRow(
                   plotOutput("grid"),
                   plotOutput("int"),
                   plotOutput("int2")

                   )

                 ),

        tabPanel("chenjie",
                 fluidRow(
                   plotOutput("density"),
                   plotOutput("density2"),
                   plotOutput("density3"),
                   plotOutput("density4")
                 )),




        inverse = TRUE)),
    tabItem(tabName = "repro",
            fluidPage(
              tags$iframe(
                src = './about.html',
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
        title = "Proportion of Bookings by Country fot the Resort Hotel",
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
      title = "Proportion of Bookings by Country fot the City Hotel"
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





  )



  output$map <- renderLeaflet(


    map
  )


  output$busy <- renderPlot({
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



  })



  output$cheap <- renderPlot({


    calendar


  })


output$picto <- renderPlot({

  pictogram

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

  grid.arrange(interval_c,interval_r,ncol=2)
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

}

shinyApp(ui, server)


