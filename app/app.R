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
    menuItem(text = "Authors",
             tabName = "autohors",
             icon = icon("user-friends")),
    menuItem(text = "About",
             tabName = "about",
             icon = icon("question"))
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
      tabBox(tabPanel("Motivation", ""),
             tabPanel("Research Questions"),
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
    tabItem(tabName = "about",
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

}

shinyApp(ui, server)


