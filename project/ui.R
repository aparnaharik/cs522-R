library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("India Demography App",
                   tabPanel("Literacy",
                            
                            # Sidebar with a slider input for the number of bins
                            sidebarLayout(
                              sidebarPanel(
                                # Copy the line below to make a checkbox
                                checkboxInput("checkbox", label = "Tooltip on Hover", value = TRUE),
                                sliderInput("bins",
                                            "Year:",
                                            min = 1951,
                                            max = 2011,
                                            value = 1951,
                                            step = 10),
                                uiOutput("plot_ui")
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                ggvisOutput("distPlot")
                              )
                            )
                          ),
                    tabPanel("Social/Health Statistics",
                         
                         # Sidebar with a slider input for the number of bins
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("variable", "Demographic Factor:",
                                         c("Population" = "Population",
                                           "Exponential Growth Rate" = "Exponential.Growth.Rate",
                                           "Literacy Rate (Female 7+)" = "Literacy.Rate.Female.7.",
                                           "Sex Ratio (Female per 1000 Male)" = "Sex.Ratio",
                                           "Sex Ratio (0-6years)" = "Sex.Ratio.1",
                                           "Crude Birth Rate" = "Crude.Birth.Rate",
                                           "Crude Death Rate" = "Crude.Death.Rate",
                                           "Infant Mortality Rate" = "Infant.Mortality.Rate",
                                           "Under 5 Mortality Rate " = "Mortality.Rate.5.",
                                           "Maternal Maternity Ratio" = "Maternal.Maternity.Ratio",
                                           "Total Fertility Rate" = "Total.Fertility.Rate",
                                           "Mean age at effective marriage (Females)" = "Mean.Female.marriage.age"
                                           )
                                        )
                           ),
                           # Show a plot of the generated distribution
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Plot", plotOutput("statsPlot")),
                               tabPanel("Map", plotOutput("mapsPlot")),
                               tabPanel("Data", dataTableOutput("dataTable"))
                             )
                           )
                         )
                    ),
                   tabPanel("Ranking",
                            
                            # Sidebar with a slider input for the number of bins
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("choose_states")
                              ),
                              # Show a plot of the generated distribution
                              mainPanel(
                                dataTableOutput("rankTable")
                              )
                            )
                   )
            )
        )