#'@title Run a Gadget
#'@importFrom shiny selectInput sliderInput verbatimTextOutput plotOutput icon h3 renderPlot renderPrint stopApp observeEvent runGadget
#'@importFrom miniUI gadgetTitleBar miniPage miniTabstripPanel miniTabPanel miniContentPanel
#'@importFrom ggplot2 ggplot aes geom_smooth geom_point
#'@importFrom dplyr filter between select
#'@importFrom stats na.omit
#'@importFrom rlang sym
#'
#'@param dd Input data set
#'@param outcomes List of outcomes
#'@param predictors List of predictors

myGadgetFunc <- function(dd, outcomes, predictors) {
  ui <- miniPage(gadgetTitleBar("health insurance"),
                 miniTabstripPanel(
                   miniTabPanel(
                     "Parameters",
                     icon = icon("sliders"),
                     miniContentPanel(
                       selectInput(
                         "country",
                         label = "select country",
                         choices = country_choices,
                         selected = c("Saudi Arabia", "United States", "United Kingdom"),
                         multiple = TRUE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL
                       ),
                       sliderInput(
                         "year",
                         label = h3("year"),
                         min = 1960,
                         max = 2020,
                         value = c(1960, 2020)
                       ),
                       selectInput(
                         "outcome",
                         label = "select an outcome",
                         choices = outcomes,
                         selected = outcomes[2],
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL

                       ),
                       selectInput(
                         "predictor",
                         label = "select a predictor",
                         choices = predictors,
                         selected = predictors[1],
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL
                       ))),
                   miniTabPanel(
                     "compare series",
                     icon = icon("area-chart"),
                     miniContentPanel(plotOutput("chartA", height = "100%"))
                   ),
                   miniTabPanel(
                     "regression analysis",
                     icon = icon("area-chart"),
                     miniContentPanel(verbatimTextOutput("chartD"))
                   )
                 )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.

    output$chartA<-     renderPlot({
      dd %>%
        filter(country %in% input$country) %>%
        filter(between(year, input$year[[1]], input$year[[2]])) %>%
        select(year, country, input$outcome, input$predictor) %>%
        na.omit() %>%
        ggplot(aes(
          x = year,
          y = !!sym(input$predictor)
          ,
          color = country
        )) +
        geom_smooth() +
        geom_point()
    })

    output$chartD <-  renderPrint({
      formula1 <-
        rlang::new_formula(sym(input$outcome), sym(input$predictor))
      #formula1
      linearMod <- lm(formula1, data = dd)
      summary(linearMod)
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <-
        stopApp(returnValue)
    })
  }

  runGadget(ui, server)
}

myGadgetFunc <- function(dd, outcomes, predictors) {
  ui <- miniPage(gadgetTitleBar("health insurance"),
                 miniTabstripPanel(
                   miniTabPanel(
                     "Parameters",
                     icon = icon("sliders"),
                     miniContentPanel(
                       selectInput(
                         "country",
                         label = "select country",
                         choices = alharbi::country_choices,
                         selected = c("Saudi Arabia", "United States", "United Kingdom"),
                         multiple = TRUE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL
                       ),
                       sliderInput(
                         "year",
                         label = h3("year"),
                         min = 1960,
                         max = 2020,
                         value = c(1960, 2020)
                       ),
                       selectInput(
                         "outcome",
                         label = "select an outcome",
                         choices = outcomes,
                         selected = outcomes[2],
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL

                       ),
                       selectInput(
                         "predictor",
                         label = "select a predictor",
                         choices = predictors,
                         selected = predictors[1],
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL
                       ))),
                   miniTabPanel(
                     "compare series",
                     icon = icon("area-chart"),
                     miniContentPanel(plotOutput("chartA", height = "100%"))
                   ),
                   miniTabPanel(
                     "regression analysis",
                     icon = icon("area-chart"),
                     miniContentPanel(verbatimTextOutput("chartD"))
                   )
                 )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.

    output$chartA<-     renderPlot({
      dd %>%
        filter(country %in% input$country) %>%
        filter(between(year, input$year[[1]], input$year[[2]])) %>%
        select(year, country, input$outcome, input$predictor) %>%
        na.omit() %>%
        ggplot(aes(
          x = year,
          y = !!sym(input$predictor)
          ,
          color = country
        )) +
        geom_smooth() +
        geom_point()
    })

    output$chartD <-  renderPrint({
      formula1 <-
        rlang::new_formula(sym(input$outcome), sym(input$predictor))
      #formula1
      linearMod <- lm(formula1, data = dd)
      summary(linearMod)
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <-
        stopApp(returnValue)
    })
  }

  runGadget(ui, server)
}
