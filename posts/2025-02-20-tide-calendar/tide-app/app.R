library(shiny)
library(bslib)

source("tide_functions.R")

stations <- rtide::tide_stations()
tz_choices <- c("US/Pacific", "US/Mountain", "US/Central", "US/Eastern", "US/Alaska",
                "US/Hawaii", "America/Puerto_Rico", "UTC")

# Best-guess time zone from the state name at the end of the station string
guess_tz <- function(station) {
  st <- trimws(sub(".*,", "", station))
  if (st %in% c("Washington", "Oregon", "California")) "US/Pacific"
  else if (st == "Alaska") "US/Alaska"
  else if (st == "Hawaii") "US/Hawaii"
  else if (st %in% c("Texas", "Louisiana", "Mississippi", "Alabama")) "US/Central"
  else if (st == "Puerto Rico") "America/Puerto_Rico"
  else "US/Eastern"
}

ui <- page_sidebar(
  title = "Intertidal fieldwork calendar",
  sidebar = sidebar(
    width = 340,
    selectizeInput("station", "Tide station", choices = stations,
                   selected = "Friday Harbor, San Juan Island, San Juan Channel, Washington"),
    selectInput("tz", "Time zone", tz_choices, selected = "US/Pacific"),
    dateRangeInput("dates", "Date range", start = Sys.Date(), end = Sys.Date() + 120,
                   max = Sys.Date() + 730),
    checkboxGroupInput("days", "Days you can work", ALL_DAYS, selected = ALL_DAYS),
    sliderInput("hours", "Times of day you can work", min = 0, max = 24,
                value = c(0, 24), step = 0.5, post = " h"),
    radioButtons("units", "Height units", c("Meters" = "m", "Feet" = "ft"), inline = TRUE),
    numericInput("height", "Maximum tide height to work in", value = 0.25, step = 0.05),
    numericInput("minmins", "Minimum working period (minutes)", value = 30, min = 1, step = 5),
    actionButton("go", "Make calendar", class = "btn-primary w-100")
  ),
  uiOutput("msg"),
  uiOutput("month_picker"),
  plotOutput("cal", height = "650px"),
  layout_columns(
    downloadButton("dl_ics", "Download .ics"),
    downloadButton("dl_pdf", "Download this month (PDF)"),
    downloadButton("dl_zip", "All months (zip of PDFs)")
  ),
  p(class = "text-muted mt-3",
    "Predictions come from harmonic constants in the rtide package (same data as XTide) and are ",
    "NOT for navigation. Tip: create a blank calendar before importing the .ics.")
)

hhmmss <- function(h) {
  h <- min(h, 23.9999)
  sprintf("%02d:%02d:%02d", floor(h), floor((h %% 1) * 60), 0)
}

server <- function(input, output, session) {
  observeEvent(input$station, updateSelectInput(session, "tz", selected = guess_tz(input$station)))
  observeEvent(input$units, {
    updateNumericInput(session, "height", value = if (input$units == "ft") 1 else 0.25)
  }, ignoreInit = TRUE)

  sessions <- eventReactive(input$go, {
    validate(need(length(input$days) > 0, "Pick at least one day of the week."),
             need(diff(input$dates) >= 0 && diff(input$dates) <= 730, "Choose a range of up to 2 years."),
             need(input$hours[2] > input$hours[1], "End time must be after start time."))
    withProgress(message = "Predicting tides...", {
      find_sessions(
        station = input$station, from = input$dates[1], to = input$dates[2] + 1, tz = input$tz,
        start_time = hhmmss(input$hours[1]),
        end_time = if (input$hours[2] >= 24) "23:59:59" else hhmmss(input$hours[2]),
        work_days = input$days, height_max = input$height,
        min_session = input$minmins, units = input$units
      )
    })
  }, ignoreNULL = FALSE)

  output$msg <- renderUI({
    s <- sessions()
    if (nrow(s) == 0) div(class = "alert alert-warning", "No workable sessions with these settings.")
    else p(strong(nrow(s)), " workable sessions found.")
  })

  output$month_picker <- renderUI({
    s <- sessions(); req(nrow(s) > 0)
    selectInput("month", "Month", levels(droplevels(s$month_lab)))
  })

  month_data <- reactive({
    s <- sessions(); req(nrow(s) > 0, input$month)
    dplyr::filter(s, month_lab == input$month)
  })

  short_station <- reactive(sub(",.*", "", input$station))

  output$cal <- renderPlot(plot_tide_calendar(month_data(), short_station()), res = 96)

  output$dl_ics <- downloadHandler(
    filename = function() "fieldwork_schedule.ics",
    content = function(file) {
      writeLines(build_ics(sessions(), short_station(), input$station), file)
    }
  )

  output$dl_pdf <- downloadHandler(
    filename = function() paste0("tides_", gsub(" ", "_", input$month), ".pdf"),
    content = function(file) {
      pdf(file, width = 11, height = 8.5)
      print(plot_tide_calendar(month_data(), short_station()))
      dev.off()
    }
  )

  output$dl_zip <- downloadHandler(
    filename = function() "tide_calendars.zip",
    content = function(file) {
      d <- file.path(tempdir(), "pdfs"); unlink(d, recursive = TRUE)
      files <- write_month_pdfs(sessions(), short_station(), d)
      zip::zipr(file, files)
    }
  )
}

shinyApp(ui, server)
