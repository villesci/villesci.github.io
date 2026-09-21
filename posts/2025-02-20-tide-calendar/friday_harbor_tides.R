# Friday Harbor workable-tide calendar. Helpers live in tide-app/ so the Shiny app shares them.
source("posts/2025-02-20-tide-calendar/tide-app/tide_functions.R")

station <- "Friday Harbor, San Juan Island, San Juan Channel, Washington"

sessions <- find_sessions(
  station = station,
  from = "2026-09-21", to = "2027-09-15", tz = "US/Pacific",
  start_time = "00:00:00", end_time = "23:59:59",
  work_days = ALL_DAYS,
  height_max = 0.25, units = "m",   # rtide is metric
  min_session = 30
)
head(sessions)

# Quick look at one month (July 2027) to check day ordering
plot_tide_calendar(dplyr::filter(sessions, month_lab == "July 2027"), "Friday Harbor")

# One PDF per month
write_month_pdfs(sessions, "Friday Harbor", "posts/2025-02-20-tide-calendar/pdfs")

# .ics
writeLines(build_ics(sessions, "Friday Harbor", "Friday Harbor, WA"),
           "posts/2025-02-20-tide-calendar/fieldwork_schedule_friday2627.ics")
