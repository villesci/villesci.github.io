## Shared helpers for the intertidal fieldwork calendar.
## Used by both friday_harbor_tides.R and the Shiny app (app.R).

suppressPackageStartupMessages({
  library(rtide)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(glue)
  library(hms)
})

ALL_DAYS <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Tile order within a calendar row: Monday AM ... Sunday PM
SLOT_LEVELS <- as.vector(t(outer(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                 c(" AM", " PM"), paste0)))

#' Find workable low-tide sessions.
#' @param height_max highest tide (in the units of `units`) we are willing to work in
#' @param min_session minimum session length, minutes
#' @param units "m" or "ft"; rtide returns metres, so height_max is converted internally
find_sessions <- function(station, from, to, tz,
                          start_time = "00:00:00", end_time = "23:59:59",
                          work_days = ALL_DAYS,
                          height_max = 0.25, min_session = 30, units = "m") {
  tides <- rtide::tide_height(
    stations = station, minutes = 6L,
    from = as.Date(from), to = as.Date(to), tz = tz,
    harmonics = rtide::harmonics
  )

  # Group by tide cycle (not date) so two low tides in a day are kept separate:
  # a new cycle starts at every local maximum (high tide).
  th <- tides$TideHeight
  is_high <- c(FALSE, diff(sign(diff(th))) == -2, FALSE)
  tides$tide_cycle <- cumsum(is_high)

  limit_m <- if (units == "ft") height_max * 0.3048 else height_max
  tod <- hms::as_hms(format(tides$DateTime, "%H:%M:%S"))

  keep <- tod >= hms::as_hms(start_time) & tod <= hms::as_hms(end_time) &
    weekdays(tides$DateTime) %in% work_days &
    tides$TideHeight <= limit_m

  out <- tides[keep, ] %>%
    group_by(tide_cycle) %>%
    summarise(
      total_mins      = as.numeric(difftime(max(DateTime), min(DateTime), units = "mins")),
      min_ht          = min(TideHeight),
      low_tide_time   = DateTime[which.min(TideHeight)],
      fieldwork_start = min(DateTime),
      fieldwork_end   = max(DateTime),
      .groups = "drop"
    ) %>%
    filter(total_mins > min_session)

  if (nrow(out) == 0) return(out)

  if (units == "ft") out$min_ht <- out$min_ht / 0.3048
  out$units <- units
  out$height_max <- height_max

  # Calendar placement. The row is the week *within the month* (Monday-start).
  # Using lubridate::week() here was the bug: it counts 7-day blocks from Jan 1,
  # so days from different calendar weeks (e.g. Jul 2-3 and Jul 8) shared a row.
  d <- as.Date(out$low_tide_time, tz = tz)
  out %>% mutate(
    date       = d,
    month_lab  = factor(format(d, "%B %Y"),
                        levels = unique(format(sort(unique(d)), "%B %Y"))),
    week_row   = as.integer((floor_date(d, "week", week_start = 1) -
                               floor_date(floor_date(d, "month"), "week", week_start = 1)) / 7) + 1L,
    wday_slot  = factor(paste0(as.character(wday(d, label = TRUE, abbr = TRUE, week_start = 1,
                                                 locale = "C")),
                               ifelse(hour(low_tide_time) < 12, " AM", " PM")),
                        levels = SLOT_LEVELS)
  )
}

#' Calendar-style tile plot for one or more months of `sessions`.
plot_tide_calendar <- function(sessions, station, ncol = 1) {
  u <- sessions$units[1]
  # white text on the dark (low) end of the viridis fill, black on the light end
  sessions$txt <- ifelse(scales::rescale(sessions$min_ht) < 0.45, "white", "black")
  ggplot(sessions, aes(x = wday_slot, y = week_row, fill = min_ht,
                       group = paste(date, format(low_tide_time, "%H:%M")))) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_viridis_c(na.value = "transparent") +
    scale_x_discrete(drop = FALSE) +
    scale_color_identity() +
    scale_y_reverse(breaks = NULL) +
    facet_wrap(~ month_lab, scales = "free_y", ncol = ncol) +
    # Four stacked lines per tile (offsets are in tile-height units; y axis is reversed)
    geom_text(aes(color = txt, y = week_row - 0.30, label = day(low_tide_time)), size = 3.2, fontface = "bold") +
    geom_text(aes(color = txt, y = week_row - 0.10, label = paste0(round(total_mins), " min")), size = 3.2) +
    geom_text(aes(color = txt, y = week_row + 0.10, label = format(low_tide_time, "%H:%M")), size = 3.2) +
    geom_text(aes(color = txt, y = week_row + 0.30, label = paste0(round(min_ht, 2), " ", u)), size = 3.2) +
    labs(
      x = NULL, y = NULL,
      fill = glue("Min tide\nheight ({u})"),
      title = glue("{station}: workable low tides"),
      subtitle = glue("Tide below {sessions$height_max[1]} {u}; sessions > minimum length. ",
                      "Tile: day, minutes workable, time and height of low tide.")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "gray10"),
      panel.background = element_rect(fill = "gray96", color = NA),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      panel.spacing = unit(1.5, "lines"),
      strip.background = element_rect(fill = "gray20", color = NA),
      strip.text = element_text(color = "white", face = "bold", size = 11,
                                margin = margin(5, 0, 5, 0)),
      plot.title = element_text(face = "bold", size = 14, color = "gray10"),
      plot.subtitle = element_text(size = 9, color = "gray40", margin = margin(0, 0, 12, 0))
    )
}

#' Write one PDF per month into `dir`; returns the file paths.
write_month_pdfs <- function(sessions, station, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  vapply(levels(droplevels(sessions$month_lab)), function(m) {
    f <- file.path(dir, paste0("tides_", gsub(" ", "_", m), ".pdf"))
    p <- plot_tide_calendar(filter(sessions, month_lab == m), station)
    pdf(f, width = 11, height = 8.5)
    print(p)
    dev.off()
    f
  }, character(1))
}

#' Build .ics text. Times are written as floating local times (no TZID), as before.
build_ics <- function(sessions, station, location = station) {
  ev <- vapply(seq_len(nrow(sessions)), function(i) {
    s <- sessions[i, ]
    category <- dplyr::case_when(
      s$min_ht < -1 ~ "Red", s$min_ht < 0 ~ "Orange", s$min_ht < 0.5 ~ "Yellow", TRUE ~ "Blue"
    )
    as.character(glue(
      "BEGIN:VEVENT
UID:tide-{format(s$fieldwork_start, '%Y%m%dT%H%M%S')}@villesci.github.io
DTSTAMP:{format(Sys.time(), '%Y%m%dT%H%M%SZ', tz = 'UTC')}
DTSTART:{format(s$fieldwork_start, '%Y%m%dT%H%M%S')}
DTEND:{format(s$fieldwork_end, '%Y%m%dT%H%M%S')}
SUMMARY:Fieldwork - Low Tide Monitoring
DESCRIPTION:Low tide reached {round(s$min_ht, 2)} {s$units}. Workable for {round(s$total_mins)} minutes. Event start and stop mark when the tide drops below or rises above {s$height_max} {s$units}. Predictions from {station} (rtide harmonics; not for navigation).
LOCATION:{location}
CATEGORIES:{category}
END:VEVENT"))
  }, character(1))
  c("BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//villesci.github.io//tide-calendar//EN", ev,
    "END:VCALENDAR")
}
