ggplot2::ggplot(datasets::airquality) +
  ggplot2::geom_line(stat = "identity", ggplot2::aes(x = lubridate::make_date(lubridate::year(lubridate::today()), Month, Day), y = Wind)) +
