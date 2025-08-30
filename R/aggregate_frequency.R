#' Aggregate Time Series Data to a Target Frequency
#'
#' This function aggregates a time series raster (`SpatRaster` object) to a
#' specified target frequency using a user-defined aggregation function.
#'
#' @param x A `SpatRaster` object with a time dimension.
#' @param iteracation optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#' @param target_freq A character string specifying the target time frequency.
#'  The default for the format methods is `"%Y-%m-%d"`.
#' @param agg_fn A function to apply for aggregation (e.g., `mean`, `sum`,
#'   `max`, `min`).
#'
#' @return A `SpatRaster` object aggregated to the specified time frequency.
#'
#' @export
#'
#' @examples
#' # Create a sample SpatRaster with daily data
#' time_seq <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
#' r <- terra::rast(nrows=10, ncols=10, nlyrs=length(time_seq))
#' terra::time(r) <- time_seq
#'
#' # Aggregate to monthly means
#' r_monthly <- aggregate_frequency(r, target_freq = "%Y-%m", agg_fn = mean)

aggregate_frequency <- function(x,
                                iteracation = NULL,
                                target_freq = "%Y-%m-%d",
                                agg_fn = "mean") {
    if (!is.null(iteracation)) cat("aggregating:", iteracation, "\n")

    hour <- terra::time(x)
    if (all(is.na(hour))) {
        hour <- gsub("^t2m_valid_time=", "", x = names(x)) |>
            as.numeric() |>
            as.POSIXct(origin = "1970-01-01", tz = "GMT")
    }
    dates <- format(hour, target_freq)
    out <- terra::tapp(x, index = dates, fun = agg_fn)
    names(out) <- unique(dates)
    terra::time(out) <- lubridate::as_date(unique(dates), format = target_freq)
    return(out)
}
