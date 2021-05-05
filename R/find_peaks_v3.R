#' Find peaks in a vascular time series
#'
#' @param input_vector vector of values to analyze
#' @param kband K smoothing window to apply to the data
#' @param nups number of increases before and after the dataset to threshold on
#' @param min_change minimum size of change to be termed significant
#'
#' @importFrom stats ksmooth time
#' @importFrom pracma findpeaks
#' @importFrom graphics grid points
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#'
#'
#' @return A vector of peaks detected
#'
#' @export
#'
#' @examples
#' i = 1
#' # Test to come
#'
find_peaks = function(input_vector, kband = 30, nups = 10, min_change = 0.25, min_dist = 10)
{

  smooth_vector = ksmooth(time(1:length(input_vector)), input_vector, 'normal', bandwidth = kband)$y
  inverted_vector = 0-smooth_vector

  x = findpeaks(inverted_vector, nups = nups, ndowns = nups, zero = "+", minpeakdistance = min_dist)

  # Not run:
  # plot(smooth_vector, type="l", col="navy")
  # grid()
  # points(x[, 2], x[, 1], pch=20, col="maroon")
  #
  # return(x)

  if(!isTRUE(nrow(x)>1))
  {
    return(NULL)
  }



  if(isTRUE(nrow(x)>=1))
  {
    events = data.frame(event_maxima = x[,2], event_start = x[,3],event_end = x[,4],type = "contract")
  }else
  {
    return(NULL)
  }

  events$start_value = smooth_vector[events$event_start]
  events$end_value = smooth_vector[events$event_end]
  events$max_value = smooth_vector[events$event_maxima]

  events = events %>% rowwise() %>%
                      mutate(
                             `baseline_change` = (`start_value`-`max_value`),
                             `event_duration` = `event_end` - `event_start`,
                             `event_gradient` = `baseline_change`/`event_duration`)

  raw_events = events

  #events = subset(events, !events$event_end == length(input_vector) & !events$event_start == 1)

  events = events %>% filter(abs(baseline_change)>min_change) %>% filter(event_duration>min_dist)

  if(nrow(events) ==0)
  {
    return(NULL)
  }



  # function_plot = ggplot() + geom_line(aes(x = c(1:length(smooth_vector)), y = input_vector, color = "a"), color = "grey") +
  #   geom_line(aes(x = c(1:length(smooth_vector)), y = smooth_vector), color = "violetred4") +
  #   geom_point(data = raw_events, aes(x = event_start, y = start_value), color = "red", size = 4) +
  #   geom_point(data = raw_events, aes(x = event_end, y = end_value), color = "blue", size = 2) +
  #   geom_point(data = raw_events, aes(x = event_maxima, y = max_value), color = "orange", size = 4)+
  #   geom_point(data = events, aes(x = event_maxima, y = max_value), color = "green", size = 2)
  #
  #
  # plot(function_plot)


  return(events)
}

