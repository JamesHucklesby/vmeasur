#' #' Find peaks in a vascular time series
#' #'
#' #' @param input_vector vector of values to analyze
#' #' @param kband K smoothing window to apply to the data
#' #' @param nups number of increases before and after the dataset to threshold on
#' #' @param min_change minimum size of change to be termed significant
#' #'
#' #' @importFrom stats ksmooth time
#' #' @importFrom pracma findpeaks
#' #' @importFrom graphics grid points
#' #' @importFrom magrittr %>%
#' #' @importFrom dplyr mutate
#' #'
#' #'
#' #'
#' #' @return A vector of peaks detected
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' i = 1
#' #' # Test to come
#' #'
#' find_peaks = function(input_vector, kband = 20, nups = 10, min_change = 0, min_dist = 10)
#' {
#'
#' smooth_vector = ksmooth(time(1:length(input_vector)), input_vector, 'normal', bandwidth = kband)$y
#'
#' x = findpeaks(smooth_vector, nups = 0, ndowns = nups, zero = "+", minpeakdistance = min_dist)
#' y = findpeaks(-smooth_vector, nups = 0, ndowns = nups, zero = "+", minpeakdistance = min_dist)
#'
#' if(!isTRUE(nrow(x)>1 & nrow(y)>1))
#' {
#'   return(NULL)
#' }
#'
#' y[,1] = -y[,1]
#'
#' # # Not run:
#' # plot(smooth_vector, type="l", col="navy")
#' # grid()
#' # points(x[, 2], x[, 1], pch=20, col="maroon")
#' # points(y[, 2], y[, 1], pch=20, col="darkgreen")
#'
#'
#' if(isTRUE(nrow(x)>=1 & nrow(y)>=1))
#' {
#' maxima = data.frame(event_start = x[,2],event_end = x[,4],type = "contract")
#' minima = data.frame(event_start = y[,2],event_end = y[,4],type = "fill")
#' events = rbind(maxima, minima)
#' }else
#' {
#'   return(NULL)
#' }
#'
#' events$start_value = smooth_vector[events$event_start]
#' events$end_value = smooth_vector[events$event_end]
#'
#' events = events %>% mutate(`event_change` = `end_value` - `start_value`,
#'                            `event_duration` = `event_end` - `event_start`,
#'                            `event_gradient` = `event_change`/`event_duration`)
#'
#' raw_events = events
#'
#' #events = subset(events, !events$event_end == length(input_vector) & !events$event_start == 1)
#'
#' events = events %>% filter(abs(event_change)>min_change) %>% filter(event_duration>min_dist)
#'
#' if(nrow(events) ==0)
#' {
#'   return(output)
#' }
#'
#' function_plot = ggplot() + geom_line(aes(x = c(1:length(smooth_vector)), y = input_vector, color = "a"), color = "grey") +
#'   geom_line(aes(x = c(1:length(smooth_vector)), y = smooth_vector), color = "violetred4") +
#'   geom_rect(data = events, aes(xmin=event_start, xmax=event_end, ymin=0, ymax=Inf, fill = type), alpha = 0.4) +
#'   scale_y_continuous(breaks = c(1:20)) +
#'   geom_point(data = raw_events, aes(x = event_start, y = start_value), color = "green") +
#'   geom_point(data = raw_events, aes(x = event_end, y = end_value), color = "red", shape = 4)
#'
#'
#' # function_plot = ggplot() + geom_line(aes(x = c(1:length(smooth_vector)), y = input_vector, color = "a"), color = "grey") +
#' #   geom_line(aes(x = c(1:length(smooth_vector)), y = smooth_vector), color = "violetred4") +
#' #   geom_line(aes(x = c(1:length(smooth_vector)), y = c(diff(smooth_vector),NA)*100, color = "blue"))+
#' #   geom_line(aes(x = c(1:length(smooth_vector)), y = c(diff(diff(smooth_vector)*10),NA,NA)*100, color = "green"))
#'
#'
#' plot(function_plot)
#'
#'
#' return(events)
#' }
#'
#'
#'
#'
#' #' find peaks at a single y row
#' #'
#' #' @param ylocation y co-ordinate to analyse
#' #' @param datum raw dataset
#' #' @param ... further arguments to be passed through to find_peaks
#' #'
#' #' @importFrom magrittr %>%
#' #' @importFrom dplyr filter
#' #'
#' #'
#' #' @return A find_peaks result
#' #'
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' # Test here
#' #'
#' #'
#' find_peaks_y = function(ylocation, datum, ...)
#' {
#'
#'   datum_mini = datum %>% filter(`y` == ylocation)
#'
#'   raw_vector = datum_mini$p_width
#'
#'   toreturn = find_peaks(raw_vector, ...)
#'
#'   toreturn$y_location = ylocation
#'
#'   return(toreturn)
#'
#' }
#'
#'
#' # Legacy code for detecton of peaks
#'
#' # events = data.frame(location = c(y[,2],x[,2]), value = c(y[,1],x[,1]))
#' #
#' # next_highest(value = allcontractions, vector = allcontractions)
#' #
#' #
#' # peakquant = data.frame(event_start = allcontractions, event_end = next_highest(value = allcontractions, vector = allcontractions))
#' # peakquant$event_duration = peakquant$event_end - peakquant$event_start
#' #
#' # peakquant = subset(peakquant, peakquant$event_duration<Inf)
#' #
#' #
#' #
#' # next_highest = function(value, vector)
#' # {
#' #   if(length(value)>1)
#' #   {
#' #     result = lapply(value, next_highest, vector)
#' #     return(unlist(result))
#' #   }
#' #
#' #   svector = subset(vector, vector>value)
#' #   return(min(svector))
#' # }
#' #
#' #
#' #
#' #
#'
#'
