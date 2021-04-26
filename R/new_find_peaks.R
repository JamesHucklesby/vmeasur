#' Title
#'
#' @param input_vector
#' @param kband
#' @param nups
#' @param min_change
#'
#' @return
#' @export
#'
#' @examples
find_peaks = function(input_vector, kband = 30, nups = 10, min_change = 1)
{

smooth_vector = ksmooth(time(1:length(input_vector)), input_vector, 'normal', bandwidth = kband)$y

x = findpeaks(smooth_vector, nups = nups, ndown = nups, zero = "+")
y = findpeaks(-smooth_vector, nups = nups, ndown = nups, zero = "+")

if(!isTRUE(nrow(x)>1 & nrow(y)>1))
{
  return(NULL)
}

y[,1] = -y[,1]

# Not run:
plot(smooth_vector, type="l", col="navy")
grid()
points(x[, 2], x[, 1], pch=20, col="maroon")
points(y[, 2], y[, 1], pch=20, col="darkgreen")

if(isTRUE(nrow(x)>1 & nrow(y)>1))
{
maxima = data.frame(event_start = x[,2],event_end = x[,4],type = "contract")
minima = data.frame(event_start = y[,2],event_end = y[,4],type = "fill")
events = rbind(maxima, minima)
}else
{
  return(NULL)
}

events$start_value = smooth_vector[events$event_start]
events$end_value = smooth_vector[events$event_end]

events = subset(events, !events$event_end == length(input_vector))

events = events %>% mutate(event_change = end_value - start_value,
                  event_duration = event_end - event_start,
                  event_gradient = event_change/event_duration)


return(events)
}




#' Title
#'
#' @param ylocation
#' @param datum
#'
#' @return
#' @export
#'
#' @examples
find_peaks_y = function(ylocation, datum, ...)
{

  datum_mini = datum %>% filter(y == ylocation)

  raw_vector = datum_mini$p_width

  toreturn = find_peaks(raw_vector, ...)

  toreturn$y_location = ylocation

  return(toreturn)

}


# Legacy code for detecton of peaks

# events = data.frame(location = c(y[,2],x[,2]), value = c(y[,1],x[,1]))
#
# next_highest(value = allcontractions, vector = allcontractions)
#
#
# peakquant = data.frame(event_start = allcontractions, event_end = next_highest(value = allcontractions, vector = allcontractions))
# peakquant$event_duration = peakquant$event_end - peakquant$event_start
#
# peakquant = subset(peakquant, peakquant$event_duration<Inf)
#
#
#
# next_highest = function(value, vector)
# {
#   if(length(value)>1)
#   {
#     result = lapply(value, next_highest, vector)
#     return(unlist(result))
#   }
#
#   svector = subset(vector, vector>value)
#   return(min(svector))
# }
#
#
#
#


