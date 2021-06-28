# library(zoo)
#
# y = dataum2$mean
#
#
# find_peaks = function(y, w = 20, t = 0.25, returngraph = FALSE)
# {
#
#   # baseline = find_baseline(y)
#   # y = y/baseline
#
#   x = c(1:length(y))
#
#   dat = data.frame( x = x , y = y)
#
#   dat$ksmooth_box = ksmooth(time(dat$x), dat$y, 'normal', bandwidth = 100)$y
#   dat$ksmooth_box_res = dat$y - dat$ksmooth_box
#   dat$ksmooth_box_res_smooth = ksmooth(time(dat$x), dat$ksmooth_box_res, 'normal', bandwidth = 20)$y
#
#   dat$ksmooth = ksmooth(time(dat$x), dat$y, 'normal', bandwidth = 20)$y
#   ggplotly(ggplot(dat) + geom_line(aes(y = ksmooth, x = x, color = "ksmoothed raw")) + geom_line(aes(y = y, x = x, color = "raw")))
#
#   ggplot(dat) + geom_line(aes(y = ksmooth_box_res, x = x, color = "ksmoothed raw"))
#
#   ggplot(dat) + geom_line(aes(y = ksmooth_box_res_smooth, x = x, color = "ksmoothed raw"))
#
#   t = sd(dat$ksmooth_box_res_smooth)*t
#
#   dat$input = dat$y/mean(dat$y)
#
#   # Calculate rolling variables
#
#   dat$max <- rollapply(dat$input, w, max, align="center", fill = NA)
#   dat$min =  rollapply(dat$input, w, min, align="center", fill = NA)
#   dat$mean <- rollapply(dat$input, w, mean, align="center", fill = NA)
#
#   dat = dat %>% fill(max, min, .direction  = "up") %>% fill(max, min, .direction  = "down")
#
#   ggplot(dat) + geom_line(aes(y = input, x = x, color = "Baseline")) +
#     geom_line(aes(y = max, x = x, color = "RollMax")) +
#     geom_line(aes(y = min, x = x, color = "RollMin"))
#
#   # Find full length plateaus
#   smalltable = table(dat$min)
#   smalltable = as.data.frame(smalltable)
#   minima = subset(smalltable, smalltable$Freq == w)
#
#   smalltable = table(dat$max)
#   smalltable = as.data.frame(smalltable)
#   maxima = subset(smalltable, smalltable$Freq == w)
#
#
#   # Calculate threshold values
#
#   lowthreshold = -t
#   highthreshold = t
#
#   dat$isminpoint = dat$input %in% minima$Var1
#   dat$ismaxpoint = dat$input %in% maxima$Var1
#
#   dat$isbelowthreshold =  dat$input < lowthreshold
#   dat$isabovethreshold =  dat$input > highthreshold
#
#   dat$iscontractedminima = (dat$isminpoint + dat$isbelowthreshold) ==2
#   dat$iscontractedmaxima = (dat$ismaxpoint + dat$isabovethreshold)==2
#
#   low_lines = subset(dat, dat$iscontractedminima)$x
#   high_lines = subset(dat, dat$iscontractedmaxima)$x
#
#   graph = ggplot(dat) + geom_line(aes(y = input, x = x, color = "Normalised data")) +
#     geom_hline(yintercept = lowthreshold) + geom_hline(yintercept = highthreshold) +
#     geom_line(aes(x = x, y = max, color = "Roling maxima"))+
#     geom_line(aes(x = x, y = min, color = "Roling minima"))+
#     geom_vline(xintercept = low_lines, colour = "blue")+
#     geom_vline(xintercept = high_lines, colour = "orange")
#
#   graph
#
#
#   library(pracma)
#   findpeaks(dat$y)
#
#   keypoints = dat %>% filter(isminpoint | ismaxpoint)
#
#
#   if(returngraph)
#   {
#     return(graph)
#   }
#
#   return(max(length(low_lines), length(high_lines)))
#
# }
#
#
#
# find_baseline = function(y)
# {
#
#   return(median(y))
#
#   d <- density(y, bw = "sj")
#   # plot(d)
#   baseline = d$x[which.max(d$y)]
#   return(baseline)
# }
#
#
