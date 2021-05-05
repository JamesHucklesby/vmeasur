function(){

library(tidyverse)


# Plot out all the data


roi_list = summary.df %>% select(site, animal, treatment) %>% distinct()


pdf("plots.pdf", onefile = TRUE, width = 12, height = 9)

for (i in 1:3)
{

  print(i)

  roi = roi_list[i,]

  fulldata_mean_mini = summary.df %>% filter(site == roi$site, animal == roi$animal, treatment == roi$treatment)

  video_shift = (fulldata_mean_mini %>% group_by(source_video) %>% summarise(video_shift = max(frame_id)))$video_shift

  theplot = ggplot(fulldata_mean_mini) +
    geom_line(aes(x = frame_id, y = p_mean, color = vessel)) +
    geom_vline(xintercept = video_shift) +
    labs(title = (paste(i, ")" , roi$treatment, roi$animal, " S",roi$site, sep = "")))

  grid.arrange(theplot)

}

dev.off()



pdf("plots_smooth.pdf", onefile = TRUE, width = 12, height = 9)

for (i in 1:nrow(roi_list))
{

  print(i)

  roi = roi_list[i,]

  fulldata_mean_mini = summary.df %>% filter(site == roi$site, animal == roi$animal, treatment == roi$treatment)


  video_shift = (fulldata_mean_mini %>% group_by(source_video) %>% summarise(video_shift = max(frame_id)))$video_shift

  theplot = ggplot(fulldata_mean_mini) +
    geom_line(aes(x = frame_id, y = smooth, color = vessel)) +
    geom_vline(xintercept = video_shift) +
    labs(title = (paste(i, ")" , roi$treatment, roi$animal, " S",roi$site, sep = "")))

  grid.arrange(theplot)

}

dev.off()





pdf("plots_ecg.pdf", onefile = TRUE, width = 9, height = 12)

for (i in 1:nrow(roi_list))
{

  print(i)

  roi = roi_list[i,]

  fulldata_mean_mini = summary.df %>% filter(site == roi$site, animal == roi$animal, treatment == roi$treatment)


  video_shift = (fulldata_mean_mini %>% group_by(source_video) %>% summarise(video_shift = max(frame_id)))$video_shift

  theplot = ggplot(fulldata_mean_mini) +
    geom_line(aes(x = frame_id, y = smooth, color = vessel)) +
    labs(title = (paste(i, ")" , roi$treatment, roi$animal, " S",roi$site, sep = ""))) +
    facet_wrap(vars(vessel), ncol = 1, scales = "free_y")+
    geom_vline(xintercept = video_shift)

  grid.arrange(theplot)

}

dev.off()



trace_list = summary.df %>% select(source_video, vessel, animal, treatment, site) %>% distinct()



trace_list = trace_list[sample(c(1:nrow(trace_list)),100),]

pdf("plots_points.pdf", onefile = TRUE, width = 9, height = 12)

result = foreach(i = 1:nrow(trace_list)) %do%
  {
    print(paste(i, "of", nrow(trace_list)))
    traced = trace_list[i,]
    trace = summary.df %>% filter(source_video == traced$source_video, vessel == traced$vessel)


    if(sum(trace$p_mean)>5 && length(trace$p_mean)>10)
    {
      resulting = find_peaks(input_vector =trace$p_mean, min_change = 0)
      resulting$source_video = traced$source_video
      resulting$vessel = traced$vessel
      resulting$animal = traced$animal
      resulting$treatment = traced$treatment
      resulting$site = traced$site
      return(resulting)
    }

  }

contractions = bind_rows(result)

dev.off()

unique(contractions$type)
minic = contractions %>% filter(abs(event_change)>1)

minic$treatment = str_replace(minic$treatment, "Ap", "AP")


ggplot(minic) + geom_density(aes(x = event_change, color = treatment))
ggplot(minic) + geom_density(aes(x = event_duration, color = treatment))
ggplot(minic) + geom_density(aes(x = event_gradient, color = treatment))

minics = minic %>% group_by(animal, site, treatment, vessel, type) %>%
  summarise(event_change_mean = median(abs(event_change)), nevents = n())

ggplot(minics) + geom_point(aes(x = treatment, y = event_change_mean, color = type))
ggplot(minics) + geom_boxplot(aes(x = treatment, y = nevents, color = type))

minicss = minics %>% group_by(animal, site, treatment, type) %>%
  summarise(event_change_mean = median(event_change_mean), nevents = mean(nevents))

ggplot(minicss) + geom_point(aes(x = treatment, y = event_change_mean, color = type))
ggplot(minicss) + geom_boxplot(aes(x = treatment, y = event_change_mean, color = type))
ggplot(minicss) + geom_boxplot(aes(x = treatment, y = nevents, color = type))

minicsss = minicss %>% group_by(animal, treatment, type) %>%
  summarise(event_change_mean = median(event_change_mean), nevents = mean(nevents))

ggplot(minicsss) + geom_point(aes(x = treatment, y = event_change_mean, color = type))
ggplot(minicsss) + geom_boxplot(aes(x = treatment, y = event_change_mean, color = type))

ggplot(minicsss) + geom_boxplot(aes(x = treatment, y = nevents, color = type))








# fulldata_mean = fulldata_mean %>% filter(source_video == "image034")

mean_mini_demo = filter(mean_mini, vessel == 1.2)

find_peaks(input_vector = mean_mini_demo$p_width)



# Bits of stuff for single file quant


find_peaks_y(100,datum)
find_peaks_y(200,datum)


datum_groups = datumgood

datum_groups$grouping = cut(datum_groups$y, 20)

datum_groups = datum_groups %>% group_by(grouping, frame) %>% summarise(p_width = mean(p_width, na.rm = TRUE), y = mean(y))

datum_groups = subset(datum_groups, !is.nan(datum_groups$p_width))

ggplot(datum_groups) + geom_line(aes(x = frame, y = p_width, color = grouping))

output = lapply(unique(datum_groups$y), find_peaks_y, datum_groups)

outputsummary = bind_rows(output, .id = "column_label")

outputsummary$column_label = as.numeric(outputsummary$column_label)

ggplotly(ggplot(outputsummary) + geom_point(aes(x = event_start, y = y_location)))


unique(outputsummary$event_start)

}
