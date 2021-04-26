function()
{


  testdat =import_files(c("P:\\Full Dataset 2\\SH22S1\\image034_SH22S1.1_1.1_overlaid.csv"))


csv_files = list.files("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2",
                       recursive = TRUE, pattern = "\\_widths.csv$", full.names = TRUE)


length(csv_files)

# Completed files
done = unique(basename(dirname(csv_files)))
todo = basename(list.dirs("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2"))

length(todo)
length(done)
length(done)/(length(todo)+length(done))

done_dirs = unique(dirname(csv_files))

# current_dir = "//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/SH22S1"

for(current_dir in done_dirs)
{

folder_files = list.files(current_dir, recursive = TRUE, pattern = "\\_widths.csv$", full.names = TRUE)

fulldata = import_all_folder(folder_files)

fulldata %>% group_by(source_video) %>% summarise(max = max(frame))

fulldata = fulldata %>% group_by(y, vessel, site, animal, treatment) %>% mutate(frame_id = row_number())

fulldata_mean = fulldata %>% filter(!excluded) %>%
  group_by(frame_id, frame,source_video, site, animal, treatment, vessel) %>%
  summarise(p_mean = mean(p_width, na.rm = TRUE), p_median = median(p_width, na.rm = TRUE))

write.csv(fulldata_mean, paste(current_dir, "/average.csv", sep = ""))
}


csv_files = list.files("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2",
                       recursive = TRUE, full.names = TRUE)

summary_csv = subset(csv_files,(str_count(csv_files, "average.csv")>0))

summary_data  = lapply(summary_csv, read.csv, as.is = TRUE)

rbind(summary_data[[1]],summary_data[[2]],summary_data[[3]])

summary.df = summary_data[[1]]

for(i in c(2:length(summary_data)))
{
  print(i)
  summary.df = rbind(summary.df, summary_data[[i]])
}

vessel_list = summary.df %>% select(vessel, source_video) %>% distinct()



# summary.df = summary.df %>% group_by(vessel, source_video) %>%
#   mutate(smooth = ksmooth(c(1:length(p_mean)), p_mean, kernel = "normal", bandwidth = 3, n.points= nrow(p_mean))$y) %>%
#   ungroup()

summary.df = summary.df %>% group_by(vessel, source_video) %>%
  mutate(smooth = rollmean(p_mean, 5, fill = NA)) %>%
  ungroup()


roi_list = summary.df %>% select(site, animal, treatment) %>% distinct()


pdf("plots.pdf", onefile = TRUE, width = 12, height = 9)

for (i in 1:nrow(roi_list))
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


ggplot(minic) + geom_freqpoly(aes(x = event_change, color = treatment))
ggplot(minic) + geom_density(aes(x = event_duration, color = treatment))
ggplot(minic) + geom_density(aes(x = event_gradient, color = treatment))

minics = contractions %>% group_by(animal, site, treatment, vessel, type) %>%
  summarise(event_change_mean = mean(abs(event_change)))

ggplot(minics) + geom_point(aes(x = treatment, y = event_change_mean, color = type))

minicss = minics %>% group_by(animal, site, treatment, type) %>%
  summarise(event_change_mean = mean(event_change_mean))

ggplot(minicss) + geom_point(aes(x = treatment, y = event_change_mean, color = type))

minicsss = minicss %>% group_by(animal, treatment, type) %>%
  summarise(event_change_mean = mean(event_change_mean))

ggplot(minicsss) + geom_point(aes(x = treatment, y = event_change_mean, color = type))
ggplot(minicsss) + geom_point(aes(x = treatment, y = event_change_mean, color = type))

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
