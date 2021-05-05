function()
{



# Y banded ROI

roi_list = summary.df %>% select(site, animal, treatment, vessel) %>% distinct()


for(j in c(1:nrow(roi_list)))
{
  try({

  roi = roi_list[j,]
  print(paste(j, "of",nrow(roi_list), "-",ceiling(j/nrow(roi_list)*100),"%"))

# Find peaks locally

fulldata_mean_mini = summary.df %>% filter(site == roi$site, animal == roi$animal, vessel == roi$vessel, treatment == roi$treatment)


video_shift = (fulldata_mean_mini %>% group_by(source_video) %>% summarise(video_shift = max(frame_id)))$video_shift

ggplot(fulldata_mean_mini) +
  geom_line(aes(x = frame_id, y = smooth, color = paste(vessel, "/", ygroup))) +
  geom_vline(xintercept = video_shift) +
  labs(title = (paste(i, ")" , roi$treatment, roi$animal, " S",roi$site, sep = "")))


trace_table = fulldata_mean_mini %>% select(site, animal, vessel, treatment, source_video, ygroup) %>% distinct()
trace_table$id = c(1:nrow(trace_table))

res = list()

for(i in c(1:nrow(trace_table)))
{
  #print(paste(j,i))
  filters = trace_table[i,]
  local_data = fulldata_mean_mini %>% filter(site == filters$site, animal == filters$animal, vessel == filters$vessel, treatment == filters$treatment, source_video == filters$source_video, ygroup == filters$ygroup)
  output = find_peaks(input_vector = local_data$p_mean, min_dist = 30, kband = 30, min_change = 1, nups = 10)
  if(!is.null(output))
  {
    output$id = i
    res[[i]] = output
  }
}

res.df = bind_rows(res)

if(nrow(res.df)==0)
{
  return(TRUE)
}

full_trace_table = res.df %>% left_join(trace_table, by = c("id"))

full_trace_contractions = full_trace_table %>% group_by(site, animal, vessel, treatment, source_video, ygroup) %>%
      mutate(contractions = n())

ggplot(full_trace_contractions) + geom_tile(aes(x = source_video, y = as.character(ygroup), fill = as.character(contractions)))+
  geom_text(aes(label = contractions, x = source_video, y = ygroup)) +
  theme(axis.text.x = element_text(angle = 90))

combined_full_table = fulldata_mean_mini %>% left_join(full_trace_contractions) %>% mutate(event_maxima_id = event_maxima - frame + frame_id)


vessid = (paste(roi$treatment, roi$animal, "_S",roi$site, "_V", roi$vessel, sep = ""))

file_root = paste("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/quantification/",vessid, sep = "")

ggplot(combined_full_table) +
  geom_line(aes(x = frame_id, y = smooth, color = as.character(contractions)))+
  geom_vline(xintercept = video_shift) +
  geom_point(aes(x = event_maxima_id, y = max_value), color = "blue") +
  labs(title = vessid) +
  facet_wrap(~paste(vessel, "/", ygroup))

ggsave(file=paste(file_root, ".png", sep = ""), width = 297, height = 210, units = "mm")


full_table_summary = combined_full_table %>% group_by(source_video, site, animal, treatment, vessel) %>% summarise(max_cont = max(contractions,0, na.rm = TRUE), max_magnitude = max(baseline_change))

write.csv(full_table_summary, paste(file_root, "_summary.csv", sep = ""))
})
}


csv_files = list.files("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/quantification/",
                       recursive = TRUE, full.names = TRUE)

summary_csv = subset(csv_files,(str_count(csv_files, ".csv")>0))

summary_data  = lapply(summary_csv, read.csv, as.is = TRUE)

summary.df = summary_data[[1]]

for(i in c(2:length(summary_data)))
{
  print(i)
  summary.df = rbind(summary.df, summary_data[[i]])
}

sum_clean = summary.df %>% mutate(treatment = str_replace(treatment, "Ap", "AP"))%>%
  mutate(treatment = str_replace(treatment, "#FOS", "FOS"))

ggplot(sum_clean) + geom_boxplot(aes(y = max_cont, x = treatment))

summ2.df = sum_clean %>% group_by(site, animal, treatment, vessel, source_video) %>%
  summarise(total_cont = max(max_magnitude))

ggplot(summ2.df) + geom_boxplot(aes(y = total_cont, x = treatment))

summ2.5.df = summ2.df %>% group_by(site, animal, treatment, vessel) %>%
  summarise(total_cont = max(total_cont,0))

ggplot(summ2.5.df) + geom_boxplot(aes(y = total_cont, x = treatment))

summ3.df = summ2.5.df %>% group_by(site, animal, treatment) %>%
  summarise(total_cont = mean(total_cont, na.rm = TRUE))

ggplot(summ3.df) + geom_boxplot(aes(y = total_cont, x = treatment))

summ4.df = summ3.df %>% group_by(animal, treatment) %>%
  summarise(total_cont = mean(total_cont, na.rm = TRUE))

ggplot(summ4.df) + geom_boxplot(aes(y = total_cont, x = treatment)) +
  geom_point(aes(y = total_cont, x = treatment), color = "blue") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(y = "Average Contractions", x = "Treatment")

ggplot(summ4.df) + geom_point(aes(y = total_cont, x = treatment)) +
  geom_point(aes(y = total_cont, x = treatment), color = "blue") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(y = "Average Contractions", x = "Treatment")


# Compute the analysis of variance
res.aov <- aov(total_cont ~ treatment, data = summ4.df)
# Summary of the analysis
summary(res.aov)

TukeyHSD(res.aov)


}
