function()
{


csv_files = list.files("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2",
                       recursive = TRUE, pattern = "\\_widths.csv$", full.names = TRUE)


csv_files = csv_files[(295-3):295]

fulldata_mean = fulldata %>% filter(!excluded) %>% group_by(frame, source_video, vessel, site, animal, treatment) %>% summarise(p_width = mean(p_width, na.rm = TRUE)) %>% ungroup()

vessels_done = unique(select(fulldata_mean,treatment, animal, site, source_video))

mean_mini = fulldata_mean %>% filter(source_video == unique(fulldata_mean$source_video)[1])

unique(select(mean_mini, treatment, animal, site, source_video))

ggplot(mean_mini) + geom_line(aes(x = frame, y = p_width, color = vessel))

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
