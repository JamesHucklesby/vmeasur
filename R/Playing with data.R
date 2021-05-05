
target_folder = choose.dir()

csv_files = list.files(target_folder, recursive = TRUE, pattern = "\\_widths.csv$", full.names = TRUE)

# Import all the data in a folder
fulldata = import_all_folder(csv_files)

# Check what ROI's exist
fulldata %>% select(treatment, animal, site, vessel) %>% distinct()

fulldata = fulldata %>% group_by(y, vessel, site, animal, treatment) %>% mutate(frame_id = row_number())

fulldata_mean = fulldata %>% filter(!excluded) %>%
  group_by(frame_id, frame,source_video, site, animal, treatment, vessel) %>%
  summarise(p_mean = mean(p_width, na.rm = TRUE), p_median = median(p_width, na.rm = TRUE))


video_shift = (fulldata_mean %>% group_by(source_video) %>% summarise(video_shift = max(frame_id)))$video_shift

ggplot(fulldata_mean) +
  geom_line(aes(x = frame_id, y = p_mean, color = vessel)) +
  geom_vline(xintercept = video_shift) +
  labs(title = paste(unique(fulldata_mean$treatment), roi$animal, " S",roi$site, sep = ""))

pixel_bin = 30

groupfull = fulldata %>% mutate(ygroup = ((y-1) %/% pixel_bin) + 1)  %>%
  group_by(treatment, animal, site, vessel, ygroup) %>%
  mutate(max = max(y), min = min(y), npix = max-min +1, trace = cur_group_id()) %>%
  filter(npix == pixel_bin) %>%
  ungroup()

groupfull %>% select(treatment, animal, site, vessel, ygroup, min, max, npix, trace) %>% distinct

fulldata_mean_group = groupfull %>% filter(!excluded) %>%
  group_by(frame_id, trace, treatment, animal, site, vessel, ygroup) %>%
  summarise(p_mean = mean(p_width, na.rm = TRUE), p_median = median(p_width, na.rm = TRUE))


fulldata_MGS = fulldata_mean_group %>% filter(frame_id<600) %>%
  group_by(trace) %>%
  mutate(p_mean_smooth = ksmooth(c(1:length(unique(frame_id))), p_mean, "normal", 10)$y)

ggplot(fulldata_MGS) +
  geom_line(aes(x = frame_id, y = p_mean_smooth, color = paste(vessel, "/", ygroup)))







vector_trace = subset(fulldata_MGS, trace == 1)$p_mean

find_peaks(vector_trace)

vec.df = data.frame(time = c(1:length(vector_trace)), p_mean = vector_trace)

vec.df$p_max = rollmax(vec.df$p_mean, 10, fill = NA)
vec.df$ksmooth2 = ksmooth(c(1:length(vector_trace)),vec.df$p_mean, "normal", 20)$y


ggplot(vec.df) +
  geom_line(aes(x = time, y = p_mean)) +
  geom_line(aes(x = time, y = ksmooth2, color = "smooth"))











