# # Clean up the issues in data entry on Peter's end
# sum_clean_full = summary.df %>% mutate(treatment = str_replace(treatment, "Ap", "AP"))%>%
#   mutate(treatment = str_replace(treatment, "#FOS", "FOS")) %>% mutate(animal = ifelse(animal == 1, 19, animal))
#
# # Assign serial id numbers to animals
# sum_clean_full = sum_clean_full %>% group_by(treatment, animal) %>% mutate(animal_id = cur_group_id()) %>% group_by(treatment) %>% mutate(animal_id = 1+ animal_id - min(animal_id))
#

unique(summary_cont.df$site)


# Clean up the issues in data entry on Peter's end
sum_clean = summary_cont.df %>% mutate(treatment = str_replace(treatment, "Ap", "AP"))%>%
  mutate(treatment = str_replace(treatment, "#FOS", "FOS")) %>% mutate(animal = ifelse(animal ==1, 19, animal))

sum_clean = sum_clean %>%
  mutate(treatment = str_replace(treatment, "AP", "AP")) %>%
  mutate(treatment = str_replace(treatment, "FOP", "AP + AFR")) %>%
  mutate(treatment = str_replace(treatment, "FOS", "SH + AFR")) %>%
  mutate(treatment = str_replace(treatment, "SH", "SH"))

sum_clean$treatment = as.factor(sum_clean$treatment)

sum_clean  = sum_clean %>% mutate(treatment = fct_relevel(treatment, "SH", "SH + AFR", "AP", "AP + AFR"))


# Assign serial id numbers to animals
sum_clean = sum_clean %>% group_by(treatment, animal) %>% mutate(animal_id = cur_group_id()) %>% group_by(treatment) %>% mutate(animal_id = 1+ animal_id - min(animal_id))

# Find which vascular tree each vessel is in
sum_clean = sum_clean %>% mutate(vessel_tree = floor(parse_number(vessel)), site = floor(site))

# Check that everything got imported by printing out the number of videos per animal
vessel_videos = sum_clean %>% select(animal, source_video, treatment) %>% distinct() %>% group_by(treatment, animal) %>% summarise(number_videos = n())


sum_clean_s = sum_clean %>% group_by(animal, treatment) %>% summarise(n = n())


#####################
# Start summarising
####################

maxo = function(vector)
{
  vector = max(vector, 0, na.rm = TRUE)
}


meano = function(vector)
{
  vector = mean(vector, na.rm = TRUE)
}



# Summarise by video
# Calculate the maximum number of contractions in each video, anywhere in the source tree
summ2.df = sum_clean %>% group_by(site, animal, animal_id, treatment, source_video, vessel_tree) %>%
  summarise(max_n_cont= maxo(n_contraction), max_mag = maxo(max_magnitude),
            mean_mag = meano(mean_magnitude), cont_sum = maxo(sum_magnitude),
            width = maxo(mean_width),  duration = meano(mean_duration), fpf = meano(FPF), ef = mean(mean_EF))

ggplot(summ2.df) + geom_boxplot(aes(y = max_n_cont, x = treatment))

# Summarise by vessel tree
# Total up all the videos, to give the total number of contractions in a vessel tree accross all 8 videos (check there are actualy 8?)
summ2.5.df = summ2.df %>% group_by(site, animal, animal_id, treatment, vessel_tree) %>%
  summarise(nvid = n(), max_n_cont = meano(max_n_cont), max_mag = maxo(max_mag),
            mean_mag = meano(mean_mag), cont_sum = meano(cont_sum), width = median(width),
            duration = mean(duration), fpf = mean(fpf), ef = mean(ef))


ggplot(summ2.5.df) + geom_boxplot(aes(y = max_n_cont, x = treatment))

# Summarise by site
# Find the most contractile vessel tree in each site of each animal
summ3.df = summ2.5.df %>% group_by(site, animal, animal_id, treatment) %>%
  summarise(max_n_cont = meano(max_n_cont), max_mag = maxo(max_mag),
            mean_mag = meano(mean_mag), cont_sum = meano(cont_sum), width = median(width),
            duration = meano(duration), fpf = mean(fpf), ef = mean(ef))

if(true){
animal_site_counts = summ3.df %>% select(animal, site, treatment) %>% distinct() %>% group_by(animal, treatment) %>% mutate(n = n()) %>% arrange(n)

ggplot(summ3.df) + geom_boxplot(aes(y = max_n_cont, x = treatment, fill = factor(animal_id)))
ggplot(summ3.df) + geom_violin(aes(y = max_n_cont, x = treatment, fill = factor(animal_id)))
ggplot(summ3.df) + geom_dotplot(aes(y = max_n_cont, x = treatment, fill = factor(animal_id)), binaxis = "y", binpositions = "all", stackgroups = TRUE, binwidth = 1, stackdir = "center")

animal_site_counts = sum_clean_full %>% select(animal, site, treatment) %>% distinct() %>% group_by(animal, treatment) %>% mutate(n = n()) %>% arrange(n)

ggplot(summ3.df) + geom_boxplot(aes(y = duration, x = treatment, fill = factor(animal_id)))
}

# Summarise by animal
# Find the mean number of contractions accross all 4 sites in each animal
summ4.df = summ3.df %>% group_by(animal, treatment, animal_id) %>%
  summarise(max_n_cont = meano(max_n_cont), max_mag = mean(max_mag, na.rm = TRUE),
            mean_mag = meano(mean_mag), cont_sum = meano(cont_sum), width = median(width),
            duration = mean(duration), fpf = mean(fpf), ef = mean(ef))



# ggplot(summ4.df) + geom_violin(aes(y = max_n_cont, x = treatment)) +
#   geom_dotplot(aes(y = max_n_cont, x = treatment), color = "blue", binaxis = "y", binpositions = "all", stackdir = "center") +
#   scale_y_continuous(limits = c(0,NA)) +
#   labs(y = "Average Contractions", x = "Treatment")



ggplot(summ4.df) + geom_boxplot(aes(y = max_n_cont, x = treatment)) +
  geom_point(aes(y = max_n_cont, x = treatment, color = as.character(animal)), position = "dodge") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(y = "Average Contraction Frequency / Vessel / Minute", x = "")


ggplot(summ4.df) + geom_boxplot(aes(y = max_mag, x = treatment)) +
  geom_point(aes(y = max_mag, x = treatment, color = as.character(animal)), position = "dodge") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(y = "Max Magnitude", x = "Treatment")

ggplot(summ4.df) + geom_boxplot(aes(y = mean_mag, x = treatment)) +
  geom_point(aes(y = mean_mag, x = treatment), color = "blue", position = "dodge") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(y = "Average Magnitude", x = "Treatment")

ggplot(summ4.df) + geom_boxplot(aes(y = fpf, x = treatment)) +
  geom_point(aes(y = fpf, x = treatment, color = as.character(animal_id)), position = "dodge") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(y = "Average Magnitude", x = "Treatment")



# Mean magnitude per vessel tree
# Number of contractions per vessel
# Sum of magnitudes per vessel
# Width per vessel
# Duration of ejection phase per vessel

saveRDS(summ4.df, "TEST.rds")

ggplot(summ4.df) + geom_boxplot(aes(y = max_n_cont*2, x = treatment)) +
  geom_point(aes(y = max_n_cont*2, x = treatment), color = "blue", position = "dodge") +
  scale_y_continuous(limits = c(NA,NA)) +
  labs(y = "Average Contraction Frequency / Vessel", x = "")

# Compute the analysis of variance
res.aov <- aov(max_n_cont ~ treatment, data = summ4.df)
summary(res.aov)
TukeyHSD(res.aov)


ggplot(summ4.df) + geom_boxplot(aes(y = mean_mag, x = treatment)) +
  geom_point(aes(y = mean_mag, x = treatment), color = "blue", position = "dodge") +
  scale_y_continuous(limits = c(NA,NA)) +
  labs(y = "Average Contraction Amplitude / Vessel", x = "")

# Compute the analysis of variance
res.aov <- aov(mean_mag ~ treatment, data = summ4.df)
summary(res.aov)
TukeyHSD(res.aov)


ggplot(summ4.df) + geom_boxplot(aes(y = width, x = treatment)) +
  geom_point(aes(y = width, x = treatment), color = "blue", position = "dodge") +
  scale_y_continuous(limits = c(NA,NA)) +
  labs(y = "Average Diameter / Vessel", x = "")

# Compute the analysis of variance
res.aov <- aov(width ~ treatment, data = summ4.df)
summary(res.aov)
TukeyHSD(res.aov)


ggplot(summ4.df) + geom_boxplot(aes(y = duration, x = treatment)) +
  geom_point(aes(y = duration, x = treatment), color = "blue", position = "dodge") +
  scale_y_continuous(limits = c(NA,NA)) +
  labs(y = "Average Contraction Duration / Vessel", x = "")

# Compute the analysis of variance
res.aov <- aov(duration ~ treatment, data = summ4.df)
summary(res.aov)
TukeyHSD(res.aov)



ggplot(summ4.df) + geom_boxplot(aes(y = fpf, x = treatment)) +
  geom_point(aes(y = fpf, x = treatment), color = "blue", position = "dodge") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(y = "Mean Fractional Pump Flow", x = "")

# Compute the analysis of variance
res.aov <- aov(fpf ~ treatment, data = summ4.df)
summary(res.aov)
TukeyHSD(res.aov)


ggplot(summ4.df) + geom_boxplot(aes(y = ef, x = treatment)) +
  geom_point(aes(y = ef, x = treatment), color = "blue", position = "dodge") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(y = "Mean Ejection Fraction", x = "")

# Compute the analysis of variance
res.aov <- aov(ef ~ treatment, data = summ4.df)
summary(res.aov)
TukeyHSD(res.aov)






ggplotly(ggplot(summ4.df) + geom_boxplot(aes(y = cont_sum, x = treatment)) +
  geom_point(aes(y = cont_sum, x = treatment, color = as.character(animal)), position = "dodge") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(y = "Average Contractions", x = "Treatment"))


ggplotly(ggplot(summ4.df) + geom_boxplot(aes(y = width, x = treatment)) +
           geom_point(aes(y = width, x = treatment, color = as.character(animal)), position = "dodge") +
           scale_y_continuous(limits = c(0,NA)) +
           labs(y = "Median diameter", x = "Treatment"))


ggplotly(ggplot(summ4.df) + geom_boxplot(aes(y = duration, x = treatment)) +
           geom_point(aes(y = duration, x = treatment, color = as.character(animal)), position = "dodge") +
           scale_y_continuous(limits = c(0,NA)) +
           labs(y = "Mean duration", x = "Treatment"))

# Calculate stats for a final barplot

summ5.df = summ4.df %>% group_by(treatment) %>%
  summarise(mean = mean(max_n_cont*2), std = sd(max_n_cont*2)/sqrt(n()),
            mag_mean = mean(mean_mag), mag_std = sd(mean_mag)/sqrt(n()),
            cont_mean = mean(cont_sum), cont_std = sd(cont_sum)/sqrt(n()))

ggplot(summ5.df) + geom_col(aes(y = mean, x = treatment)) +
  geom_errorbar(aes(y = mean, x = treatment, ymax = (mean + std), ymin = (mean-std))) +
  labs(y = "Average Contractions/ Minute", x = "Treatment")

ggplot(summ5.df) + geom_col(aes(y = mag_mean, x = treatment)) +
  geom_errorbar(aes(y = mag_mean, x = treatment, ymax = (mag_mean + mag_std), ymin = (mag_mean-mag_std))) +
  labs(y = "Average Contracton Magnitude (px)", x = "Treatment")

ggplot(summ5.df) + geom_col(aes(y = cont_mean, x = treatment)) +
  geom_errorbar(aes(y = cont_mean, x = treatment, ymax = (cont_mean + cont_std), ymin = (cont_mean-mag_std))) +
  labs(y = "Average Contraction Total Magnitude", x = "Treatment")



# # Compute the analysis of variance
# res.aov <- aov(cont_sum ~ treatment, data = summ4.df)
# # Summary of the analysis
# summary(res.aov)
# TukeyHSD(res.aov)




# Compute the analysis of variance
res.aov <- aov(duration ~ treatment, data = summ4.df)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)

library(car)
leveneTest(max_n_cont ~ treatment, data = summ4.df)

# Extract the residuals
aov_residuals <- residuals(object = res.aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

kruskal.test(max_n_cont ~ treatment, data = summ4.df)

pairwise.wilcox.test(summ4.df$max_n_cont, summ4.df$treatment,
                     p.adjust.method = "fdr")

