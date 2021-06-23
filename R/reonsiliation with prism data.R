# install.packages("pzfx")
# library(pzfx)
# library(tidyverse)
#
# prism_path = "\\\\files.auckland.ac.nz\\research\\ressci202000061-PROM-study\\physiol_data.pzfx"
#
# table_list = pzfx_tables(prism_path)
#
#
# import = foreach(current_table = table_list, .combine=rbind) %do% {
#
# read_pzfx(prism_path, table=current_table) %>%
#   pivot_longer(-`Minutes from disease onset`, "animal") %>%
#   separate(animal, c("treatment", "animal_id"), sep = "_") %>%
#   mutate(measure = current_table)
# }
#
# unique(import$measure)
#
# time_series = import
#
# imp1 = import %>%
#   filter(`Minutes from disease onset` == 315) %>%
#   pivot_wider(names_from = measure) %>%
#   mutate(animal_id = as.numeric(animal_id), treatment = str_replace(treatment, "SHAM", "SH"), `Minutes from disease onset` = NULL)
#
#
#
# prism_path = "\\\\files.auckland.ac.nz\\research\\ressci202000061-PROM-study\\fluids_data.pzfx"
#
# table_list = pzfx_tables(prism_path)
#
#
# import = foreach(current_table = table_list[c(1,2,3,4,6,11,12,13)], .combine=rbind) %do% {
#
#   read_pzfx(prism_path, table=current_table) %>%
#     mutate(animal_id = c(1:n())) %>%
#     pivot_longer(-animal_id) %>%
#     rename(treatment = name) %>%
#     mutate(measure = current_table)
# }
#
# unique(import$measure)
#
# imp2 = import %>% mutate(treatment = str_replace(treatment, "Sham", "SHAM")) %>%
#   pivot_wider(names_from = measure, values_from = value) %>%
#   mutate(treatment = str_replace(treatment, "SHAM", "SH"))
#
#
#
# prism_path = "\\\\files.auckland.ac.nz\\research\\ressci202000061-PROM-study\\oedema_data.pzfx"
#
# table_list = pzfx_tables(prism_path)
#
#
# import = foreach(current_table = table_list, .combine=rbind) %do% {
#
#   read_pzfx(prism_path, table=current_table) %>%
#     mutate(animal_id = c(1:n())) %>%
#     pivot_longer(-animal_id) %>%
#     rename(treatment = name) %>%
#     mutate(measure = current_table)
# }
#
# unique(import$measure)
#
# imp3 = import %>% mutate(treatment = str_replace(treatment, "Sham", "SHAM")) %>%
#   pivot_wider(names_from = measure, values_from = value) %>%
#   mutate(treatment = str_replace(treatment, "SHAM", "SH"))
#
#
# imp_all = left_join(imp1, imp2) %>% left_join(imp3)
#
#
# mass_combine_real = summ4.df %>%
#   left_join(imp_all, by = c("treatment", "animal_id"))
#
#
#
#
#
#
#
#
#
# import %>%
# filter(measure == "O2 Sats") %>%
# ggplot() +
# geom_line(aes(x = `Minutes from disease onset`, y = value, color = treatment, group = interaction(animal_id, treatment)))
#
#
#   import %>%
#   filter(measure == "O2 Sats") %>%
#   group_by(treatment, `Minutes from disease onset`) %>%
#   summarise(mean = mean(value, na.rm = TRUE), sem = sd(value, na.rm = TRUE)/sqrt(n())) %>%
#
#   ggplot() +
#   geom_line(aes(x = `Minutes from disease onset`, y = mean, color = treatment)) +
#   geom_ribbon(aes(ymax = mean + sem, ymin = mean - sem, x = `Minutes from disease onset`, fill = treatment), alpha = 0.3)
#
#
#
#
# unique(tomerge$treatment, summ4.df$treatment)
#
# mass_combine_real = summ4.df %>%
#   left_join(tomerge, by = c("treatment", "animal_id"))
#
# mass_combine_real %>% ggplot  + geom_point(aes(x = fpf, y = MAP, color = treatment))
#
# mass_combine_real %>% ggplot  + geom_point(aes(x = fpf, y = `Hematocrit - ABG`, color = treatment))
# mass_combine_real %>% ggplot  + geom_point(aes(x = fpf, y = WBC, color = treatment))
#
# mass_combine_real %>% ggplot  + geom_point(aes(x = fpf, y = Lactate, color = treatment))
#
#
# mass_combine_real %>% ggplot  + geom_point(aes(x = max_n_cont, y = max_mag, color = treatment))
# mass_combine_real %>% ggplot  + geom_point(aes(x = max_n_cont, y = width, color = treatment))
#
# mass_combine_real %>% ggplot  + geom_point(aes(x = fpf, y = MAP)) +
#   geom_smooth(method = "lm", aes(x = fpf, y = MAP)) +
#   facet_wrap(vars(treatment))
#
#
#
# mass_combine_real %>% ggplot  +
#   geom_point(aes(x = log(fpf), y = MAP, color = treatment)) +
#   geom_smooth(aes(x = log(fpf), y = MAP, color = treatment, fill = treatment), alpha = 0.3,method = "lm")
#
# mass_combine_real = as.data.frame(mass_combine_real)
#
# ggpairs(data = mass_combine_real[c(2,4,8)])
#
#
# model = lm(max_n_cont ~ MAP + treatment, mass_combine_real)
# summary(model)
#
#
#
# mass_combine_real %>% ggplot  + geom_point(aes(x = max_mag, y = MAP, color = treatment))
#
# mass_combine_real %>% ggplot  + geom_point(aes(x = fpf, y = HR, color = treatment))
# mass_combine_real %>% ggplot  + geom_point(aes(x = max_n_cont, y = HR, color = treatment))
#
#
# mass_combine = as.data.frame(mass_combine_real)
#
# row.names(mass_combine) = paste(mass_combine$treatment, mass_combine$animal_id)
#
# mass_combine$`Minutes from disease onset` = NULL
# mass_combine$animal = NULL
# mass_combine$animal_id = NULL
# mass_combine$`O2 Sats` = NULL
# mass_combine$treatment = NULL
#
#
# library(missMDA)
#
# nb <- estim_ncpPCA(mass_combine,method.cv = "Kfold", verbose = FALSE)
#
#
# res.comp <- imputePCA(mass_combine, ncp = nb$ncp) # iterativePCA algorithm
# res.comp$completeObs[1:3,] # the imputed data set
#
# imp <- cbind.data.frame(res.comp$completeObs, mass_combine_real$treatment)
#
# library(FactoMineR)
#
# res.pca <- PCA(imp,scale.unit = TRUE, quali.sup = 59, ncp = nb$ncp, graph=FALSE)
# plot.PCA(res.pca, hab=59, lab="all");
#
# plot(res.pca, choix="var")
#
#
#
#
#
#
#
# res.pca <- prcomp(mass_combine, scale = TRUE)
#
# fviz_eig(res.pca)
#
# fviz_pca_ind(res.pca,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
#
#
# fviz_pca_var(res.pca,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
#
#
#
# groups <- as.factor(mass_combine_real$treatment)
#
# fviz_pca_ind(res.pca,
#              col.ind = groups,
#              addEllipses = TRUE, # Concentration ellipses
#              ellipse.type = "confidence",
#              legend.title = "Groups",
#              repel = TRUE
# )
#
#
#
#
