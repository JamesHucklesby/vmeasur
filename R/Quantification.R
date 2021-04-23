# datum = read.csv("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Version 5/image826_Late Test_widths.csv")
# datum = read.csv("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Version 5/image039_testz_widths.csv")
#
# datum$frame = extract_numeric(datum$filename)
# ggplot(datum, aes(x = frame, y = -y, fill = p_width)) + geom_raster()
#
#
#
# datumgood = datum %>% group_by(y) %>% mutate(excludeany = sum(excluded)==0) %>% ungroup() %>% mutate(p_width = ifelse(excludeany, p_width, NA))
#
# dataum3 = datumgood %>% group_by(y) %>% summarise(mean = mean(p_width), max = max(p_width), min = min(p_width), range = max-min) %>% ungroup()
#
# ggplot(dataum3, aes(x = y, y = mean)) + geom_line() + coord_flip() + scale_x_reverse() + labs(title = "Average width of the vessel throughout the video", x = "Vessel Location")
# ggplot(dataum3, aes(x = y, y = range)) + geom_line() + coord_flip() + scale_x_reverse() + labs(title = "Maximum - minimum vessel width throughout time", x = "Vessel Location")
#
# ggplot(dataum3, aes(x = y, y = range/max*100)) + geom_line() + coord_flip() + scale_x_reverse() + labs(title = "Maximum percent reduction in vessel width throughout time")
#
#
# dataum2 = datumgood %>% group_by(frame) %>% summarise(mean = mean(p_width, na.rm = TRUE)) %>% ungroup()
#
# ggplot(dataum2, aes(x = frame, y = mean)) + geom_line()
#
#
