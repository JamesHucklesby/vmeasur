# datum = read.csv("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Version 5/image826_Late Test_widths.csv")
# datum = read.csv("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Version 5/image039_testz_widths.csv")
#
# datum$frame = extract_numeric(datum$filename)
# 1.1
# ggplot(datum, aes(x = frame, y = -y, fill = p_width)) + geom_raster()
#
# dataum2 = datum %>% group_by(frame) %>% summarise(mean = mean(p_width)) %>% ungroup()
#
# ggplot(dataum2, aes(x = frame, y = mean)) + geom_line()
#
#
