blank = function()
{

  libraries = c("imager", "av", "tools", "ggplot2", "dplyr", "rlang", "foreach", "doSNOW", "pbmcapply", "devtools")
  new.packages <- libraries[!(libraries %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)

  lapply(libraries, library, character.only = TRUE)

  setwd("//files.auckland.ac.nz/research/ressci202000061-PROM-study/vmeasur")
  devtools::load_all()

  scratch_dir("Q://")
  setwd("//files.auckland.ac.nz/research/ressci202000061-PROM-study")

  select_roi()

  threshold_apply( threshold = '0.5',roi_name = 'Test 16_4',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Version 5/image826.avi', radians = 0.193621992855945, xlength = 60, ylength = 241, xstart = 688, ystart = 310 )

  threshold_apply( threshold = '0.885',roi_name = 'testz',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Version 5/image039.avi', radians = -0.0713074647852902, xlength = 60, ylength = 125, xstart = 512, ystart = 198 )



datum = read.csv("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Version 5/image826_Late Test_widths.csv")
datum = read.csv("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Version 5/image039_testz_widths.csv")

datum$frame = extract_numeric(datum$filename)
1.1
ggplot(datum, aes(x = frame, y = -y, fill = p_width)) + geom_raster()

dataum2 = datum %>% group_by(frame) %>% summarise(mean = mean(p_width)) %>% ungroup()

ggplot(dataum2, aes(x = frame, y = mean)) + geom_line()




#  All ROIs listed below
# AP19S1.1_1.1
threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image112.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )

threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image113.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )

threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image114.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image115.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image116.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image117.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image118.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image119.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
#AP19S1.1_2.1
threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image112.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image113.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image114.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image115.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image116.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image117.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image118.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image119.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
#Ap19S1.1_2.2





}













