#' Title
#'
#' @return
#' @export
#'
#' @importFrom av av_video_images av_encode_video
#' @importFrom imager load.image grabLine draw_rect
#' @importFrom rlang hash
#' @importFrom utils winDialogString
#'
#' @examples
select_roi = function()
{

roi_name = winDialogString("Enter ROI name", "")

video_path = file.choose()
video_folder = dirname(video_path)
video_name = basename(file_path_sans_ext(video_path))


workingdir = paste(scratch_dir(),"/",hash(video_name), sep = "")

av_video_images(video_path, format = "png", destdir = workingdir, fps = 1)

raw = load.image(paste(workingdir,"/image_000001.png", sep = ""))

latch = TRUE

while(latch)
{

angleselect = grabLine(raw, output = "coord")

degrees = atan((angleselect["y0"]-angleselect["y1"])/(angleselect["x0"]-angleselect["x1"]))/(2*pi)*360
rotation = (90-abs(degrees))%%90 * (degrees/abs(degrees))
radians = rotation/360*(2*pi)
rotated = imrotate(raw, rotation)

areaselect = grabRect(rotated, output = "coord")

xstart = min(areaselect["x1"],areaselect["x0"]) - width
ystart = min(areaselect["y1"],areaselect["y0"]) - width

ylength = max(areaselect["y1"],areaselect["y0"])-ystart
xlength = max(areaselect["x1"],areaselect["x0"])-xstart

cropped = crop_dims(rotated, xstart, ystart, xlength, ylength)

plot(cropped)

cat("\nEnter y to accept, or n to go again, or c to cancel")
again <- scan(what=character(),nmax=1,quiet=TRUE)

if(again =="y")
{
  latch = FALSE
}

if(again =="c")
{
  return(FALSE)
}

}


# draw_rect(rotated,xstart,ystart,xstart+xlength, ystart+ylength,color="green",opacity=0.3) %>% plot()


filter_string = paste("framestep = 100, rotate = '",radians,":out_w=rotw(",radians,"):out_h=roth(",radians,"):c = red',",
                      "crop=",xlength,":",ylength,":",xstart,":",ystart,"",
                      sep = "")

av_encode_video(video_path, paste(workingdir, "/test1w%03d_cropped.png", sep = ""), vfilter = filter_string, codec = "png")

file_list = list.files(workingdir, full.names = TRUE, pattern = "\\_cropped.png$")


thresholds = c()

for(file in file_list)
{
  thresholds = c(thresholds,calculate_auto_threshold(file_path = file))
}

overallthreshold = mean(thresholds)

accept_threshold = FALSE

while(!accept_threshold)
{

thresholded = imlist()

for(file in file_list)
{
  print(file)
  thresholded = ci(thresholded, threshold_image(file_path = file, threshold = overallthreshold)[[1]])
}

make_matrix(thresholded, width = 8) %>% plot

print(paste("Threshold is:", overallthreshold))

cat("\nEnter y to accept, or a new number to recalculate")
again <- scan(what=character(),nmax=1,quiet=TRUE)

if(again =="y")
{
  accept_threshold = TRUE
}
else
{
  overallthreshold = as.numeric(again)
}

}


video_path = gsub("\\\\", "/", video_path)

variables = paste("threshold = '",overallthreshold,"',",
      "roi_name = '",roi_name,"',",
      "video_path = '", video_path, "',",
      " radians = ", radians,",",
      " xlength = ", xlength,",",
      " ylength = ", ylength,",",
      " xstart = ", xstart,",",
      " ystart = ", ystart, sep ="")

unlink(workingdir, recursive = TRUE)

function_string = paste("threshold_apply(",variables,")")
writeClipboard(function_string)

function_string = paste("\n", function_string, "\n ")

cat(crayon::green(function_string))

}


