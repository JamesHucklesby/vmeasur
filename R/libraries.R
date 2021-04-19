library(imager)
library(av)
library(tools)
library(tidyverse)
library(tools)
library(rlang)
library(foreach)
library(doSNOW)
library(pbmcapply)


library(renv)

libraries = c("imager", "av", "tools", "ggplot2", "dplyr", "rlang", "foreach", "doSNOW", "pbmcapply", "devtools")

install(libraries)
