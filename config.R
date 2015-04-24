library(nnet)
library(mboost)
library(gtools)

source("multistep.R")
source("methods.R")
source("utils.R")

# For strategies with KNN model
path_to_fastkknn <- "../fastkknn/"
fastkkn_file <- paste(pat_to_fastkknn, "fastkknn.R", sep = "")
if(!file.exists(fastkkn_file)){
	stop("You need to download the fastkknn code (https://github.com/bsouhaib/fastkknn) and set the path_to_fastkknn variable.")
}
source(fastkkn_file)

