mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

fname <- file.path(mvadir, "helper.R")
stopifnot(file.exists(fname))
source(fname)