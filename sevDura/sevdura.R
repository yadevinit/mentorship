if(! exists("cRoot")) cRoot <- c(file.path("."), file.path("../Downloads/sevDura"))[2]
source(file.path(cRoot, "sevdura-helper.R"))
c_sevDura_Filename_suffix <- sub(pattern="^ - ", replacement="-", x=c_gsFilename_which)
  # was till 2023Nov27: c("-2023Nov22-2015", "-2023Nov20-1140", "-2023Nov17-1525", "-2023Nov06-2242")[1]
spec.i <- 1 # init
c_fName_fits <- file.path(cRoot, paste0("fits", c_sevDura_Filename_suffix, ".rds"))
c_fName_loos <- file.path(cRoot, paste0("loos", c_sevDura_Filename_suffix, ".rds"))
c_sevDura_Filename <- file.path(cRoot, paste0("dat", c_sevDura_Filename_suffix, ".csv"))
if(! file.exists(c_sevDura_Filename)){
  dat <- myRead()
  dat$sevDura <- create_sevDura_v2(dat)
  write.csv(dat, file=c_sevDura_Filename, row.names=FALSE)
} # else continue
stopifnot(file.exists(c_sevDura_Filename))

dat1 <- myRead_sevDura(sevDura_0nil_default=0, ref=c("1Ami25x001", "3Ami37x001")[1], # was: [2]
  cuts=c(0, 3), grp=(0 + c(0, 123, 45678910)) # was till 2023Dec02: grp=(0 + c(0, 123, 45678))
  # considering at least 60/300 observations (days) for sev0 as well as sev4+ for stabler modeling.
  # alt: hist() shows dips at 1 and 5; so, ... cuts=c(0, 4), grp=(0 + c(0, 1234, 5678)
)
# stop(); fits.bak <- fits; fits <- list() # ; fi <- 0 # init

# c.decreasing <- c(FALSE, TRUE)[1]
if(! file.exists(c_fName_fits)){
  # rethinkPlots(dat1) # dat.mdl <- dat1
  fits <- genSaveFits(dat1, bayeSpec, c_fName_fits)
} # else continue
if(! file.exists(c_fName_loos)){
  genSaveLoos(patn=c.patn.n[3], fits, c_fName_loos)
} # else continue
stopifnot(file.exists(c_fName_fits) & file.exists(c_fName_loos))

# if(file.exists(c_fName_fits)){
  # rethinkPlots(dat1)
  fits <- readRDS(file=c_fName_fits); fits.names <- names(fits)
    # fits.names <- sort(names(fits), decreasing=c.decreasing)
  fits.names.which <- list(seq_along(fits.names),
    grep(pattern="[.][0-9]$", fits.names))[[1]]; fits.names.which
  ix <- list(fits.names.which[], spec.i)[[1]]
  view_brms_fits(fits, ix=ix)
# } # else continue
# if(file.exists(c_fName_loos)){
  loos <- readRDS(file=c_fName_loos)
  fits.names.sel <- names(loos)
  loo_compare_ySame(loos, fits.names.sel)
  # Warning: [In plot.psis_loo(loos.j.i, label_points = TRUE) :
  # 19% of Pareto k estimates are Inf/NA/NaN and not plotted.]
  # was till 2024Jan23: loo_compare(loos[fits.names.sel])
  # c("sev02.02.1.tf.ca", "sev09.02.1")
  # [(0.7, 1]   (bad)        2    0.6%   109 ... See help('pareto-k-diagnostic') for details.]
  # [Diagnostics for Pareto smoothed importance sampling (PSIS)]
  # loo::pareto_k_ids(loos[["sev09.02.1"]]) # [[1] 315 331] observations
# } # else continue
