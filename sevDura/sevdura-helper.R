print(date())
options(warnPartialMatchDollar = TRUE)
  # ref https://adv-r.hadley.nz/subsetting.html:
  # x <- list(abc = 1); x$a
  # > Warning in x$a: partial match of 'a' to 'abc'
c_betterBy <- ifelse(exists("c_betterBy"), c_betterBy, 40 / 100)
  # initial prophylaxis regime bettered prior baseline by
c_posts.prescription=c("1Ami25x001", "8Top50x101_sosNaxRiz")
c_bayeSpec <- "bayeSpec.csv"
require(ggplot2)
require(brms)

c.patn.n <- if(! exists("c.patn.n")){ c("^sev02[.]02.*", "^sev09[.]02.*", "^sev02[.]02.*|^sev09[.]02.*") } else c.patn.n
c_gsFilename_which <- ifelse(exists("c_gsFilename_which"), c_gsFilename_which,
  c(" - 2024Jan28-0605", " - 2024Jan26-0710", " - 2024Jan23-1032", " - 2024Jan22-1012", " - 2024Jan18-0950",
      " - 2024Jan13-1222", " - 2024Jan11-1210", " - 2024Jan10-0635", " - 2023Dec28-0655", " - 2023Dec23-0705", " - 2023Dec19-2135",
    " - 2023Dec03-1235", " - 2023Dec02-0945", " - 2023Nov30-0935",
      # includes `medication` & `triggerTravel`.  Was till 2023Nov25: " - 2023Nov22-2000",
    " - 2023Nov02-2015" # shared for 2023Nov03 review
  )[1]
)
c_gsFilename <- ifelse(exists("c_gsFilename"), c_gsFilename,
  paste0("nimHeadacheDiary", c_gsFilename_which, ".csv")
)
c_wt_severityMax <- ifelse(exists("c_wt_severityMax"), c_wt_severityMax,
  c(1.00, 0.60, 0.70)[3] # ie (1 - c_wt_severityMax) is weight for durationH, as suggested by Anirudh
)
c_width <- ifelse(exists("c_width"), c_width,
  c(7)[1] # aggregate days---e.g., 7 for a week---for rolling analysis
)
c_myRollFunc_which <- ifelse(exists("c_myRollFunc_which"), c_myRollFunc_which,
  c("mean", "median")[2]
)

c_carry_width <- c(21, 365)[2]
c_sevDura_0nil_default <- c(NA_integer_, 0)[2] # or NA_real_
c_tryFormats <- c("%Y-%b-%d", "%Y-%m-%d")
if(! require(zoo)){ install.packages(c("zoo")); require(zoo) }

# c_myRollFunc_which <- if(! exists("c_myRollFunc_which")) c("mean", "median")[2] else c_myRollFunc_which
myRollFunc <- switch(c_myRollFunc_which,
  mean = { function(winVec, naAct=TRUE){ mean(winVec, na.rm=naAct) } },
  median = { function(winVec, naAct=TRUE){ median(winVec, na.rm=naAct) } },
  stop("Unsupported c_myRollFunc_which")
)
myFunc_old <- list(
  function(winVec, naAct=TRUE){ mean(winVec, na.rm=naAct) },
  function(winVec, naAct=TRUE){ median(winVec, na.rm=naAct) }
)[[1]]
  # alt: runmed(x, k, endrule = c("median", "keep", "constant")[1]) # for the 'most robust' scatter plot smoothing possible
myReadSheet <- function(gsFilename) {
  # ref https://stackoverflow.com/questions/59746036/how-to-read-data-from-google-drive-using-r-in-colab
  require(googlesheets4)
  require(googledrive)
  gs_file <- drive_get(gsFilename) # was: drive_get('name_of_sheet_on_google')
  gs_data <- read_sheet(gs_file)
  return(gs_data)
}
myRead <- function(fName=c_gsFilename, sevDura_0nil_default=c_sevDura_0nil_default){
  # sevDura_0nil_default arg added 2023Dec03
  gsfData <- read.csv(file=file.path(cRoot, fName))
    # was till 2023Nov27: read.csv(file=fName) # avoid myReadSheet(c_gsFilename) coz might cache OAuth credentials
  # str(gsfData)
  dat <- gsfData
  dat$date <- as.Date(dat$date, tryFormats=c("%Y-%b-%d"))
  dat$site <- as.factor(dat$site)
  # str(dat)
  dat$siteTemp <- gsfData$site
  hasTemporal <- grepl(pattern="Temporal.*", x=dat$siteTemp); str(which(hasTemporal))
  hasFrontal <- grepl(pattern="Frontal.*", x=dat$siteTemp); str(which(hasFrontal))
  dat$siteTemp[hasFrontal & (! hasTemporal)] <- "FrontalsnoTemporal"
  dat$siteTemp[hasTemporal] <- "Temporalplus"
  dat$siteTemp[(dat$siteTemp == "")] <- "0nil"
  dat$siteTemp <- as.factor(dat$siteTemp)
  dat$prescription <- as.factor(dat$prescription) # changed 2023Dec03
  dat$severityMax[is.na(dat$severityMax) & (dat$siteTemp == "0nil")] <- sevDura_0nil_default # was till 2023Dec03: 0
  dat$durationH[is.na(dat$durationH) & (dat$siteTemp == "0nil")] <- sevDura_0nil_default # was till 2023Dec03: 0
  medication <- dat$medication
    medication[medication == ""] <- "0nil"
    dat$medication_fac <- as.factor(medication)
    # dat$medication_fac <- addNA(dat$medication_fac, ifany=TRUE)
  triggerTravel <- sub(pattern="yes.*", replacement="yes", x=dat$triggerTravel)
    triggerTravel[triggerTravel == ""] <- "0nil"
    dat$triggerTravel_fac <- as.factor(triggerTravel)
    # dat$triggerTravel_fac <- addNA(dat$triggerTravel_fac, ifany=TRUE)
  summary(dat)
  return(dat)
}
create_sevDura_v2 <- function(dat, sevDura_0nil_default=c_sevDura_0nil_default){
  # added 2023Nov13
  wt_dur <- (max(dat$severityMax, na.rm=TRUE) /
    max(dat$durationH, na.rm=TRUE)) *
      (1 - c_wt_severityMax)
  dur_sevMax <- (dat$severityMax * c_wt_severityMax) + (dat$durationH * wt_dur)
  dur_sevMax[is.na(dur_sevMax)] <- sevDura_0nil_default # was: 0
    summary(dur_sevMax)
  # plot(dur_sevMax, type="l")
  sevDura <- zoo(x=dur_sevMax, order.by=dat$date)
  print(summary(sevDura))
  return(sevDura)
}
create_sevDura <- function(dat){
  wt_dur <- (max(dat$severityMax, na.rm=TRUE) /
    max(dat$durationH, na.rm=TRUE)) *
      (1 - c_wt_severityMax)
  dur_sevMax <- (dat$severityMax * c_wt_severityMax) + (dat$durationH * wt_dur)
  dur_sevMax[is.na(dur_sevMax)] <- 0; summary(dur_sevMax)
  # plot(dur_sevMax, type="l")
  sevDura <- zoo(x=dur_sevMax, order.by=dat$date)
  return(sevDura)
}
getHMfrac <- function(start24Hvec){
  start24H.h <- (start24Hvec %/% 100)
  start24H.m <- (start24Hvec - (start24H.h * 100)) / 60
  start24H.hm <- start24H.h + start24H.m
  return(start24H.hm)
}

myRead_headacheDiary_v2 <- function(fName=c_gsFilename, tryFormats=c_tryFormats,
  sevDura_0nil_default=c_sevDura_0nil_default){
  gsfData <- read.csv(file=fName) # avoid myReadSheet(c_gsFilename) coz might cache OAuth credentials
  # str(gsfData)
  dat <- gsfData
  dat$date <- as.Date(dat$date, tryFormats=tryFormats)
  dat$site <- as.factor(dat$site)
  dat$siteTemp <- gsfData$site
  hasTemporal <- grepl(pattern="Temporal.*", x=dat$siteTemp); str(which(hasTemporal))
  hasFrontal <- grepl(pattern="Frontal.*", x=dat$siteTemp); str(which(hasFrontal))
  dat$siteTemp[hasFrontal & (! hasTemporal)] <- "FrontalsnoTemporal"
  dat$siteTemp[hasTemporal] <- "Temporalplus"
  dat$siteTemp[(dat$siteTemp == "")] <- "0nil"
  dat$siteTemp <- as.factor(dat$siteTemp)
  dat$prescription <- as.factor(dat$prescription)
  # following 2 stmts were uncommented till 2023Dec03:
  # dat$severityMax[(dat$siteTemp == "0nil") & is.na(dat$severityMax)] <- sevDura_0nil_default
  # dat$durationH[(dat$siteTemp == "0nil") & is.na(dat$durationH)] <- sevDura_0nil_default
    # was till 2023Nov12: <- 0
  stopifnot(all((! is.na(dat$severityMax)) | (dat$siteTemp != "0nil"))) # asserted 2023Dec03
  medication <- dat$medication
    medication[(medication == "")] <- "0nil"
    dat$medication_fac <- as.factor(medication)
    # dat$medication_fac <- addNA(dat$medication_fac, ifany=TRUE)
  triggerTravel <- sub(pattern="yes.*", replacement="yes", x=dat$triggerTravel)
    triggerTravel[(triggerTravel == "")] <- "0nil"
    dat$triggerTravel_fac <- as.factor(triggerTravel)
    # dat$triggerTravel_fac <- addNA(dat$triggerTravel_fac, ifany=TRUE)
  dat$start24HMfrac <- getHMfrac(dat$start24H)
  print(summary(dat))
  return(dat)
}

brms_41ex <- function(){
  # ref https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf heading 4.1:
  system("g++ -v")
  library("brms")
  data("kidney")
  head(kidney, n = 3)
  # summary(kidney) # pre-assessing the sample before choosing priors is frowned upon in the Bayesian approach.
  fit1 <- brm(formula = time | cens(censored) ~ age * sex + disease
    + (1 + age|patient),
    data = kidney, family = lognormal(),
    prior = c(set_prior("normal(0,5)", class = "b"),
      set_prior("cauchy(0,2)", class = "sd"),
      set_prior("lkj(2)", class = "cor")),
    warmup = 1000, iter = 2000, chains = 4,
    control = list(adapt_delta = 0.95))
  summary(fit1, waic = TRUE)
  hypothesis(fit1, "Intercept - age > 0", class = "sd", group = "patient")
  fit2 <- update(fit1, formula. = ~ . - (1 + age|patient) + (1|patient))
  LOO(fit1, fit2)
  return()
}
mapAdversity <- function(prophylaxisVec, patternSeq=c(".*", "Ami37x", "Ami25x", "Ami10x"),
  xSeq=c("0nil", "3moderate", "1mild", "0nil")){ # "Ami10x" included 2023Nov28
  # (Safety) Adversity during prescription regimes:
  #   mild where Ami25* is present, moderate where Ami37* is present, nil otherwise.
  #   was till 2023Nov17: mild during 1Ami25*, moderate during 3Ami37*, nil otherwise.
  stopifnot(is.factor(prophylaxisVec))
  stopifnot(nlevels(prophylaxisVec) == (7+1+1+1)) # coz levels beyond need to be mapped here.
    # +1 2024Jan18; 7+1+1 changed 2023Dec18
  advers <- prophylaxisVec

  advers.levels.toBe <- levels(prophylaxisVec)
  patternSeq.which <- lapply(patternSeq,
    FUN=function(patternSeq.i) grep(pattern=patternSeq.i, x=advers.levels.toBe)
  )
  stopifnot(length(xSeq) == length(patternSeq))
  for(i in 1:length(xSeq)){
    advers.levels.toBe[patternSeq.which[[i]]] <- xSeq[i]
  }
  # advers.levels.toBe <- c("1mild", "3moderate");
  #   advers.levels.toBe <- c(advers.levels.toBe,
  #     rep("0nil", times=(nlevels(advers) - length(advers.levels.toBe))))

  levels(advers) <- advers.levels.toBe
  advers <- relevel(x=advers, ref="0nil")
    # was: levels(advers) <- reorder(levels(advers), c(2, 3, 1))
  return(advers)
}
mapCarry_v2 <- function(dat, periodCarry=c_width, periodCarrySeq=c_carry_width, grp=c(0, 1),
  mustFactorize=FALSE){ # created 2023Dec25. alt: grp=c(FALSE, TRUE). alt: dat$date
  eDays_carry_startVec <- unlist(lapply(levels(dat$prophylaxis),
    FUN=function(proph_i){
      min(dat[dat$prophylaxis == proph_i, "eDays"])
    }
  ))
  eDays_carry_endVec <- eDays_carry_startVec + periodCarry - 1
    # alt: sapply(eDays_carry_startVec + periodCarry - 1, FUN=function(x) min(nrow(dat), x))
    eDays_carry_endVec[nrow(dat) < eDays_carry_endVec] <- nrow(dat) # print(eDays_carry_endVec)
  eDays_carrySeq_endVec <- eDays_carry_startVec + periodCarrySeq - 1
    eDays_carrySeq_endVec[nrow(dat) < eDays_carrySeq_endVec] <- nrow(dat) # print(eDays_carrySeq_endVec)
  carryBool <- rep(grp[1], times=nrow(dat)) # by default
  carrySeq <- rep(0, times=nrow(dat)) # init
  lapply(1:length(eDays_carry_startVec),
    FUN=function(i){
      proph_i_range <- eDays_carry_startVec[i] : eDays_carry_endVec[i]
      proph_i_carrySeq_range <- eDays_carry_startVec[i] : eDays_carrySeq_endVec[i]
      carryBool[proph_i_range] <<- grp[2] # coz global to this FUN block; so not <-
      carrySeq[proph_i_carrySeq_range] <<- dat[proph_i_carrySeq_range, "eDays"] - eDays_carry_startVec[i] + 1
    }
  )
  if(mustFactorize){
    carryBool <- as.factor(carryBool)
  } # else continue
  carryBool.carrySeq <- list(carryBool, carrySeq)
  return(carryBool.carrySeq)
}
mapCarry <- function(dat, periodCarry=c_width, grp=c(0, 1), mustFactorize=FALSE){ # alt: grp=c(FALSE, TRUE)
  # alt: dat$date
  eDays_carry_startVec <- unlist(lapply(levels(dat$prophylaxis),
    FUN=function(proph_i){
      min(dat[dat$prophylaxis == proph_i, "eDays"])
    }
  ))
  eDays_carry_endVec <- eDays_carry_startVec + periodCarry - 1
    # alt: sapply(eDays_carry_startVec + periodCarry - 1, FUN=function(x) min(nrow(dat), x))
  eDays_carry_endVec[nrow(dat) < eDays_carry_endVec] <- nrow(dat)
  # print(eDays_carry_endVec)
  carry <- rep(grp[1], times=nrow(dat)) # by default
  lapply(1:length(eDays_carry_startVec),
    FUN=function(i){
      carry[eDays_carry_startVec[i] : eDays_carry_endVec[i]] <<- grp[2] # coz global to this FUN block; so not <-
    }
  )
  if(mustFactorize){
    carry <- as.factor(carry)
  } # else continue
  return(carry)
}
fac2num <- function(f){
  stopifnot(is.factor(f))
  # ref https://stackoverflow.com/questions/4798343/convert-factor-to-integer
  as.numeric(levels(f))[f] # is recommended and slightly more efficient than as.numeric(as.character(f))
}
mapSeverityGrp_v2 <- function(dat, cuts, grp){
  dSeverityGrp <- fac2num(dat$severityMax)
  dSeverityGrp.notNA <- (! is.na(dSeverityGrp))
  # stopifnot(all(is.integer(dSeverityGrp)))
  # levels(dSeverityGrp) <- c("0", rep("123", times=3), rep("45678", times=5))
  dSeverityGrp[dSeverityGrp.notNA & (cuts[2] < dSeverityGrp)] <- grp[3]
  dSeverityGrp[dSeverityGrp.notNA & (cuts[1] < dSeverityGrp) & (dSeverityGrp <= cuts[2])] <- grp[2]
  dSeverityGrp[dSeverityGrp.notNA & (dSeverityGrp <= cuts[1])] <- grp[1]
  return(dSeverityGrp)
}
mapSeverityGrp_v1 <- function(dat){
  dSeverityGrp <- dat$severityMax
  levels(dSeverityGrp) <- c("0", rep("123", times=3), rep("45678", times=5))
  return(dSeverityGrp)
}
prescription2int <- function(prescVec, pattern){
  as.integer(sub(pattern=pattern, replacement="\\1", x=prescVec))
}
prescription2fac.int <- function(prescVec, ref, pattern="^([0-9]+).*"){
  presc.int <- prescription2int(prescVec, pattern)
  presc.fac0 <- factor(prescVec)
  presc.levels.int <- prescription2int(levels(presc.fac0), pattern)
  presc.fac1 <- factor(presc.fac0,
    levels=levels(presc.fac0)[order(presc.levels.int, decreasing=FALSE)]
  )
  presc.fac2 <- relevel(presc.fac1, ref=ref)
  # dat$prophylaxis <- relevel(factor(dat$prescription, levels=unique(dat$prophylaxis_int)), ref=ref)
    # was till 2023Dec17: dat$prophylaxis <- relevel(as.factor(dat$prescription), ref=ref)
    # was till 2023Nov19: as.factor(dat$prescription) # renamed too
  ans <- list(presc.fac2, presc.int)
  return(ans)
}
myRead_sevDura <- function(fName=c_sevDura_Filename, tryFormats=c_tryFormats,
  sevDura_0nil_default=c_sevDura_0nil_default, ref="3Ami37x001",
  periodCarry=c_width, cuts=c(0, 3), grp=(1 + c(0, 2, 5)), # alt: grp=(0 + c(0, 123, 45678))
  severityMax.levels=0:10 # added 2023Dec03
){
  dat <- read.csv(file=fName)
  dat$date <- as.Date(dat$date, tryFormats=tryFormats)
  dat$start24HMfrac <- getHMfrac(dat$start24H)
  presc.fac.int <- prescription2fac.int(dat$prescription, ref=ref)
  dat$prophylaxis <- presc.fac.int[[1]]
  dat$prophylaxis_int <- presc.fac.int[[2]]
    # was till 2023Dec03: x=dat$prophylaxis))
  dat$siteTemp <- as.factor(dat$siteTemp)
  dat$medSOS <- as.factor(dat$medication_fac)
  dat$triggerTravel_fac <- as.factor(dat$triggerTravel_fac)
  dat$advers <- mapAdversity(dat$prophylaxis)
  dat$eDays <- dat$date - (min(dat$date) - 1) # alt: c(NA, diff(dat$date, lag=1, differences=1))
  carryBool.carrySeq <- mapCarry_v2(dat, periodCarry=periodCarry)
    dat$carry <- carryBool.carrySeq[[1]]; dat$carrySeq <- carryBool.carrySeq[[2]]
  dat$durationH[is.na(dat$durationH) & (dat$siteTemp == "0nil")] <- sevDura_0nil_default
  dat$severityMax_asis <- dat$severityMax
  dat$severityMax[is.na(dat$severityMax) & (dat$siteTemp == "0nil")] <- sevDura_0nil_default

  dat[, paste0("carry_", "severityMax", periodCarry)] <- c(rep(NA, times=(periodCarry - 1)),
    rollapply(dat$severityMax, width=periodCarry, FUN=myRollFunc, align="right")
  )
  dat[, paste0("carry_", "severityMax", 2*periodCarry)] <- c(rep(NA, times=(2*periodCarry - 1)),
    rollapply(dat$severityMax, width=2*periodCarry, FUN=myRollFunc, align="right")
  )
    # before any factor(), [so the sequential model can be understood as a sequence of conditionally
    # independent Bernoulli trials ...]
  dat$severityMax <- factor(x=dat$severityMax, levels=severityMax.levels, ordered=TRUE)
    # added levels=severityMax.levels 2023Dec03, not as.factor()
  dat$severityMaxGrp <- mapSeverityGrp_v2(dat, cuts=cuts, grp=grp)
    dat$severityMaxGrp <- factor(x=dat$severityMaxGrp, ordered=TRUE) # not as.factor()
    # maybe before any factorizing of severityMax.  Was mapSeverityGrp_v1()
  str(dat)

  savepar <- par(ask=TRUE)
    # plot(medSOS ~ eDays, data=dat)
    # plot(severityMax ~ prophylaxis, data=dat)
    plot(advers ~ prophylaxis, data=dat)
    plot(prophylaxis ~ severityMaxGrp, data=dat)
  par(savepar)
  return(dat)
}
brms_48ex <- function(){
  # ref https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf heading 4.8 (page 13):
  # [brms allows changes to this basic model in at least three ways. First of all, three additional
  # ordinal families are implemented. Families sratio (stopping ratio) and cratio (continuation ratio) are so
  # called sequential models (Tutz 1990). Both are equivalent to each other for symmetric link
  # functions such as logit but will differ for asymmetric ones such as cloglog. The fourth
  # ordinal family is acat (adjacent category) also known as partial credits model (Masters 1982;
  # Andrich 1978b). Second, restrictions to the thresholds can be applied. By default, thresholds
  # are ordered for family cumulative or are completely free to vary for the other families. This
  # is indicated by argument threshold = "flexible" (default) in brm. Using threshold =
  # "equidistant" forces the distance between two adjacent thresholds to be the same
  # Third, the assumption that predictors have constant effects across categories may be relaxed
  # for non-cumulative ordinal models ... leading to category specific effects.]

    # library(parallel)
    # detectCores() # chain=1, cores= and threading(...) as per this?
  system("g++ -v")
  library("brms")
  data("inhaler")
  head(inhaler, n = 3)
  # summary(inhaler) # pre-assessing the sample before choosing priors is frowned upon in the Bayesian approach.
  fit3 <- brm(formula = rating ~ treat + period + carry + (1|subject),
    data = inhaler, family = cumulative)
  fit4 <- brm(formula = rating ~ period + carry + cs(treat) + (1|subject),
    data = inhaler, family = sratio, threshold = "equidistant",
    prior = set_prior("normal(-1,2)", coef = "treat"))
  summary(fit4, waic = TRUE)
  plot(fit4)
    # [The treatment effect seems to
    # be strongest between category 3 and 4. At the same time, however, the credible interval is also
    # much larger. In fact, the intervals of all three effects of treat are highly overlapping, which
    # indicates that there is not enough evidence in the data to support category specific effects.
    # On the bottom of the output, parameter delta specifies the distance between two adjacent
    # thresholds and indeed the intercepts differ from each other by the magnitude of delta.]
  return()
}
is.myLabel.covar <- function(labl, patn="^sev[0-9][0-9][.][0-9][0-9][.].+"){
  # eg 02.02.abc1 likely has covar by naming convention
  doesIt <- 1 <= length(grep(pattern=patn, x=labl))
  return(doesIt)
}
view_brms_fits <- function(fits, ix=NULL, side=4, cex=c(0.5, 1)[1], ask=c(FALSE, TRUE)[1]){
  # added cex= 2024Jan28. added ask= 2024Jan24. added myLabels=, side= 2023Dec04
  # require(ggplot2)
  ixVec <- if(is.null(ix)) 1:length(fits) else ix
  myLabels <- names(fits) # names(fits[ixVec])
  stopifnot(class(ixVec) %in% c("integer", "numeric"))
  lapply(ixVec, FUN=function(i){
    myLabels_i <- myLabels[i]; print(paste0("Viewing ", myLabels_i))
    fit_i <- fits[[i]]; print(waic(fit_i)) # alt: waic(fit_i, fit_j ..., compare=TRUE)
      # requires same observations' count. Wonder if ... could be a list
    print(summary(fit_i, waic=TRUE)) # did not show WAIC
    savepar <- par(ask=ask) # alt: plot(..., ask=TRUE)
      plot(fit_i) #; title(sub=myLabels_i, cex.sub=cex); mtext(myLabels_i, side=side, cex=cex) # , main=myLabels_i)
        # was till 2023Dec22: plot(fit_i); mtext(myLabels_i, side=side) # plot.new()
        # ref https://cran.r-project.org/web/packages/brms/vignettes/brms_distreg.html:
      if(is.myLabel.covar(myLabels_i)){
        fit_i_condeff <- conditional_effects(fit_i, categorical=TRUE)
        plot(fit_i_condeff, points=TRUE) #; mtext(myLabels_i, side=side, cex=cex)
        # plot(conditional_effects(fit_i, categorical=TRUE), points=TRUE, main=myLabels_i)
      } # else continue coz [! No valid effects detected] 2024Jan24
      # alt: tryCatch() ref https://stackoverflow.com/questions/8093914/use-trycatch-skip-to-next-value-of-loop-upon-error
        # was till 2023Dec22: plot(conditional_effects(fit_i, categorical=TRUE), points=TRUE); mtext(myLabels_i, side=side)
        # sp2 + geom_text(x=3, y=30, label="Scatter plot")
        # sp2 + annotate(geom="text", x=3, y=30, label="Scatter plot", color="red")
        # conditional_effects(fit_i, categorical=TRUE, points=TRUE); mtext(myLabels_i, side=side)
        # marginal_effects(fit_i, categorical=TRUE)
        # [Method 'marginal_effects' is deprecated. Please use 'conditional_effects']
      # color = ordered(cyl); scale_color_brewer(palette = "Dark2")
      # https://colorbrewer2.org/?type=qualitative&scheme=Paired&n=11
      # plot(conditional_effects(fit_i, categorical=TRUE), points=TRUE, plot=FALSE)[[1]] +
      #   scale_color_grey() + scale_fill_grey()
      # plot(conditional_effects(fit_i, categorical=TRUE), points=TRUE, plot=FALSE)[[1]] +
      #   scale_colour_gradientn(colours = terrain.colors(1+10)) + scale_fill_gradientn(colours = terrain.colors(1+10))
      #   # [Error: Discrete value supplied to continuous scale]
      # plot.new() # [In doTryCatch(return(expr), name, parentenv, handler) : invalid graphics state]
      # plot(brms::pp_check(fit_i)) # mtext(myLabels_i, side=side) # type="xyz". ecdf_overlay
      view_brms_fits2.1(fit_i, myLabels_i)
#      plot(brms::pp_check(fit_i, ndraws=100, type='ecdf_overlay')) # better for categorical
#      plot.new()
#      plot(brms::pp_check(fit_i, type="stat_2d")) # color_scheme_set("purple")
        # pp_check() returns ggplot2 object, and so maybe, plot() plots it.
        # brms::pp_check() coz there's bayesplot::pp_check() that doesn't plot!
        # ref https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html
        # ref https://bayesball.github.io/BRMS/federalist-paper-study.html
      # plot(pairs(fit_i)) # mtext(myLabels_i, side=side) doesn't work on brms plots!
    par(savepar)
  })
  return()
}
view_brms_fits2.dontUse <- function(fits, ixVec){
  lapply(ixVec, FUN=function(ix){
    fit_i <- fits[[ix]]
    print("Starting next:")
    plot(brms::pp_check(fit_i)) # mtext(myLabels[i], side=side) # type="xyz". ecdf_overlay
    plot(brms::pp_check(fit_i, type="stat_2d")) # color_scheme_set("purple")
    # brms::pp_check() to avoid default bayesplot::pp_check() that doesn't plot!
    # ref https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html
    # ref https://bayesball.github.io/BRMS/federalist-paper-study.html
    # plot(pairs(fit_i)) # mtext(myLabels[i], side=side) doesn't work on brms plots!
  })
  return()
}
view_brms_fits2.1 <- function(fit_i, myLabels_i, ndraws=NULL){ # alt: ndraws=100
  # myLabels <- names(fits)
#  lapply(ixVec, FUN=function(ix){
    # fit_i <- fits[[ix]]; myLabels_i <- myLabels[ix] # or <-ix?
    # print("Starting next:")
  # plot.new()
  plot(brms::pp_check(fit_i, ndraws=ndraws, type='ecdf_overlay')) # + labs(tag=myLabels_i) # type="xyz". ecdf_overlay
#  plot.new() # when in between plot(pp_check()), this seems to crash knitr()!
  plot(brms::pp_check(fit_i, type="stat_2d")) # + labs(tag=myLabels_i)
  plot.new()
    # color_scheme_set("purple")
    # brms::pp_check() to avoid default bayesplot::pp_check() that doesn't plot!
    # ref https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html
    # ref https://bayesball.github.io/BRMS/federalist-paper-study.html
    # plot(pairs(fit_i)) # mtext(myLabels[i], side=side) doesn't work on brms plots!
#  })
  return()
}
NbyCol <- function(dat, colname.which="prescription", colname.NA=paste0("carry_", "severityMax", c_width)){
  prescription_levelsLike_unique <- unique(dat[[colname.which]])
  plui.countWithNA.without <- do.call(rbind, lapply(prescription_levelsLike_unique,
    FUN=function(plui){
      dat.plui <- dat[dat[[colname.which]] == plui, ] # was: colname.which]
      countWithNA.without <- c(nrow(dat.plui), length(na.omit(dat.plui[[colname.NA]])))
      return(countWithNA.without)
    }
  ))
  return(plui.countWithNA.without)
}
prophylaxis.effect.sevDura.sample_v2 <- function(dat, initProphylaxis="1Ami25x001", measure="Median",
  betterBy=c_betterBy, digits=2){ # beware: _v2() now uses chr $prescription, rather than possibly-factor $prophylaxis
  prescription_levelsLike_unique <- unique(dat$prescription)
  presc.summ <- lapply(prescription_levelsLike_unique, # was: levels(dat$prescription),
    FUN=function(presc) summary(na.exclude(coredata(dat$sevDura[dat$prescription == presc])))
  )
  names(presc.summ) <- prescription_levelsLike_unique # was: levels(dat$prescription)
  plui.countWithNA.without <- NbyCol(dat)
  presc.summ.df <- cbind(round(do.call(rbind, presc.summ), digits=digits),
    CountWithNA=plui.countWithNA.without[[1]], CountWithoutNA=plui.countWithNA.without[[2]])
      # was: summary(dat$prescription))
  colnames(presc.summ.df) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max", "CountWithNA", "CountWithoutNA")
  print(presc.summ.df)
  USL <- round(presc.summ.df[rownames(presc.summ.df) == initProphylaxis, measure] / (1 - betterBy),
    digits=digits)
  presc.summ.df_USL <- list(presc.summ.df, USL)
  return(presc.summ.df_USL)
}
prophylaxis.effect.sevDura.sample <- function(dat, initProphylaxis="1Ami25x001", measure="Median",
  betterBy=c_betterBy, digits=2){
  presc.summ <- lapply(levels(dat$prescription),
    FUN=function(presc) summary(na.exclude(coredata(dat$sevDura[dat$prescription == presc])))
  )
  names(presc.summ) <- levels(dat$prescription)
  presc.summ.df <- cbind(round(do.call(rbind, presc.summ), digits=digits),
    count=summary(dat$prescription))
  colnames(presc.summ.df) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max", "CountWithNA")
  print(presc.summ.df)
  USL <- round(presc.summ.df[rownames(presc.summ.df) == initProphylaxis, measure] / (1 - betterBy),
    digits=digits)
  presc.summ.df_USL <- list(presc.summ.df, USL)
  return(presc.summ.df_USL)
}
acheDpriors <- function(dat1, posts.prescription=c("1Ami25x001", "8Top50x101_sosNaxRiz"), # was: "8Top25x101_sosNaxRiz"),
  posts.betterBy=c(c_betterBy, c_betterBy / 2), measure="Median", prefix="0pre"){
  set.seed(1234) # for reproducibility
  ach22 <- list(); ach22.USL <- list()
  for(ppi in 1:length(posts.prescription)){
    ach22[[ppi]] <- subset(dat1, prescription %in% posts.prescription[ppi]) # not factor prophylaxis == posts.prescription
      # c("date", "severityMin", "severityMax", "prescription", "sevDura", "prophylaxis", "advers", "eDays",
      #   "prophylaxis_int", "carry", "(severityMaxGrp") # need adjusting
    print(summary(ach22[[ppi]]$severityMax_asis))
    ach22[[ppi]]$prescription <- paste0(prefix, ach22[[ppi]]$prescription) # name the corresponding prior
    presc.summ.df_USL <- prophylaxis.effect.sevDura.sample_v2(ach22[[ppi]], # dat1,
      initProphylaxis=posts.prescription[ppi], measure=measure)
    ach22.USL[[ppi]] <- presc.summ.df_USL[[2]] # alt: ach22$severityMax_asis / (1 - posts.betterBy)
    if(FALSE){
      # ref https://cran.r-project.org/web/packages/brms/brms.pdf: 
      #   mean mu in [0,1] [= alpha * beta mean probability of trial success.]
      #   precision phi > 0 [= (1 - mu) * beta precision or over-dispersion, component.]
      dArgs <- list(
        c(n=21, size=10, mu=0.11/(1-.4), phi=0.5),
        c(n=21, size=10, mu=0.4, phi=0.6))[[1]]
      duh <- rbeta_binomial(n=dArgs["n"], size=dArgs["size"], mu=dArgs["mu"], phi=dArgs["phi"]); duh; summary(duh)
        # rbeta_binomial(n=dArgs$n, size=dArgs$size, mu=dArgs$mu, phi=dArgs$phi)

      data=c(23,45,21,34,5) 
      # get 10 random elements with probability 
      print(sample(data, size = 10, replace = TRUE, prob = c(0.6,0.1,0.1,0.1,0.1)))
    }
# beware: the following could exceed 1--10 scale!
    ach22[[ppi]]$severityMax_asis <- ach22.USL[[ppi]] * ach22[[ppi]]$severityMax_asis # rnorm()
    # data=dat_long, prior=prior_TBD, inits=0
  }
  ach22.df <- do.call(rbind, ach22)
  return(ach22.df)
}
myDistribs <- function(){
NK<-1;sizeSev<-10;rbinom(NK,size=sizeSev,prob=rep(1/(sizeSev+1),times=sizeSev+1))
  # brm( n_cited | trunc(lb = 0) ~ 1 + year, seed = 1024 )
  inc_warmup <- isTRUE(object$fit@sim$n_save[1] > niterations(object))
  draws <- as.array(object, inc_warmup = inc_warmup)
### an overview on parameters and parameter classes to define priors on
get_prior(...)
launch_shiny(fit1)

  # ref https://www.magesblog.com/post/2018-08-02-use-domain-knowledge-to-review-prior-predictive-distributions/:
  # [instead of fitting the model, I set the parameter sample_prior = "only" to generate samples from the prior
  # predictive distribution only, i.e. the data will be ignored and only the prior distributions will be used. ...
  # Generate posterior samples ... To fit my model with the data I call update and set the parameter
  # sample_prior="no". Note, the model doesn’t need to be recompiled.]

  # https://paul-buerkner.github.io/brms/reference/hypothesis.brmsfit.html:
  # [When interpreting Bayes factors, make sure that your priors are reasonable and carefully chosen, as the
  # result will depend heavily on the priors. In particular, avoid using default priors. ...
  # Although this allows testing of hypotheses in a similar manner as in the frequentist null-hypothesis
  # testing framework, we strongly argue against using arbitrary cutoffs (e.g., p < .05) to determine the
  # 'existence' of an effect.]

  # ref https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Multinom.html
  # rmultinom(n=20, size=30, prob)
  # rmultinom(10, size = 12, prob = c(0.1,0.2,0.8)) # multinomial Dirichlet ... for topic modeling too
  # dmultinom(x, size = NULL, prob, log = FALSE)
  return()
}
sampleDist <- function(n, prob, replace=T){
  x <- 1:length(prob)
  # adapted from https://stats.stackexchange.com/questions/67911/how-to-sample-from-a-discrete-distribution
  sample(x = x, n, replace = replace, prob = prob) 
}
gen_ach22_v2 <- function(n=12*30){
stop()
}
gen_ach22 <- function(n=21, prob=c(0.0000000, 0.5821119, 0.2258439, 0.1796816),
  grp=c("0", "123", "45678", "9X"), prescription="0asis2022"){
  # sev22 <- gen_ach22(n=21*2)
  samdi <- sampleDist(n=n, prob=prob)
  sev22 <- as.data.frame(cbind(samdi, severityMaxGrp=grp[samdi], prescription=prescription))
  sev22$severityMax <- as.integer(sev22$samdi)
  sapply(sort(unique(sev22$severityMax), decreasing=TRUE),
    FUN=function(sevu){
      sub.must <- which(sev22$severityMax == sevu)
      sub.toBe <- switch(sevu, 0, 3:1, 8:4, 10:9)
      sub.toBe.ix <- seq_len(min(length(sub.toBe), length(sub.must))); print(sub.toBe.ix)
        # seq(from=8, to=4, by=-1, 
      sev22$severityMax[sub.must] <<- sub.toBe[sub.toBe.ix] + 0 * sub.must
        # <<- coz global. was: sev22$severityMax[sub.must] <- 10:9 + 0 * sub.must
        # ref https://www.geeksforgeeks.org/vector-recycling-in-r/.
        # Warning: [longer object length is not a multiple of shorter object length]
    }
  )
    # c("date", "severityMin", "severityMax", "prescription", "sevDura", "prophylaxis", "advers", "eDays",
    #   "prophylaxis_int", "carry", "(severityMaxGrp") # need adjusting
  return(sev22)
}
logOdds2prob <- function(coef){
  # coef == log(p / (1-p)). => p == (1-p) * exp(coef). => p == exp(coef) / (1+exp(coef))
  return(exp(coef) / (1 + exp(coef)))
}
coef2prob <- function(coefs, coefsNames, represents=c("cumulative_logit"),
  patn.b_="^b_", patn.b_I=paste0(patn.b_, "Intercept")){
  f1 <- function(patn.b_Iix, patn.b_Xix){
    lapply(patn.b_Iix, FUN=function(Iix)
      sapply(patn.b_Xix, FUN=function(Xix) logOdds2prob(coefs[Xix] - coefs[Iix]))) # T(i+1) - T(i) matters for cum
  }
  f2 <- function(patn.b_Iix, patn.b_Xix){ # added 2023Dec03
    lapply(patn.b_Iix, FUN=function(Iix)
      sapply(patn.b_Xix, FUN=function(Xix) logOdds2prob(coefs[Xix]) - logOdds2prob(coefs[Iix])))
  }
  fn <- list(f1, f2)[[2]]
  coefs.p <- switch(represents,
    cumulative_logit = {
      patn.b_Iix <- grep(pattern=patn.b_I, coefsNames)
      patn.b_Xix <- setdiff(grep(pattern=patn.b_, coefsNames), patn.b_Iix)
      coef.p.I <- sapply(patn.b_Iix, FUN=function(Iix) logOdds2prob(coefs[Iix]))
      coef.p.X <- do.call(cbind, fn(patn.b_Iix, patn.b_Xix))
      ans <- as.data.frame(cbind(c(patn.b_I, coefsNames[patn.b_Xix]), (rbind(coef.p.I, coef.p.X))))
      colnames(ans) <- c("coefsNames", coefsNames[patn.b_Iix])
      for(cix in coefsNames[patn.b_Iix]){
        ans[, cix] <- as.numeric(ans[, cix])
      }
      ans # as.numeric()
    },
    stop(paste0("Unsupported ", represents))
  )
  # names(coefs.p) <- coefsNames
  return(coefs.p)
}
read.bayeSpec <- function() {
  bayeSpec <- read.csv(file=file.path(cRoot, c_bayeSpec))
  bayeSpec <- bayeSpec[bayeSpec$wanted == 1,]; tail(bayeSpec); dim(bayeSpec)
  return(bayeSpec)
}
runSpec <- function(dat1) {
  dat.mdl <- dat1 # alt: subset(dat1, prescription %in% posts.prescription[1]); str(dat.mdl)
  bayeSpec <<- read.bayeSpec() # beware: global via <<-
  if(FALSE){
    # https://mc-stan.org/cmdstanr/articles/cmdstanr.html
    # we recommend running this is a fresh R session or restarting your current session
    # install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
    library(cmdstanr)
    check_cmdstan_toolchain()
    cmdstanr::check_cmdstan_toolchain(fix = TRUE)
    install_cmdstan()
  } # else continue
  # ref https://discourse.mc-stan.org/t/brms-cmdstanr-update-the-desired-updates-require-recompiling-the-model/26876/5
  library(cmdstanr)
  options(cmdstanr_write_stan_file_dir = getwd()) # rstan default backend
  # backend = getOption("brms.backend", "rstan") # https://cran.r-project.org/web/packages/brms/brms.pdf
  # backend = "cmdstanr" # in brm()
  # ref https://github.com/paul-buerkner/brms/issues/1135
  c_file_refit <- list("on_change", "never")[[1]]
  options("brms.file_refit"=c_file_refit); getOption("brms.file_refit", "never") # default "never"
  c_recompile <- list(NULL, TRUE)[[1]] # beware: T 2 forces recompiling! also c(NULL, TRUE) == TRUE!!
  dat.mdl0 <- cbind(dat.mdl, sevMax0=(dat.mdl$severityMax_asis == 0)) # beware NAs
  dat.mdl.bak <- dat.mdl; dat.mdl <- dat.mdl0
  fits <- list() # if(! exists("fits")) fits <- list() # iff directly global <-
  c_seed <- as.integer(tail(dat.mdl$date, 1)); c_seed
  # alt: sub(pattern="^ - (.*)-.*$", replacement="\\1", x=c_gsFilename_which)
  # was: c_seed <- c(20240107, 20231228)[1] # yyyymmdd for reproducibility
  set.seed(c_seed)
  for(spec.i in 1:nrow(bayeSpec)){
    bayeSpec.i <- bayeSpec[spec.i,]; print(bayeSpec.i)
    fmla <- formula(rlang::parse_expr(bayeSpec.i$formula)) # was till 2024Jan08: fmla <- bayeSpec$formula[spec.i]
    fly <- eval(rlang::parse_expr(paste0(bayeSpec.i$family, "(\"", bayeSpec.i$link, "\")")))
    fits[[bayeSpec.i$id]] <- brm(formula=fmla,
      data=dat.mdl, family=fly, backend="cmdstanr", # was till 2024Jan08: family=bayeSpec$family[spec.i],
      # file_refit = getOption("brms.file_refit", "never"), recompile=c_recompile,
      save_model=file.path(cRoot, paste0(bayeSpec.i$id, ".txt")), file=file.path(cRoot, bayeSpec.i$id)
    )
  }
  # ref https://adv-r.hadley.nz/expressions.html http://adv-r.had.co.nz/Expressions.html
  # rlang::parse_exprs() if you have multiple expression separated by ; or \n. It returns a list.
  # fmla <- brmsformula(severityMax | thres(10 - 1, gr=(! sevMax0)) ~ 1) # , severityMax | subset(sevMax0) ~ 1)
  # alt: [yi | trunc(lb = 0, ub = 100) ~ predictors] ref R> ?brmsformula
  # also for modeling $duration alongside, mvbrmsformula()
  # or define a multivariate model: bf(mvbind(y1, y2) ~ x * z + (1|g))
  # bf(severityMax_asis~1, autocor=~arma(time=carrySeq, gr=prophylaxis, p=1,q=1, cov=TRUE))
  #   [Error: Please set cov = TRUE when modeling MA structures for this family.]
  #   [Error: Covariance formulation of ARMA structures is only possible for effects of maximal order one.]
  # =bf(severityMax_asis~1, hu~0),
  # =bf(severityMax_asis~1+advers, hu~0+prophylaxis),
  # =bf(severityMax_asis~1+prophylaxis, hu~0+advers),
  # =bf(severityMax_asis~1+prophylaxis, hu~0+advers+(1|prophylaxis)),
  # =bf(severityMax_asis|thres(10-1)~1+advers+prophylaxis, hu~0+advers+(1|prophylaxis)),
  # =bf(severityMax_asis | thres(10 - 1) ~ 1 + advers + prophylaxis, hu ~ 0 + advers + prophylaxis),
  # =bf(severityMax_asis | thres(10 - 1) ~ 1 + advers * prophylaxis, hu ~ 0 + advers * prophylaxis),
  
  return(fits)
}
merge.list.my <- function(list1, list2){
  list12 <- c(list1, list2)
    # list(unlist(list1, recursive=FALSE), unlist(list2, recursive=FALSE))
  return(list12)
}
update.1pro <- function(bayeSpec, suffix=".1pro", fits, dat.mdl) {
  warning("not fits but loops bayeSpec")
  fits.new <- list() # init
  for(spec.i in (1:nrow(bayeSpec))[]){
    # grep(pattern="^sev[0-9]+[.][0-9]+$", x=fits.names)[5:6]){
    id0 <- bayeSpec$id[spec.i]; id0.x <- paste0(id0, suffix); id0; id0.x
    fits.new[[id0.x]] <- update(fits[[id0]], formula. = ~ . + prophylaxis,
      newdata=dat.mdl, backend="cmdstanr", # recompile=c_recompile,
      save_model=file.path(cRoot, paste0(id0.x, ".txt")), file=file.path(cRoot, id0.x)
    )
  }
  return(fits.new)
}
updateFits.tf.ca <- function(id0.n, suffix, fits, dat.mdl) {
  fits.new <- list() # init
  for(id0 in id0.n){
    id0.x <- paste0(id0, suffix); id0; id0.x
    fits.new[[id0.x]] <- update(fits[[id0]], formula. = ~ . + triggerTravel_fac + carry,
      newdata=dat.mdl, backend="cmdstanr", # recompile=c_recompile,
      save_model=file.path(cRoot, paste0(id0.x, ".txt")), file=file.path(cRoot, id0.x)
    )
  }
  return(fits.new)
}
rethinkPlots <- function(dat1) {
  ## 0,2. Bayesian Distributional Rethinking with uncertainty:
  plot(ecdf(dat1$severityMax_asis)) # beware: $severityMax differs!
  plot(medSOS ~ severityMax, data=dat1[dat1$siteTemp != "0nil",])
  # higher sev attracted likelier medSOS; seems understandable, but this is not uniformly proportional
  # (in those high sev cases).  OK to include for sevGrp; but for sev as is, it needs care.
  plot(severityMaxGrp ~ carry, data=dat1[,]) # plot(severityMax ~ carry, data=dat1[,])
  # sev0 proportion drops with carry=1 and sev45678 rises!  That's not uniformly proportional over ~7 weeks.
  # So, not OK; exclude and separately investigate.
  plot(carrySeq ~ severityMaxGrp, data=dat1[,]) # plot(severityMax ~ carry, data=dat1[,])
  ## 4. Headache Time Series:
  # include $carry_severityMax7,14
  plot(severityMax ~ carry_severityMax7, data=dat1)
  # sev0 seems to shrink with increasing carry_sev*
  plot(severityMax ~ carry_severityMax14, data=dat1)
}
gen.loos <- function(fits.names.sel, fits) {
  loos <- list()
  for(fi in fits.names.sel){
    print(fi); loos[[fi]] <- loo(fits[[fi]])
    # LOO() # moment_match=TRUE # Use if Pareto k diagnostic values are high
  }
  return(loos)
}
duhFUN <- function(dat1, posts.prescription=c_posts.prescription){
  dat.mdl <- dat1
  if(! exists("fits")) fits <- list() # iff directly global <-
  fits <- runSpec(dat.mdl); fits.names <- names(fits); fits.names # [nchar(names(fits)) > 0]

  suffix <- ".1pro"; patn.which <- grep(pattern="^02.02$", x=fits.names)
  fits.new <- update.1pro(bayeSpec[patn.which,], suffix, fits, dat.mdl)
  fits <- merge.list.my(fits, fits.new); fits.names <- names(fits); fits.names
  
  suffix <- ".tf.ca"; id0.n <- fits.names[grep(pattern="[.]1$", x=fits.names)]
    # was till 2024Jan22: "sev02.02.1" # added 2024Dec25
  fits.new <- updateFits.tf.ca(id0.n, suffix, fits, dat.mdl)
  fits <- merge.list.my(fits, fits.new); fits.names <- names(fits); fits.names

  # loos <- list()
  patn <- c("^sev02[.]02.*", "^sev09[.]02.*")[2]; patn.which <- grep(pattern=patn, fits.names); patn.which
  for(fi in fits.names[patn.which]){
    print(fi); loos[[fi]] <- loo(fits[[fi]])
  }
  loo_compare(loos[fits.names[patn.which]]) # c("sev02.02.1.tf.ca", "sev09.02.1")
  # [(0.7, 1]   (bad)        2    0.6%   109 ... See help('pareto-k-diagnostic') for details.]
  # [Diagnostics for Pareto smoothed importance sampling (PSIS)]
  loo::pareto_k_ids(loos[["sev09.02.1"]]) # [[1] 315 331] observations

fits.names <- names(fits); fits.names # [nchar(names(fits)) > 0]
fits.names.which <- list(seq_along(fits.names), grep(pattern="[.][0-9]$", fits.names))[[1]]; fits.names.which
# stop("turn off brm( drop unused levels). even 10.01.1 b_pro CI overlaps 0, except 3Ami and 8/9Top")
ix <- list(fits.names.which[], spec.i)[[1]]; view_brms_fits(fits, ix=ix)
  # was: ix=fits.names[fits.names.which]
  # was till 2023Dec03: sapply(fits.names[fits.names.which], FUN=function(ix) view_brms_fits(fits, ix=ix))
  # c("sev10.01.1", "sev02.02.2", "sev02.03.1") # seem best as per WAIC
  # "sev02.01.1" | "sev02.02.1" | "sev02.03.1" | "sev10.01.1" WAIC seem best
# "horseshoe(1)" coz interesting extremes in the (multimodal 0+2/3?) ordinal response?
# but maybe relevant for frequency, which when aggregated, would lose timing information?
#   one_dist_param_prior <- prior_string("horseshoe(1)", class = "b")
prior_summary(fits[[3]])
  # class=b prior= hu_prophylaxis10Top50x101_Ami25x001 hu_prophylaxis8Top50x101_sosNaxRiz
  # source: default vectorized
#set_prior("horseshoe(1)", class="b")
  # [to set up regularized horseshoe priors and related hierarchical shrinkage priors ...
  # on all population-level effects at once (excluding the intercept)]
c_sample_prior <- c("no", "yes", "only")[2]
c_prior_Psev0_mean <- paste0("normal(", min(1/50, 1/20), ", 2)") # eg =normal(0.02, 2)
needyCoefs <- paste0("b_", c("prophylaxis10Top50x101_Ami25x001", "prophylaxis8Top50x101_sosNaxRiz"),
  c("", "[1]")[2]) # "[j]" for category-specific coefs eg via cs() with family=sratio() or acat()
set_prior(c_prior_Psev0_mean, dpar=needyCoefs) # alt: class="b", coef=needyCoefs)
  # min(1/50==0.02, 1/20==0.05) "skeptical" "bearish" "exuberant" "bullish" 0.15 0.35 for P(severityMax_asis==0)
  #   (maybe as hurdle parameter) esp for prophylaxis 8* & 10* coz rare sev0s
  #   considering 8* is expected to be the sans-prophylaxis natural headache-generative-process behaviour and
  #   considering a reality: 1/20 days == 0.05 (sample) frequency of occurrence. That or lesser might not get noticed.
  # similarly, 4/10=0.4 for 45678910 grp.

fits.names <- names(fits)[nchar(names(fits)) > 0]; fits.names
suffix <- ".2"; id0 <- paste0("sev", c("02.02.1", "11.02.1"))[1]
  id0.x <- sub(pattern="(0[0-9])([.][0-9])$", replacement=paste0("\\1", suffix), x=id0); id0; id0.x
  fits[[id0.x]] <- update(fits[[id0]], formula. = ~ . - prophylaxis + cs(prophylaxis), newdata=dat.mdl,
    backend="cmdstanr", # recompile=c_recompile,
    save_model=file.path(cRoot, paste0(id0.x, ".txt")), file=file.path(cRoot, id0.x)
  )
  # [5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.]
  # WAIC didn't improve, and Rhat>=1.01 maybe coz small sample of 9Top* which correlates with 8Top*.

suffix <- ".advers"; id0 <- paste0("sev", c("02.01.1", "02.02.2"))[2] # added 2023Dec24
  id0.x <- paste0(id0, suffix); id0; id0.x
  fits[[id0.x]] <- update(fits[[id0]], formula. = ~ . + advers, newdata=dat.mdl,
    backend="cmdstanr", # recompile=c_recompile,
    save_model=file.path(cRoot, paste0(id0.x, ".txt")), file=file.path(cRoot, id0.x)
  )

suffix <- ".csAdvers"; id0 <- paste0("sev", c("02.02.2"))[1] # added 2023Dec24
  id0.x <- paste0(id0, suffix); id0; id0.x # beware: this could take an hour!
  fits[[id0.x]] <- update(fits[[id0]], formula. = ~ . + cs(advers), newdata=dat.mdl,
    backend="cmdstanr", # recompile=c_recompile,
    save_model=file.path(cRoot, paste0(id0.x, ".txt")), file=file.path(cRoot, id0.x)
  )

suffix <- ".cas"; id0 <- "sev02.02.1.tf.ca" # added 2024Dec25
  id0.x <- paste0(id0, suffix); id0; id0.x
  fits[[id0.x]] <- update(fits[[id0]], formula. = ~ . - carry + carrySeq, newdata=dat.mdl,
    backend="cmdstanr", # recompile=c_recompile,
    save_model=file.path(cRoot, paste0(id0.x, ".txt")), file=file.path(cRoot, id0.x)
  )

# maybe ".travelCarry1"
suffix <- ".1"; id0 <- "sev10.01.1" # added 2023Dec04
stop("triggerTravel was mistakenly included as a term, instead of its aggregate factor")
  id0.x <- paste0(id0, suffix)
  fits[[id0.x]] <- update(fits[[id0]], formula. = ~ . + triggerTravel + (1 + carry | prophylaxis), newdata=dat.mdl,
    backend="cmdstanr", # recompile=c_recompile,
    save_model=file.path(cRoot, paste0(id0.x, ".txt")), file=file.path(cRoot, id0.x)
  )

# maybe ".travel2"
suffix <- ".2"; id0 <- "sev10.01.1.1" # added 2023Dec04
  id0.x <- sub(pattern="([0-9])([.][0-9])$", replacement=paste0("\\1", suffix), x=id0)
  fits[[id0.x]] <- update(fits[[id0]], formula. = ~ . - triggerTravel + triggerTravel_fac, newdata=dat.mdl,
    # triggerTravel was mistakenly included as a term, instead of its aggregate factor
    backend="cmdstanr", # recompile=c_recompile,
    save_model=file.path(cRoot, paste0(id0.x, ".txt")), file=file.path(cRoot, id0.x)
  )

fits.names <- names(fits); fits.names # [nchar(names(fits)) > 0]
# loos <- list()
patn <- c("^sev02[.]02.*", "^sev09[.]02.*")[2]; patn.which <- grep(pattern=patn, fits.names); patn.which
for(fi in fits.names[patn.which]){
  print(fi); loos[[fi]] <- loo(fits[[fi]])
}
loo_compare(loos[fits.names[patn.which]]) # c("sev02.02.1.tf.ca", "sev09.02.1")
  # [(0.7, 1]   (bad)        2    0.6%   109 ... See help('pareto-k-diagnostic') for details.]
  # [Diagnostics for Pareto smoothed importance sampling (PSIS)]
loo::pareto_k_ids(loos[["sev09.02.1"]]) # [[1] 315 331] observations
# loos[[fits.names[patn.which]]] <- lapply(fits.names[patn.which], FUN=function(fi){ print(fi); loo(fits[[fi]]) })
  # loo(fits[[fits.names[patn.which]]])
  # m1id <- "sev02.02.1.tf.ca.cas"; loos[[m1id]] <- loo(fits[[m1id]])
  # m2id <- "sev09.02.pro0ad1_pro"; loos[[m2id]] <- loo(fits[[m2id]])

vcov(fits[["sev09.01.2"]])
  # with c_gsFilename_which == " - 2023Dec19-2135":
  # hu~proph8Top & 10Top cor 7Top & 9Top maybe so Rhat convergence problem; also <7n sample 10Top
  # vcov(): hu_prophylaxis8Top50x101_sosNaxRiz & hu_prophylaxis10Top50x101_Ami25x001 are strongly correlated
  # with Intercepts, prophylaxis categories, and other hu_*.
  # vcov(..., correlation = FALSE) by default for covariance matrix; else, correlation matrix.
vcov(fits[["sev09.01.4"]])
  # hu_prophylaxis3Ami37x001 & hu_prophylaxis4Gab100x100_Ami37x001 also seem cor in effect.

stop() # following attempts sampling posterior distributions to estimate unobserved-data prior distribution:
  fit <- fits[[c("sev02.01.1", "sev10.01.1")[2]]]
  # ref https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html#extracting-distributional-regression-parameters
  library(tidybayes) # [masked from ‘package:brms’: dstudent_t, pstudent_t, qstudent_t, rstudent_t]
  library(magrittr)
  library(dplyr)

  # [posterior_epred, posterior_predict and log_lik computing predicted mean values, predicted response values,
  # and log-likelihood values, respectively.]
  # # extract posterior samples of population-level effects 
  # samples1 <- as_draws(fit, variable="^b_", regex=TRUE) # deprecated: posterior_samples(fit, "^b")
  # head(samples1)
  # (draws_fit <- as_draws_array(fit))
  fit.draws.summ <- posterior::summarize_draws(as_draws_array(fit))
  fit.draws.summ.coefs <- fit.draws.summ$mean # alt robust: $median
  fit.draws.summ.coefs_yPprob <- coef2prob(coefs=fit.draws.summ.coefs, coefsNames=fit.draws.summ$variable,
    represents=c("cumulative_logit")[1]) # fit.draws.summ.coefs.yPprob fails ... maybe coz too many dots!!

  epred.median <- tibble(prophylaxis = c_posts.prescription[1]) %>%
    add_epred_draws(fit, dpars=TRUE) %>%
    median_qi(.epred)
  epred.median.asis <- c(epred.median$.epred) # was till 2023Dec03: c(..., 0) to include (missing) "9X"
  epred.median.asis; summary(dat.mdl$severityMax) / nrow(dat.mdl)
    # add_epred_draws(m_esoph_brm, dpar = TRUE, category = "tobgp") %>%
    # add_predicted_draws(m_cyl, ndraws=100, seed = 1234) %>%
  stopifnot(length(epred.median.asis) <= 4)
  cat.mid <- c(0, 2, 6, 9) # "9X" coz severity scale is 0--10. Should thres(2+1) through bayeSpec when modeling?
  (cat.mid * epred.median.asis) / (1 - c_betterBy)
  epred.median.try <- epred.median.asis
  epred.median.try <- c(
    epred.median.try[1] - epred.median.try[1],
    epred.median.try[2], # some of this could be diverted to "0" and "45678", if "9X" with some severity=10 doesn't suffice
    epred.median.try[3],
    epred.median.try[4] + epred.median.try[1]
  )
  cat.mid * epred.median.try
  # str(epred.median$.category)

stop() ### following models after updating formulae with more covariates; it might be obsolete:
  fName.fits <- file.path(cRoot, paste0("fits", c_sevDura_Filename_suffix, ".rds"))
  if(file.exists(fName.fits)){
    print(paste0(fName.fits, " already exists; stop or rename it, unless you wish to overwrite it."))
    # fits <- readRDS(file=fName.fits)
  } # else continue
  fmlas <- list(); fits <- list()
  fmlas.i <- 3; fmlas[[fmlas.i]] <- brmsformula(severityMax | thres(10) ~ 1)
    # severity 0--10 scale though present data might show max 8. cat= deprecated.
    # alt brmsformula() short form: bf()
  # fi <- fmlas.i * 10
  fi.seq <- (fmlas.i * 10 + c(1:7))[] # 26|23 best
  for(fi in fi.seq){
    print(paste0("Starting ", fi)) # fi <- fi + 1
    fits[[fi]] <- switch(fi - (fmlas.i * 10),
      brm(formula=fmlas[[fmlas.i]], data=dat.mdl, family=cumulative("probit")), # default: "logit"
      brm(formula=fmlas[[fmlas.i]], data=dat.mdl, family=sratio()), # threshold="equidistant"?
      update(fits[[fi - 2]], formula. = ~ . + prophylaxis, newdata=dat.mdl),
      update(fits[[fi - 2]], formula. = ~ . + cs(prophylaxis), newdata=dat.mdl), # wide posteriors
      update(fits[[fi - 4]], family=cumulative("logit")),
        # cumulative(link = "logit", link_disc = "log", threshold = "flexible")
      update(fits[[fi - 3]], family=cumulative("logit")),
      # update(fits[[fi - 1]], thres(10),
      stop("Unsupported")
    )
    saveRDS(fits, file=fName.fits)
  }
  view_brms_fits(fits, ix=fi.seq) # alt: ix=length(fits) ... ix=fi
  gen_ach22() # was: (fits[[26]])
  return()
}
loo_compare_ySame <- function(loos, fits.names.sel,
  patn="^(sev[0-9][0-9][.]).*$", side=4, cex=c(0.5, 1.0)[1]){
  finsel <- fits.names.sel
  finsel.yLab <- unlist(lapply(finsel, FUN=function(finsel.i){
    sub(pattern=patn, replacement="\\1", x=finsel.i) # alt: strsplit()
  }
  ))
  finsel.yLab.unq <- unique(finsel.yLab) # alt: distinct()
  duh <- lapply(finsel.yLab.unq, FUN=function(finsel.yLab.unq.j){
    print(paste0("loo_compare() for y as per: ", finsel.yLab.unq.j))
    finsel.ySame.j <- finsel[finsel.yLab == finsel.yLab.unq.j]
    print(loo_compare(loos[finsel.ySame.j]))
    
    # [(0.7, 1]   (bad)        2    0.6%   109 ... See help('pareto-k-diagnostic') for details.]
    # [Diagnostics for Pareto smoothed importance sampling (PSIS)]
    # loo::pareto_k_ids(loos[["sev09.02.1"]]) # [[1] 315 331] observations
    
    # https://cran.r-project.org/web/packages/loo/vignettes/loo2-example.html#computing-psis-loo-and-checking-diagnostics:
    # [loo gives us warnings about the Pareto diagnostics, which indicate that
    # for some observations the leave-one-out posteriors are different enough
    # from the full posterior that importance-sampling is not able to correct
    # the difference.]
    lapply(finsel.ySame.j, FUN=function(finsel.ySame.j.i){ # added 2024Jan27
      loos.j.i <- loos[[finsel.ySame.j.i]]
      pkids.j.i <- loo::pareto_k_ids(loos.j.i)
      
      # this code block is adapted from plot.psis_loo() at https://github.com/stan-dev/loo/blob/master/R/diagnostics.R:
      k <- loo::pareto_k_values(x=loos.j.i)
      k[is.na(k)] <- 0  # FIXME when reloo is changed to make NA k values -Inf
      k_inf <- !is.finite(k)
      k_inf_text <- if (any(k_inf)) {
        paste(signif(100 * mean(k_inf), 2),
                "% of Pareto k estimates are Inf/NA/NaN and not plotted.")
      } else {
        ""
      }
      
      if((! is.null(pkids.j.i)) && (0 < length(pkids.j.i))){
        plot(loos.j.i, label_points=TRUE)
        mtext(paste0(finsel.ySame.j.i, "; ", k_inf_text), side=side, cex=cex)
        # print() # plot(..., diagnostic="k")
      } # else continue
    })
  })
  return()
}
genSaveFits <- function(dat.mdl, bayeSpec, c_fName_fits) {
  rethinkPlots(dat.mdl)
  fits <- runSpec(dat.mdl); fits.names <- names(fits); fits.names
  # fits.bak <- fits
  
  suffix <- ".1pro"; patn.which <- grep(pattern="02.02$", x=fits.names)
  fits.new <- update.1pro(bayeSpec[patn.which,], suffix, fits, dat.mdl)
  fits <- merge.list.my(fits, fits.new); fits.names <- names(fits); fits.names
  
  suffix <- ".tf.ca"; id0.n <- fits.names[grep(pattern="[.]1$", x=fits.names)]
  # was till 2024Jan22: "sev02.02.1" # added 2024Dec25
  fits.new <- updateFits.tf.ca(id0.n, suffix, fits, dat.mdl)
  fits <- merge.list.my(fits, fits.new); fits.names <- names(fits); fits.names
  
  # loos <- list()
  saveRDS(fits, file=c_fName_fits)
  return(fits)
}
genSaveLoos <- function(patn, fits, c_fName_loos) {
  fits.names <- names(fits)
  patn.which <- grep(pattern=patn, fits.names)
  fits.names.sel <- fits.names[patn.which]; fits.names.sel
  loos <- gen.loos(fits.names.sel, fits)
  # loo_compare.brmsfit(add_criterion(mod, criterion = "loo"), ...)
  # [1: Found 71 observations with a pareto_k > 0.7 in model 'fits[[fi]]'. It is recommended to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
  # 2: Found 1 observations with a pareto_k > 0.7 in model 'fits[[fi]]'. It is recommended to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
  # 3: Found 1 observations with a pareto_k > 0.7 in model 'fits[[fi]]'. It is recommended to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
  # ]
  saveRDS(loos, file=c_fName_loos)
}


    # [Our cumulative model assumes that the observed ordinal variable Y ... originates from the categorization
    # of a latent (not observable) continuous variable Y~. ... If we assume Y~ to have a certain distribution (e.g.,
    # a normal distribution) with cumulative distribution function F ... Pr(Y=k) = F(Tk) - F(Tk-1).]
    # [Recall that when the cumulative distribution function F is phi (standard normal distribution), Y~ is a
    # standard normal variable. Consequently, the thresholds indicate where the continuous latent variable Y~ is
    # partitioned to produce the observed responses Y, in standard-deviation units. Therefore, applying phi to each
    # threshold leads to the cumulative probability of responses below that threshold if all predictor variables
    # were zero.]
    # [Sequential models model the decision between Y=k and Y>k, whereas adjacent-category models model the decision
    # between Y=k and Y=k=1.]
    # [Unequal variances can be modeled with all three classes of ordinal models ...: brm(bf(Y~X, disc~X), ...)]
    #   [brm(bf(...) + lf(disc~0+X, cmc=FALSE), ...)] to [ensure that disc is estimated only for] X "levels" by
    #   turning off cell-mean coding (`cmc`).
    # [Family 'cumulative' requires either positive integers or ordered factors as responses.]
    # In brm(), [sample_prior Indicate if draws from priors should be drawn additionally to the posterior draws.
    # Options are "no" (the default), "yes", and "only". ... See brmsformula how to obtain prior draws for the
    # intercept. If sample_prior is set to "only", draws are drawn solely from the priors ignoring the likelihood,
    # which allows among others to generate draws from the prior predictive distribution. In this case, all
    # parameters must have proper priors.]
    # [file_refit Modifies when the fit stored via the file argument is re-used. Can be set globally for the
    # current R session via the "brms.file_refit" option (see options).
    # For "never" (default) the fit is always loaded if it exists and fitting is skipped.
    # For "always" the model is always refitted. If set to "on_change", brms will
    # refit the model if model, data or algorithm as passed to Stan differ from what is
    # stored in the file.]
    #
    # ref https://github.com/paul-buerkner/brms/issues/148 on Bayesian structural time series models:
    # [brms now supports splines and Gaussian processes, which do a fairly good job in replacing (basic) BSTS terms
    # but with much less convergence issues and the immediate ability to do forecasting if desired.
    # The prophet package, which is also based on Stan, seems to be an excellent option for use cases where one
    # would consider applying BSTS structures.]
    # Univariate Bayesian time series models: https://atsa-es.github.io/atsa/Lectures/Week%206/lec_11_bayes.html#1
    #   2023May
    #
    # ref https://mc-stan.org/users/documentation/case-studies/identifying_mixture_models.html:
    # [Mixture modeling is a powerful technique for integrating multiple data generating processes into a single model.
    # Unfortunately when those data data generating processes are degenerate the resulting mixture model suffers
    # from inherent combinatorial non-identifiabilities that frustrate accurate computation. Consequently, in order
    # to utilize mixture models reliably in practice we need strong and principled prior information to ameliorate
    # these frustrations. ...
    # Unfortunately, there is one more pathology hidden in mixture models that can seriously compromise the
    # accuracy of a fit. If two of the mixture components overlap then the resulting posterior manifests a
    # particularly nasty geometry that is difficult to explore. This overlap is especially common when there
    # are more components in the mixture than needed to capture the structure in the data, and excess components
    # are forced to collapse into the necessary components.
    # ]
    #   mix <- mixture(gaussian, gaussian); fit1 <- brm(bf(y ~ x + z), dat, family = mix, prior = prior ...)
    #
    # help("brmsformula") and help("addition-terms"):
    # [For multivariate models, subset may be used in the aterms part, to use different subsets of the data in
    # different univariate models. For instance, if sub is a logical variable and y is the response of one of the
    # univariate models, we may write y | subset(sub) ~ predictors so that y is predicted only for those observations
    # for which sub evaluates to TRUE. ...
    # For all ordinal families, aterms may contain a term thres(number) to specify the number thresholds
    # (e.g, thres(6)), which should be equal to the total number of response categories - 1. If not given, the
    # number of thresholds is calculated from the data. If different threshold vectors should be used for
    # different subsets of the data, the gr argument can be used to provide the grouping variable
    # (e.g, thres(6, gr = item), if item is the grouping variable). In this case, the number of thresholds can
    # also be a variable in the data with different values per group.
    # ]
    #
    # For hurdle_cumulative and (custom) hurdle_sratio family:
    # https://github.com/paul-buerkner/brms/issues/1429.
    # https://gist.github.com/sjwild/d2f4897c98c3ad2588fe7836fa5e294e
    # ref https://search.r-project.org/CRAN/refmans/brms/html/brmsfamily.html:
    #   hurdle_cumulative(link = "logit", link_hu = "logit", link_disc = "log", threshold = "flexible")
    # https://discourse.mc-stan.org/t/how-can-i-write-a-hurdle-ordinal-model-custom-family-in-brms/29681 refers to
    # https://htmlpreview.github.io/?raw.githubusercontent.com/sjwild/hurdle_ordered/main/test_supported_hurdle_cumulative_family.html
    # https://discourse.mc-stan.org/t/deal-with-correlated-predictors-in-brms/23435/4:
    # [If you know a priori that the predictors are not strongly correlated in the population, then collecting
    # a larger sample (if feasible) should enable you to disentangle their effects. Your model seems to be telling
    # you that at the moment you don’t have enough data to separate out the effects of A and B. If you can
    # incorporate strong prior information about either A or B, you might still be able to recover good inference
    # on the other.]
    # https://paul-buerkner.github.io/brms/articles/brms_distreg.html:
    # [Please note that the probability of catching no fish is actually higher than 41%, but parts of this
    # probability are already modeled by the Poisson distribution itself (hence the name zero-inflation).
    # If you want to treat all zeros as originating from a separate process, you can use hurdle models instead ...]
    # https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/
    #   plogis(hurdle_intercept + hurdle_lifeexp) - plogis(hurdle_intercept)
    #   [brms’s conditonal_effects() will plot predicted values of specific coefficients while holding all other
    #   variables constant, and it converts the results to their original scales. ... brms::conditional_effects()
    #   with hurdle models: Since we’re working with a mixture model, the syntax for dealing with conditional/marginal
    #   effects is a little different, as the lifeExp variable exists in both the non-zero and the zero processes of the
    #   model. We can specify which version of lifeExp we want to work with using the dpar argument (distributional
    #   parameter) in conditional_effects(). By default, conditional_effects() will return the marginal/conditional
    #   means of the combined process using both the non-zero part mu and the zero part hu. If we want to see the
    #   0/not 0 hurdle part, we can specify dpar = "hu“. The conditional_effects() function helpfully converts the
    #   predicted values into their original scales: when showing predicted life expectancy, it unlogs the values;
    #   when showing predicted proportion of zeros, it unlogits the values.]
    #
    # Autocorrelation structures:
    #   bf(y ~ x, autocor = ~ arma(p=1, q=1) + car(M))
    #   eg https://mediatum.ub.tum.de/doc/1079269/829224.pdf auto-correlates with a patient's headache diary;
    #   severity 0 highest and 5 lowest nil ie [5,0] was recorded 4 times each day for about 9 months.
    #   Headaches were on around 14% of the time.
    #   [This includes information on humidity, windchill, temperature and pressure changes, wind direction,
    #   and length of sunshine on the previous day. ...
    #   covariate ’sunshine on previous day’ ... seems to be important for the analysis ...
    #   For this patient we were able to demonstrate considerable impact of weather related variables such as
    #   the present of windchill and sunshine length. ... Even though an individual analysis offers the opportunity
    #   to develop more precise migraine control mechanisms, it is of interest to identify common risk factors in
    #   groups of patients. This problem is subject of current research. A possibility is to generalize the AOP
    #   model to a multivariate setting] eg including `durationH` in multivar response.
    #
    # https://arxiv.org/pdf/1809.09445.pdf:
    # [Let y1, . . . , yn be the maxima of blocks of observations from an unknown probability distribution.
    # Extreme value theory (Fisher and Tippett, 1928; de Haan and Ferreira, 2006)
    # implies that as the block size increases and under mild conditions, each of the yi follows a
    # GEV(µi, σi, ξi) distribution ...
    # This encompasses the three classical models for maxima (Jenkinson, 1955): if ξi > 0, the distribution
    # is Fr´echet; if ξi < 0, it is reverse Weibull; and if ξi → 0, it is
    # Gumbel. The shape parameter is particularly important since it controls the tail properties
    # of the distributions. The expectation of Yi is ... (21) ... Non-stationarity of (21) could stem from
    # changes in any of the parameters, and as intepretability is priority in risk assessment, a multiple GAM
    # model for the GEV distribution is well justified. Most data analyses involving non-stationary extremes use
    # a parametric or semi-parametric form in the location and/or scale parameters while keeping the shape a
    # fixed scalar (ChavezDemoulin and Davison, 2012, §4), even though it may be plausible that it varies—seasonal
    # effects, for example, may stem from different physical processes with different extremal behaviors. ...
    # Overall, the model does not seem unrealistic, although it may underestimate slightly the uncertainty,
    # as it assumes independence of maxima in successive months.
    # ]
    #
    # More readings:
    # https://solomonkurz.netlify.app/blog/2021-12-29-notes-on-the-bayesian-cumulative-probit/
    # https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/
    # https://www.andrewheiss.com/blog/2021/12/01/multilevel-models-panel-data-guide/
    # https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/
    # https://evalf21.classes.andrewheiss.com/resource/bayes/#super-short-example
    # https://www.andreashandel.com/posts/2022-02-24-longitudinal-multilevel-bayes-3/index.html
    #
    # More foundationally:
    # https://bookdown.org/content/4857/monsters-and-mixtures.html#zero-inflated-outcomes
    # https://bookdown.org/content/3686/inferring-a-binomial-probability-via-exact-mathematical-analysis.html
    # https://betanalpha.github.io/assets/case_studies/qr_regression.html.
    #   brm(..., decomp="QR") [helps in fitting models with highly correlated predictors.]

### Following are extensions since 2023Nov07:
# ON: Use `brms` framework for a Bayesian rethinking and to also include MCP-Mod hybrid methodology.
#   Multiple Comparison Procedure (MCP) deals with dose-response trials with qualitative dose variable;
#   alternative is MCP-Mod hybrid methodology ---i.e., with quantitative modeling techniques--- ref:
#   https://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=82B9B40E48BEBAAAFEF53C9DCE7AB615?doi=10.1.1.367.3108&rep=rep1&type=pdf.
# DONE: (Safety) Adversity during prescription regimes: mild during Ami25*, moderate during Ami37*, nil otherwise.
# DONE: Encode start24H for being treated as continuous.
    ### --- LEARNING NOTES:
    # [Ordering intercepts in mixtures of ordinal families is not possible as each family has itself
    # a set of vector of intercepts (i.e. ordinal thresholds).]
    # ref https://cran.r-project.org/web/packages/brms/vignettes/brms_nonlinear.html:
    # [In contrast to generalized linear models, priors on population-level parameters (i.e., ‘fixed effects’)
    # are often mandatory to identify a non-linear model. ... Quite often, you may be forced to change your priors
    # after fitting a non-linear model for the first time, when you observe different MCMC chains converging to
    # different posterior regions. This is a clear sign of an identification problem and one solution is to set
    # stronger (i.e., more narrow) priors.]
    # ref https://journals.sagepub.com/doi/epub/10.1177/2515245918823199:
    # [Classes of Ordinal Models ... cumulative models, sequential models, and adjacent-category models. ...
    # Fitting category-specific effects in cumulative models is problematic because of the possibility of
    # negative probabilities ... and consequently is not allowed in brms. Therefore, we use an
    # adjacent-category model instead. ... In brms, the parameter related to latent variances is called
    # disc (short for “discrimination”), following conventions in item response theory. ... That is, s = 1/disc. ...
    # From a theoretical perspective, if the response under study can be understood as the categorization of a
    # latent continuous construct, we recommend using a cumulative model. The categorization interpretation is
    # natural for many Likert-item data sets, in which ordered verbal (or numerical) labels are used to obtain
    # discrete responses about a continuous psychological variable. Cumulative models are also computationally
    # less intensive than the other types of models, and therefore faster to estimate. ...
    # If the response under study can be understood as the result of a sequential process, such that a
    # higher response category is possible only after all lower categories are achieved, we recommend using a
    # sequential model. Sequential models are therefore especially useful, for example, for discrete time-to-event data.]
    # ref https://watermark.silverchair.com/igad070.pdf:
    # [we used Bayesian estimation rather than traditional frequentist methods because Bayesian statistics are
    # better suited for modeling longitudinal data with relatively small sample sizes ... Bayesian estimation
    # uses analogs to frequentist confidence intervals and p-values: highest posterior density
    # intervals (HPDs) and posterior probabilities of direction ...  computed using the bayestestR software package]
