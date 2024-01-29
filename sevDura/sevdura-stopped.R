stop(); fits.bak <- fits; fits <- list() # ; fi <- 0 # init

writeLines(readLines(file.path(Sys.getenv("HOME"), ".R/Makevars")))
sessionInfo()
  ## 1. Simulate `ach22` Distributional Prior:
  ach22.df <- TBD gen_ach22() acheDpriors(dat1)
  dat1.ach22 <- rbind(ach22.df, dat1)

  fmlas <- list()
  # ref brms_48ex()
  fmlas[[1]] <- brmsformula(severityMaxGrp ~ 1) # alt short form: bf()
fi.seq <- list(c(1), c(2,3), c(2,4), c(2,4,5), c(2,6), c(2,7), c(2,8), c(1:7)
)[[c(3, 7, 8)[3]]]
  fName.fits <- file.path(cRoot, paste0("fits", c_sevDura_Filename_suffix, ".rds"))
  if(file.exists(fName.fits)){
    print(paste0(fName.fits, " already exists; stop or rename it, unless you wish to overwrite it."))
    # fits <- readRDS(file=fName.fits)
  } # else continue
stop()

for(fi in fi.seq){
  print(paste0("Starting ", fi)) # fi <- fi + 1
fits[[fi]] <- switch(fi,
  brm(formula = fmlas[[1]], data=dat1, family=cumulative()),
    # =cumulative("logit") # with "identity" link.  "probit" Est SD more (response) than ref base
  brm(formula = fmlas[[1]], data=dat1, family=sratio()), # threshold="equidistant"?
    # was: update(fits[[1]], formula. = ~ ., family=sratio()), # threshold="equidistant"?
  update(fits[[2]], formula. = ~ . + prophylaxis, newdata=dat1),
    # proph Est: 7Top 0.5 <~ 6Rib 0.6.  pairs() looks ok.
    # WAIC 584
    # was:
    #   proph Est: 7Top* 0.08 < 6Rib* 0.17.
    #   +ve correl across pairs() plots reveals there's an underlying structure worth identifying.
  update(fits[[2]], formula. = ~ . + cs(prophylaxis), newdata=dat1),
    # was: update(fits[[3]], formula. = ~ . - prophylaxis + cs(prophylaxis), newdata=dat1),
    # insightful! bcs__prophylaxis8Top*[1] has inadequate sample (presently) ie needs more low-severity data.
    # ~.+ c1 N(-1.76,err0.60), c2 N(1.22,err0.53)
    #> exp(c(c(-.96,.32), c(-.15,.33))) # prophylaxis3Ami37x001[1]&[2], prophylaxis7Top25x001_stopRib_SPMF[1]&[2] Est
    #[1] 0.3828929 1.3771278 0.8607080 1.3909681
  update(fits[[4]], formula. = ~ . + (1 | prophylaxis)),
    # Group-Level Effects: sd(Intercept) Est 1.22 Err .96
    # c(c(-.93,.42), c(-.2,.37))
  update(fits[[2]], formula. = ~ . + (1 + cs(prophylaxis) | prophylaxis), newdata=dat1),
    # except sd_proph3Ami1 (highest Est 1.90 vs prophNot3 ~1.5) vs sd*2 (lowest Est 1.34 vs prophNot3),
    #   sd_prophK1 Est similar to sd_prophK2 and with Err [1.52, 1.77].  And cor() 0.
    # greater variation in modeling response's lower category level (vs. upper) ... coz sd(Intercept[1]) 0.65, [2] 0.48
    # Population: ~.+ c1 N(-1.80,0.65|proph) + c2 N(0.92,0.48|proph).
    # WAIC 575.7 vs earlier ~580
  update(fits[[4]], formula. = ~ . + cs(carry) + cs(medSOS), newdata=dat1),
    # == update(fits[[7]], formula. = ~ . - cs(prophylaxis*advers) +  cs(prophylaxis), newdata=dat1),
    # Warning: [Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable. ...]
    # proph8[1] misses sev0 data & medSOS happened during proph8 too.
    #   This might explain low ESS & high Err Est with medSOS[1,2].
    # vs other proph, proph1Ami sev45678 seems lower & sev0 no worse with high sev123.
    # WAIC 481
    # 
    # was on 2023Nov22: update(fits[[2]], formula. = ~ . + cs(prophylaxis*advers) + cs(carry) + cs(medSOS), newdata=dat1),
    #   Parts of the model have not converged (some Rhats are > 1.05). Be careful ...]
    # was till 2023Nov21: update(fits[[4]], formula. = ~ . + cs(advers) + cs(carry) + cs(medSOS), newdata=dat1),
    # Warning: [The largest R-hat is NA, indicating chains have not mixed. ...
    #   Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable. ...
    #   Parts of the model have not converged (some Rhats are > 1.05). Be careful ...]
    #   resolve: maybe via cs(prophylaxis*advers) coz proph1Ami & 5Gab corr with advers; proph8Top[1] corr with medSOS
    #   b_advers* c1 > c2.  b_advers1mild sample size short or coz corr with prophy??
    #   b_medSOS1 < b_medSOS2 as expected coz medSOS tends to lower/cap/censor higher sev responses.
    #     With a lag, it might affect lower sev too.  
    #   ?? b_carry1 >> b_carry2 (high Err coz sample)

  update(fits[[2]], formula. = ~ . + (cs(prophylaxis + carry + medSOS) | advers), newdata=dat1),
    # Error: [No valid effects detected] conditional_effects.brmsfit()
    # Grp: sd_c1 1.13 > sd_c2 0.88.  sd_proph8Top[1] 3.9, sd_proph4Gab[1] 2.0 > others.
    #   sd_carry[1] 2.3 > sd_carry[2] 0.8.  sd_medSOS[1] 5.1 < [2] 10.7.
    # Pop: c1 -1.04, c2 1.41
    # WAIC 481!  all posterior distributional plots look healthy!  corrs 0, Rhat 1.0!

  ,,, ,,, ,,,
  ,, brm(formula = fmlas[[1]], data=dat1, family=multinomial()),
  stop("Unsupported")
)

  saveRDS(fits, file=fName.fits)
}

  view_brms_fits(fits, ix=fi.seq) # alt: ix=length(fits) ... ix=fi
  # next:
  #   sev0 didn't occur during carry==1 in 2023!  Exclude carry==1 for stabler Est.
  #   compare classical linear model with (proph3? base &) more proph8 low-sev sample:
  #   LOO instead of WAIC
  # + (medSOS | cens) + arma()
    # - redo fit4 on fit2 ~.+ c1 N(-1.76,err0.60) + c2 N(1.22,err0.53)
    # - redo fit6 ~.+ c1 N(-1.80,0.65|proph) + c2 N(0.92,0.48|proph).
###
  fitC10 <- brm(formula = severityMaxGrp ~ (1 | prophylaxis),
    data=dat1, family=cumulative())
# sd_prophylaxis__Intercept mean=0.48
# as expected, b_Intercept[1] mean=-1.57 and b_Intercept[2] mean=1.08 don't overlap. They are +vely correlated.
# WAIC 575.9
  fitD10 <- brm(formula = severityMaxGrp ~ cs(prophylaxis_int) + (1 | prophylaxis),
    data=dat1, family=sratio(threshold="equidistant"))
# TBD
# WAIC 577.5

  fitC100 <- update(fitC10, formula. = ~ . + 1 -(1 | prophylaxis)
    # ,data=dat1, family=cumulative()
  )
# TBD
# as per ESS, bigger sample size used for Intercept[2].
#Population-Level Effects: 
#             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept[1]    -1.36      0.14    -1.64    -1.09 1.00     1678     2247
#Intercept[2]     1.21      0.14     0.95     1.49 1.00     4232     2948
# WAIC 580.6

  fitE10 <- brm(formula = severityMaxGrp ~ (1 + cs(prophylaxis_int) | prophylaxis),
    data=dat1, family=sratio(threshold="equidistant"))
      # was: family=sratio, threshold="equidistant",
      # ref https://cran.rstudio.com/web/packages/brms/news/news.html:
      #   [Deprecate argument threshold in brm and instead recommend passing threshold directly to the
      #   ordinal family functions.]
# coefs1 varies~2x coefs2; so worth including sd_coefs1 as well as sd_coefs2
#   ... coz 2x sd_coefs1 Est >2xEst sd_coefs2.
# even if you include sd{1,2}, what's required for sd_coefs2: (a) (flexible?) finer Est or (b) other factors
#   ... coz sd_coefs1 Est > Err; sd_coefs2 Est == Err.
# as expected, b_Intercept[1] Est -1.7 and b_Intercept[2] Est 1 have high |Est/Err|.
#   ??? They don't overlap. They are +vely correlated.
# TBD ... coz cor(*) seem negligible.
# WAIC 574.1
#Group-Level Effects: 
#~prophylaxis (Number of levels: 7)
#                                           Estimate Est.Error l-95% CI u-95% CI
#sd(Intercept[1])                               0.79      0.58     0.04     2.27
#sd(Intercept[2])                               0.37      0.36     0.01     1.36
#sd(prophylaxis_int[1])                         0.20      0.16     0.01     0.62
#sd(prophylaxis_int[2])                         0.09      0.08     0.00     0.29



  fits <- list(fitC10, fitD10, fitE10, fitC100)
  saveRDS(fits, file=file.path(cRoot, "fits.rds"))
  ix <- length(fits); view_brms_fits(fits, ix=ix)


stop() ### --- earlier:
  fitC1 <- brm(formula = severityMaxGrp ~ prophylaxis + (1 | prophylaxis),
    data=dat, family=cumulative)
    # Group level effects: sd(Intercept | prophylaxis) seems significant ~1.5.
    # relative to 1Ami*, coef vector shows ascending mean 3* 4*  (7*) 6* 5*  8*
    # pairs() reveals:
    #   b_Intercept[1] and b_Intercept[2] are extremely correlated. Ordered response yi=severityMaxGrp has 3 levels.
    #   They are similarly correlated with b_prophylaxis* (relative to b_proph1).
    #   b_Intercept* correls vs. b_proph3,5,6 seem tighter. b_proph3 correls with b_proph5,6 also seem tighter.

# update(formula. = ~ . + cs(prophylaxis) + (1 | prophylaxis) ... # after factor ordering
  fits <- list(fitC0, fitC1) # was: list(fitA, fitA1, fitA2, fitA3, fitB4, fitC0)
  saveRDS(fits, file=file.path(cRoot, "fits.rds"))
  view_brms_fits(fits)

  fitC0 <- brm(formula = severityMax ~ prophylaxis + (1 | prophylaxis),
    data=dat, family=cumulative)
    # Group level effects: sd(Intercept | prophylaxis) seems significant ~1.9.
    # Population level effects: only Intercept[6,7,8] seem larger apart & significant; so, factorize accordingly.
    # relative to 1Ami*, coef vector is V shaped: 4Gab* mean is nearest least 0.17 and 8Top* highest 0.77.
    # posterior distributions of parameters seem Gaussian vs. the initial "improper flat" started with.

  fitA <- brm(formula = severityMax ~ prophylaxis + eDays + start24HMfrac
    + triggerTravel_fac + siteTemp + medSOS
    + (1 | prophylaxis),
    data = dat, family = cumulative
  )
  summary(fitA, waic=TRUE)
    # Group-Level Effects:
    # ~prophylaxis (Number of levels: 7)
    #                                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # sd(Intercept)                          2.72      2.41     0.10     8.80 1.07       93       45
    # Population-Level Effects:
    # eDays                                  0.01      0.01    -0.02     0.03 1.07       71      108
    # start24HMfrac                         -0.23      0.06    -0.36    -0.11 1.09       31      114
    # triggerTravel_facyes                  -0.50      0.74    -1.93     1.08 1.03      106      250
    # siteTempFrontalsnoTemporal             0.23      1.68    -2.91     3.80 1.08       49       92
    # siteTempTemporalplus                   2.04      1.79    -1.40     5.74 1.10       30      102
    # medSOSNaxdom                           3.20      0.84     1.54     4.78 1.03       63      130

  fitA1 <- update(fitA, formula. = ~ . -prophylaxis +advers # +sd_prophylaxis_intercept
    # +(-1 | prophylaxis)
    , newdata=dat
  )
  summary(fitA1, waic=TRUE)

  fitA2 <- update(fitA1, formula. = ~ . -advers -eDays)
  fitA3 <- update(fitA2, formula. = ~ . -(1|prophylaxis) +(1 + medSOS | prophylaxis)) # [require recompiling]

  fitB4 <- brm(formula = severityMax ~ cs(prophylaxis_int) + siteTemp + medSOS,
    data=dat, family=sratio(threshold="equidistant"),
    prior=set_prior("normal(3,1)", coef="prophylaxis_int"))
      # was: family=sratio, threshold="equidistant",
      # ref https://cran.rstudio.com/web/packages/brms/news/news.html:
      #   [Deprecate argument threshold in brm and instead recommend passing threshold directly to the
      #   ordinal family functions.]


stop()
## Bayesian Analysis
# `p_severityMax_exceedsThresh = p_severityMax_exceedsThresh_givenSOS * p_SOS /
#   p_SOS_given_severityMax_exceedsThresh`
# for Latex formulae, ref:
# https://colab.research.google.com/github/bebi103a/bebi103a.github.io/blob/master/lessons/00/intro_to_latex.ipynb#scrollTo=XoU5XDjs6lSO
thresh <- (1:7)[6]
n_severityMax_exceedsThresh_givenSOS <- nrow(dat[(! is.na(dat$severityMax)) &
  (dat$severityMax >= thresh) & (dat$medication_fac != "0nil"), ])
n_severityMax_exceedsThresh <- nrow(dat[(! is.na(dat$severityMax)) &
  (dat$severityMax >= thresh), ])
n_SOS <- nrow(dat[(dat$medication_fac != "0nil"), ])
n_dat <- nrow(dat)
n_myBayes <- c(n_severityMax_exceedsThresh_givenSOS, n_SOS, n_dat,
  n_severityMax_exceedsThresh)
names(n_myBayes) <- c("n_severityMax_exceedsThresh_givenSOS", "n_SOS", "n_dat",
  "n_severityMax_exceedsThresh"); n_myBayes

p_severityMax_exceedsThresh_givenSOS <-
  n_severityMax_exceedsThresh_givenSOS / n_SOS
p_SOS <- n_SOS / n_dat
p_SOS_given_severityMax_exceedsThresh <-
  n_severityMax_exceedsThresh_givenSOS / n_severityMax_exceedsThresh
p_severityMax_exceedsThresh <- p_severityMax_exceedsThresh_givenSOS * p_SOS /
  p_SOS_given_severityMax_exceedsThresh
p_myBayes <- c(p_severityMax_exceedsThresh_givenSOS, p_SOS,
  p_SOS_given_severityMax_exceedsThresh, p_severityMax_exceedsThresh)
names(p_myBayes) <- c("p_severityMax_exceedsThresh_givenSOS", "p_SOS",
  "p_SOS_given_severityMax_exceedsThresh", "p_severityMax_exceedsThresh")

#"""Here's a Bayesian Analysis. As per Bayes Theorem:
#> $P(severityMax >= thresh) = \frac{P(severityMax >= thresh \mid SOS) \, P(SOS)}{P(SOS \mid severityMax >= thresh)}$

#Considering sample probabilities involving SOS `medication` which interacts with `severityMax`, this yields the following unconditional estimate for $P(severityMax >= thresh)$ named as `p_severityMax_exceedsThresh`:
#"""

cat(paste0("Where thresh=", thresh, ":"))
round(p_myBayes, 3)
