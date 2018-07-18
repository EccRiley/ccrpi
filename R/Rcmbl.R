#' ---
#' title: "Fulton County Schools CCRPI 2017 Replicated using 2018 Scoring Model: Closing Gaps Baseline Data"
#' author: "Riley Smith-Hunter"
#' date: "Last Updated: `r format(Sys.Date(), '%d %b %Y')`"
#' ---
#'
#' -----
#'
#'
#' `r tufte::newthought("Note:")` This file only needs to be run once to generate the subgroups' baseline scores datafile ('_**`data/CM-bySubgroup-2016-new.csv`**_'). _This script was originally written and implemented on `2018-06-08`_;
#'
#' ------

#'
#+ setup, echo=FALSE, results='hide', fig.keep='none', fig.show='none', message=FALSE, warning=FALSE
# source("C:/Users/hunterrm1/Dropbox/rstudio/SETUP.R")
#'
#' # General Setup
#'
#'
# GET Rach() & Rach_validate() -----------------------------------------------
# source("Rach.R")
# pander(Rach)
# pander(Rach_validate)

# SUBGROUP LABELS & VARIABLE NAMES ------------------------------------------
lgrps <- list(all = "all", ai = "I", as = "S", bl = "B", hp = "H", mr = "M", wh = "W", ed = "Y", el = "Y", swd = "Y")
lgrpvars <- list(all = "all", ai = "race.code", as = "race.code", bl = "race.code", hp = "race.code", mr = "race.code", wh = "race.code", ed = "ed", el = "el", swd = "swd")
lbsg <- list(ball = "all", bai = "ai", bas = "as", bbl = "bl", bhp = "hp", bmr = "mr", bwh = "wh", bed = "ed", bel = "el", bswd = "swd")
#'
#' -----
#'
#' # '`Rcmbl()`': Algorithm for Generating Subgroups' Content Mastery Baseline Data
#'
Rcmbl <- function(bl.subj, gradecluster, subject, ...) { ## '...' NOT CURRENTLY IMPLEMENTED ##
    ### (ADD COLUMN TO INPUT DATA FOR 'ALL' CATEGORY) ###
    bl.subj$all <- "all"

    ### [XSUBJ - GMA ACHIEVEMENT SCORES BY SUBGROUP] ###
    xsubj <- lapply(names(lgrps), function(x)
        Rach(bl.subj,
             groupvar = bl.subj[, lgrpvars[[x]]],
             group = lgrps[[x]],
             gradeband = gradecluster,
             subject = subject))

    ### (XSUBJ.NEWX & XSUBJ.CM* - SEPARATE Rach() OUTPUTS) ###
    xsubj.newxbl <- lapply(xsubj, function(x) x[["new_x"]])
    xsubj.cmbl0 <- lapply(xsubj, function(x) x[["cm.subj"]])

    ### (XSUBJ.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
    names(xsubj.newxbl) <- names(lgrpvars)
    names(xsubj.cmbl0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

    # ### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
    # xsubj.cmbl <- lapply(xsubj.cmbl0, function(x) {
    #     y <- x
    #     vars.achpts <- grep("AchPts", names(y), value = TRUE)
    #     y[, c(vars.achpts)] <- apply(y[, c(vars.achpts)], 2, function(x) x*100)
    #     return(y)
    # })

    ## MERGE SUBGROUPS' ELA BASELINE SCORES ##
    lsubj.cmbl <- lapply(1:10, function(x) {
        vars.subj.cmbl <- c("school.id",
                            grep("N_Students", names(xsubj.cmbl0[[x]]), value = TRUE),
                            grep("AchPts_Cpd", names(xsubj.cmbl0[[x]]), value = TRUE))
        y <- as.data.frame(xsubj.cmbl0[[x]])
        y <- y[, c(vars.subj.cmbl)]
        names(y) <- c("school.id", "n.students", "baseline")
        y$grade.cluster <- gradecluster
        y$assessment.subject <- subject
        y$reporting.category <- lbsg[[x]]
        # y$n.students <- ifelse(y$n.students < 15, NA, y$n.students)
        y$baseline <- ifelse(y$n.students < 15 , NA, y$baseline*100)
        res <- y[, c("school.id", "n.students", "grade.cluster", "reporting.category", "assessment.subject", "baseline")]
        return(res)
    })

    subj.cmbl <- rbindlist(lsubj.cmbl)
    return(subj.cmbl)
}

# pander(Rcmbl)
#'
#' -----
#'
#' # Implement '`Rcmbl()`' Using Prior Year's Student-Level _GMAS_ Data
#'
# ES SUBGROUPS BASELINES ------------------------------------------------

## LOAD ES BASELINE STUDENT-LEVEL GMAS DATA ##
# bl.ela.es <- Rrdcsv("data/CM-studentlevel-2016/CM_ES_ELA_2016.csv")
# bl.math.es <- Rrdcsv("data/CM-studentlevel-2016/CM_ES_MATH_2016.csv")
# bl.sci.es <- Rrdcsv("data/CM-studentlevel-2016/CM_ES_SCI_2016.csv")
# bl.ss.es <- Rrdcsv("data/CM-studentlevel-2016/CM_ES_SS_2016.csv")

## RUN ES STUDENT-LEVEL DATA THROUGH RSUBJCMBL() (DEFINED ABOVE) ##
# cmbl.ela.es <- Rcmbl(bl.ela.es, gradecluster = "ES", subject = "ELA")
# cmbl.math.es <- Rcmbl(bl.math.es, gradecluster = "ES", subject = "MATH")
# cmbl.sci.es <- Rcmbl(bl.sci.es, gradecluster = "ES", subject = "SCI")
# cmbl.ss.es <- Rcmbl(bl.ss.es, gradecluster = "ES", subject = "SS")

## COMBINE ES CMBL.SUBJ DATA ##
# cmbl.es <- rbind(cmbl.ela.es, cmbl.math.es, cmbl.sci.es, cmbl.ss.es)

#+ echo=FALSE, results='asis'
# kable(Rmsmm(cmbl.es), caption = "Summary Statistics for Elementary Schools' Baseline Data")
#'
#'
# MS SUBGROUPS BASELINES ------------------------------------------------

## LOAD MS BASELINE STUDENT-LEVEL GMAS DATA ##
# bl.ela.ms <- Rrdcsv("data/CM-studentlevel-2016/CM_MS_ELA_2016.csv")
# bl.math.ms <- Rrdcsv("data/CM-studentlevel-2016/CM_MS_MATH_2016.csv")
# bl.sci.ms <- Rrdcsv("data/CM-studentlevel-2016/CM_MS_SCI_2016.csv")
# bl.ss.ms <- Rrdcsv("data/CM-studentlevel-2016/CM_MS_SS_2016.csv")

## RUN MS STUDENT-LEVEL DATA THROUGH RSUBJCMBL() (DEFINED ABOVE) ##
# cmbl.ela.ms <- Rcmbl(bl.ela.ms, gradecluster = "MS", subject = "ELA")
# cmbl.math.ms <- Rcmbl(bl.math.ms, gradecluster = "MS", subject = "MATH")
# cmbl.sci.ms <- Rcmbl(bl.sci.ms, gradecluster = "MS", subject = "SCI")
# cmbl.ss.ms <- Rcmbl(bl.ss.ms, gradecluster = "MS", subject = "SS")

## COMBINE MS CMBL.SUBJ DATA ##
# cmbl.ms <- rbind(cmbl.ela.ms, cmbl.math.ms, cmbl.sci.ms, cmbl.ss.ms)

#+ echo=FALSE, results='asis'
# kable(Rmsmm(cmbl.ms), caption = "Summary Statistics for Middle Schools' Baseline Data")
#'
#'
# HS SUBGROUPS BASELINES ------------------------------------------------

## LOAD HS BASELINE STUDENT-LEVEL GMAS DATA ##
# bl.ela.hs <- rbind(Rrdcsv("data/CM-studentlevel-2016/CM_HIGH_ELA1_2016.csv"),
#                    Rrdcsv("data/CM-studentlevel-2016/CM_HIGH_ELA2_2016.csv"))
# bl.math.hs <- rbind(Rrdcsv("data/CM-studentlevel-2016/CM_HIGH_MATH1_2016.csv"),
#                     Rrdcsv("data/CM-studentlevel-2016/CM_HIGH_MATH2_2016.csv"),
#                     Rrdcsv("data/CM-studentlevel-2016/CM_HIGH_MATH3_2016.csv"),
#                     Rrdcsv("data/CM-studentlevel-2016/CM_HIGH_MATH4_2016.csv"))
# bl.sci.hs <- rbind(Rrdcsv("data/CM-studentlevel-2016/CM_HIGH_SCI1_2016.csv"),
#                    Rrdcsv("data/CM-studentlevel-2016/CM_HIGH_SCI2_2016.csv"))
# bl.ss.hs <- rbind(Rrdcsv("data/CM-studentlevel-2016/CM_HIGH_SS1_2016.csv"),
#                   Rrdcsv("data/CM-studentlevel-2016/CM_HIGH_SS2_2016.csv"))

## RUN HS STUDENT-LEVEL DATA THROUGH RSUBJCMBL() (DEFINED ABOVE) ##
# cmbl.ela.hs <- Rcmbl(bl.ela.hs, gradecluster = "HS", subject = "ELA")
# cmbl.math.hs <- Rcmbl(bl.math.hs, gradecluster = "HS", subject = "MATH")
# cmbl.sci.hs <- Rcmbl(bl.sci.hs, gradecluster = "HS", subject = "SCI")
# cmbl.ss.hs <- Rcmbl(bl.ss.hs, gradecluster = "HS", subject = "SS")

## COMBINE HS CMBL.SUBJ DATA ##
# cmbl.hs <- rbind(cmbl.ela.hs, cmbl.math.hs, cmbl.sci.hs, cmbl.ss.hs)

#+ echo=FALSE, results='asis'
# kable(Rmsmm(cmbl.hs), caption = "Summary Statistics for High Schools' Baseline Data")
#"
#'
#' # Subgroups' Baseline CM Data Combined Across All Gradebands
#'
#+ cmbl, results='asis'
# CMBL - ALL GRADEBAND'S CM BY SUBGROUP BASELINE DATA ------------------------------
# cmbl <- rbind(cmbl.es, cmbl.ms, cmbl.hs)

## WRITE CMBL DATAFILE ##
# write.csv(cmbl, "data/CM-bySubgroup-2016-new.csv", row.names = FALSE)
#'
#+ echo=FALSE, results='asis'
# kable(Rmsmm(cmbl), caption = "Summary Statistics for Combined Baseline Data")
#'
