#' ---
#' title: "Fulton County Schools CCRPI 2017 Replicated using 2018 Scoring Model: Closgin Gaps Algorithms"
#' author: "Riley Smith-Hunter"
#' date: "Last Updated: `r format(Sys.Date(), '%d %b %Y')`"
#' ---
#'
#' -----
#'
#+ setup, echo=FALSE, results='hide', fig.keep='none', fig.show='none', message=FALSE, warning=FALSE

require(magrittr); library(data.table); library(Riley)
# **DEMO** - GENERAL SETUP -----------------------------------------------------------
# source("zmeta.R")
# knitr::opts_chunk$set(echo = TRUE, tidy = TRUE, tidy.opts = list(width.cutoff = 60))
#'
#' -----
#'
#' # `Rcgpts()` Universal Setup
#'
# UNIVERSAL SETUP ------------------------------------------------
## [SUBGROUP LABELS & VARIABLE NAMES] ================================
lgrps <- list(all = "all", ai = "I", as = "S", bl = "B", hp = "H", mr = "M", wh = "W", ed = "Y", el = "Y", swd = "Y")
lgrpvars <- list(all = "all", ai = "race.code", as = "race.code", bl = "race.code", hp = "race.code", mr = "race.code", wh = "race.code", ed = "ed", el = "el", swd = "swd")
lbsg <- list(ball = "all", bai = "ai", bas = "as", bbl = "bl", bhp = "hp", bmr = "mr", bwh = "wh", bed = "ed", bel = "el", bswd = "swd") ## FOR GETTING SUBGROUP BASELINES IN 'CMBL.*SW' ##
#'
#' ## `Rrecwgt()` & `Rnflags()`
#'
### Rrecwgt() DEF ####
## (RENAMED (FROM 'f' TO 'Rrecwgt') ON 2018-05-23 ##
rec.wgt <- c("'L1' = 0; 'L2' = 0.5; 'L3' = 1; 'L4' = 1.5; else = NA")

Rrecwgt <- function(x) {
    car::recode(x, rec.wgt)
}

### Rnflags() DEF
## (RENAMED (FROM 'fcnts' TO 'Rnflags') ON 2018-05-23) ##
Rnflags <- function(x) {
    ifelse(x == "L0", 0, 1)
}
#'
#' ------
#'
#' # '`cmbl`': Subgroups' Baseline Data
#'
## NEW CM BASELINES BY SUBGROUP DATA ====================================

## LOAD BASELINE DATA ##
# cmbl <- Rrdcsv("data/CM-bySubgroup-2016-new.csv", asDT = FALSE) ## [CREATED ON 20180608] ##

## SEPARATE BASELINE DATA BY GRADEBAND, AND RESHAPE EACH GRADEBAND'S BASELINE DATAFRAME SO THAT IT CAN BE USED IN 'Rcgpts_sgt()' TO COMPUTE TARGETS ##
# vars.cmbl <- c("school.id", "reporting.category", "assessment.subject", "baseline")
# cmbl.es <- cmbl[cmbl$grade.cluster == "ES", vars.cmbl, drop = FALSE] %>% droplevels()
# cmbl.esw <- reshape(cmbl.es, v.names = c("baseline"), idvar = c("school.id", "reporting.category"), timevar = "assessment.subject", direction = "wide")
# cmbl.ms <- cmbl[cmbl$grade.cluster == "MS", vars.cmbl, drop = FALSE] %>% droplevels()
# cmbl.msw <- reshape(cmbl.ms, v.names = c("baseline"), idvar = c("school.id", "reporting.category"), timevar = "assessment.subject", direction = "wide")
# cmbl.hs <- cmbl[cmbl$grade.cluster == "HS", vars.cmbl, drop = FALSE] %>% droplevels()
# cmbl.hsw <- reshape(cmbl.hs, v.names = c("baseline"), idvar = c("school.id", "reporting.category"), timevar = "assessment.subject", direction = "wide")
#'
#' -----
#'
#' # `Rcgpts_sgt()`: Pre-Process Input Data for `Rcgpts()` (defined later)
#'
## [SUBGROUP TARGETS] =========================================

Rcgpts_sgt <- function(xw) {
    #, keep_all = getOption("Riley.qc.keep_all")
    xw.sg0 <- lapply(lbsg, function(x) xw[xw$reporting.category == x, ])
    x.sgt0 <- lapply(xw.sg0, function(x) x[, -1:-2])

    ## COMPUTE 3% & 6% TARGETS FOR ALL SUBJECT AREAS BY SUBGROUP ##
    x.sgt <- lapply(names(x.sgt0), function(x)
        data.frame(school.id = xw.sg0[[x]]$school.id, cat = xw.sg0[[x]]$reporting.category,
                   target = apply(x.sgt0[[x]], 2, function(z) {
                       t1 <- ifelse(z < 90.00, ((100 - z)*0.03) + z, 90.00)
                       t2 <- ifelse(z < 95.00, ((100 - z)*0.06) + z, 95.00)
                       return(c(t1 = t1, t2 = t2))
                   }
                   ))
    )

    ## CREATE DATAFRAME CONTAINING FOR ALL SUBJECT AREAS BY SUBGROUP ##
    x.sgt.bl <- lapply(names(x.sgt0), function(x) {
        data.frame(school.id = as.character(xw.sg0[[x]]$school.id), cat = xw.sg0[[x]]$reporting.category, x.sgt0[[x]])
    })

    ## LABEL TARGET LEVELS ("T1" = 3%; "T2" = 6%) ##
    x.sgt.tg0 <- lapply(x.sgt, function(x) {
        y <- x
        y$target.level <- gsub("(t\\d)\\..*?$", "\\1", rownames(x))
        return(y)
    })

    ## ONLY ASSIGN LEVEL 2 TARGET FOR ED, EL, & SWD SUBGROUPS ##
    x.sgt.tgw <- lapply(x.sgt.tg0, function(x) {
        w <- reshape(x[!duplicated(x), ], idvar = c("school.id", "cat"), timevar = "target.level", direction = "wide")
        return(w)
    })

    x.sgt.tg01 <- lapply(x.sgt.tgw, function(x) {
        t1names <- grep("\\.t1", names(x), value = TRUE)
        t2names <- grep("\\.t2", names(x), value = TRUE)
        y <- as.data.table(x, key = c("school.id", "cat"))
        y[, c(t2names) := lapply(.SD, function(x) {
            ifelse(cat %in% c("ed", "el", "swd"), x, NA)
        }), .SDcols = t2names]
        return(as.data.frame(y))
    })

    x.sgt.tg <- lapply(x.sgt.tg01, function(x) {
        l <- reshape(x[!duplicated(x), ], direction = "long")
        names(l) <- gsub("\\.t\\d", "", names(l), perl = TRUE)
        return(l)
    })

    names(x.sgt.bl) <- names(lgrps)
    names(x.sgt.tg) <- names(lgrps)

    # res <- ifelse(keep_all == TRUE, list(xw.sg0 = xw.sg0, x.sgt0 = x.sgt0, x.sgt = x.sgt, x.sgt.bl,
    #                                      x.sgt.tg0 = x.sgt.tg0, x.sgt.tgw = x.sgt.tgw, x.sgt.tg01 = x.sgt.tg01,
    #                                      x.sgt.tg = x.sgt.tg),
    #               list(x.sgt.bl = x.sgt.bl, x.sgt.tg = x.sgt.tg))
    return(list(x.sgt.bl = x.sgt.bl, x.sgt.tg = x.sgt.tg))
}

pander::pander(Rcgpts_sgt)

## RUN EACH GRADEBAND'S SUBGROUPS' BASELINE DATA THROUGH 'Rcgpts_sgt()' ##
# es.sgt <- Rcgpts_sgt(xw = cmbl.esw)
# ms.sgt <- Rcgpts_sgt(xw = cmbl.msw)
# hs.sgt <- Rcgpts_sgt(xw = cmbl.hsw)

#+ results='asis', message=FALSE, warning=FALSE
### (QC - CG BASELINES & TARGETS) ####

## ES BASELINES ##
# lapply(es.sgt$x.sgt.bl, function(x) knitr::kable(Rmsmm(x)))

## ES TARGETES (SHOULD NOT EXCEED 95) ##
# lapply(es.sgt$x.sgt.tg, function(x) knitr::kable(Rmsmm(x)))

## MS BASELINES ##
# lapply(ms.sgt$x.sgt.bl, function(x) knitr::kable(Rmsmm(x)))

## MS TARGETES (SHOULD NOT EXCEED 95) ##
# lapply(ms.sgt$x.sgt.tg, function(x) knitr::kable(Rmsmm(x)))

## HS BASELINES ##
# lapply(hs.sgt$x.sgt.bl, function(x) knitr::kable(Rmsmm(x)))

## HS TARGETES (SHOULD NOT EXCEED 95) ##
# lapply(hs.sgt$x.sgt.tg, function(x) knitr::kable(Rmsmm(x)))
#'
#' -----
#'
#' # `Rcgpts()`: Algorithm for Computing Closing Gaps Points Earned by Subgroup
#'
# 'Rcgpts()' DEFINITION -----------------------------------------------------

Rcgpts <- function(x, df.baseline, df.target, subject,
                   in_vivo_qc = getOption("in_vivo_qc")) {
                   #keep_all = getOption("Riley.qc.keep_all")) {
    ## Rcgpts(): A FUNCTION TO COMPUTE A GIVEN SUBJGROUP'S CLOSING GAPS POINTS FOR A GIVEN GMA ASSESSMENT.SUBJECT & GRADEBAND USING THE ABOVE-DETERMINED BASELINE & TARGETS DATA FOR THAT SUBGROUP ON THAT ASSESSMENT IN THAT GRADEBAND (I.E., MUST BE IMPLEMENTED ON EACH SUBGROUP'S DATA FOR EACH ASSESSMENT.SUBJECT WITHIN EACH GRADEBAND) ##

    ## PREEMPTIVELY ENSURE THAT INPUT DATA ARE ONLY 'DATAFRAME' CLASS (NOT 'DATA.TABLE') ##
    x <- as.data.frame(x)
    df.baseline <- as.data.frame(df.baseline)
    df.target <- as.data.frame(df.target)

    ## SUBJECT-SPECIFIC LABELS FOR BASELINE & TARGETS COLUMNS ##
    labs <- c(paste0("baseline.", subject), paste0("target.baseline.", subject),
              paste0("target.baseline.", subject, "2"))
    if (in_vivo_qc == TRUE) { cat("\n----------\n'labs':\n----------\n"); print(labs, digits = 4) } ## IN VIVO QC ##

    ## COMBINE CURRENT ('x'), BASELINE ('df.baseline'), & TARGETS ('df.target') INPUT DFs ##
    x1 <- merge(x, df.baseline[, c("school.id", "cat", labs[1])], all = TRUE) ## [CURRENT]+[BASELINE] DATA ##
    x2 <- merge(x1, df.target[df.target$target.level == "t1", c("school.id", "cat", labs[2])], all = TRUE) ## [CURRENT+BASELINE]+[3% TARGET ('t1')] ##

    if (in_vivo_qc == TRUE) { cat("\n-------------------------------\n'x1'(`[CURRENT] + [BASELINE]`):\n-------------------------------\n") }; print(x1, digits = 4) ## IN VIVO QC ##
    if (in_vivo_qc == TRUE) { cat("\n------------------------------------------------------\n'x2'(`{[CURRENT] + [BASELINE]} + [3% TARGET ('t1')]`):\n------------------------------------------------------\n") }; print(x2, digits = 4) ## IN VIVO QC ##

    ## RENAME INPUT TARGET DATA'S  [TARGET VALUES] COLUMN TO REPRESENT 't2' TARGET (FOR MERGE IN THE SUBSEQUENT LINES RESULTING IN 'xx') ##
    df.target2 <- df.target
    names(df.target2)[which(names(df.target2) == labs[2])] <- labs[3]

    ## !! NOTE: ISSUE WHEREIN SUBGROUPS WERE ASSIGNED TO 'L1' IF THEY DID NOT EXCEED THEIR BASELINE, EVEN IF THE BASELINE WAS ABOVE 90 AND THE CURRENT YEAR WAS AT OR ABOVE 90 SHOULD BE RESOLVED BELOW (PREVIOUSLY, ALL OF X2 COLUMNS WERE MERGED WITH DF.TARGET, WHICH ULTIMATELY LED TO THE WRONG COLUMNS BEING RENAMED, WHICH LED TO THE WRONG COLUMNS BEING USED IN THE SUBSEQUENT [IF-THEN-ELSE-NO] LOGIC) ##

    ## [[CURRENT+BASELINE]+[3% TARGET ('t1')]]+[6%TARGET ('t2')] DATA ##
    xx <- merge(x2[, c("school.id", "cat",
                       paste0(c("N_Students.",
                                "AchPts_Cpd.",
                                "baseline.",
                                "target.baseline."), subject))],
                df.target2[df.target2$target.level == "t2",
                           c("school.id", "cat", labs[3])], all = TRUE)
    if (in_vivo_qc == TRUE) { cat("\n----------------------------------------------------------------------------\n'xx' (`{[CURRENT] + [BASELINE] + [3% TARGET ('t1')]} + [6% TARGET ('t2')]`):\n----------------------------------------------------------------------------\n"); print(xx, digits = 4) }## IN VIVO QC ##

    ## PROGRAMMATICALLLY, VIA REGEX, RENAME XX'S COLUMNS TO AVOID POSSIBLE MIS-NAMING DUE TO MISMATCHED COLUMN ORDERING ##
    ### NOTE: THE BELOW LINES COULD ACTUALLY BE TAKEN CARE IN ONLY TWO LINES, AND PROBABLY MORE ELEGANTLY, ...
    ### ... BUT I'M TAKING THE LONG ROUTE HERE FOR CLARITY/TRANSPARENCY, GIVEN THE (OBNOXIOUS) COMPLEXITY OF THE CLOSING GAPS COMPONENT ###
    # colnmaes(xx) <- gsub("\\.[A-Z]+$", "")
    colnames(xx) <- gsub("^N_Students\\.[A-Z]+$", "N", colnames(xx))
    colnames(xx) <- gsub("^baseline\\.[A-Z]+$", "baseline", colnames(xx))
    colnames(xx) <- gsub("^target\\.baseline\\.[A-Z]+$", "target.baseline", colnames(xx))
    colnames(xx) <- gsub("^target\\.baseline\\.[A-Z]+2$", "target.baseline2", colnames(xx))
    colnames(xx) <- gsub("^AchPts_Cpd\\.[A-Z]+$", "ach.cm", colnames(xx))
    ## NOTE: REPLACED ALL INSTANCES OF 'ach.pr' WITH 'ach.cm' ON 2018-06-12 ##
    ### ... 'ach.pr' WAS USED IN THE ORIGINAL VERSION OF Rcgpts() (WRITTEN & IMPLEMENTED IN 2018-02) ###
    ### ... TO DENOTE ACHIEVEMENT POINTS WEIGHTED BASED ON PARTICIPATION RATE ...
    ### ... HOWEVER, AS OF 2018-05-??, THE PARTICIPATION RATE BUSINESS RULE IS NOT CURRENTLY IMPLEMENTED ###

    if (in_vivo_qc == TRUE) { cat("\n------------------------------\n'xx' (after renaming columns):\n------------------------------\n"); print(xx, digits = 4) }## IN VIVO QC ##

    ## CONDITIONALLY ASSIGN POINT VALUES BASED ON USER-SPECIFIED OPTION FOR 'both_targets' ARG ...
    ### ... SEE x.sgt.tg DEF IN Rcgpts_sgt() ABOVE, WHICH AUTO-FILTERS WHETHER ONE OR BOTH TARGETS SHOULD BE USED ###
    x.pts <- within(xx, {
        baseline <- ifelse(baseline == -1, NA, baseline)
        target.baseline <- ifelse(is.na(baseline), NA, target.baseline)
        target.baseline2 <- ifelse(is.na(baseline), NA, target.baseline2)

        ## DON'T APPLY POINTS TO SUBGROUPS WITH LESS THAN 15 RESULTS ##
        ach.cm <- ifelse(N < 15, NA, ach.cm)

        # *** YOU ARE HERE [20180612, @16:57]*** ------------------------------------------
        ## MODIFIED AND TESTED 'PTS' ASSIGNMENT LOGIC & FLOW ON 2018-06-12 ##
        pts.l1 <- ifelse((ach.cm <= baseline), 1, 0)
        pts.l2 <- ifelse((ach.cm > baseline) & (ach.cm < target.baseline), 2, 0)
        pts.l3 <- ifelse((ach.cm >= target.baseline), 3, 0)
        pts.l4 <- ifelse((ach.cm >= target.baseline2), 4, 0)
    })
    if (in_vivo_qc == TRUE) { cat("\n------------------\n'x.pts' (initial):\n------------------\n"); print(x.pts, digits = 4) } ## IN VIVO QC ##

    ## ADDED (AS PART OF MODIFIED 'PTS' ASSIGNMENT FLOW) ON 2018-06-02 ##
    x.pts$pts.max <- apply(x.pts[, c("pts.l1", "pts.l2", "pts.l3", "pts.l4")], 1, max, na.rm = TRUE)
    x.pts$pts.max <- ifelse(is.infinite(x.pts$pts.max), NA, x.pts$pts.max) ## IN CASE ANY ROWS CONTAIN ALL 'NAs', THEREFORE CAUSING '-Inf' AS RESULT OF 'max(...)' IMPLEMENTATION ABOVE ##
    # x.pts$pts <- ifelse(x.pts$pts.max == 0, NA, paste0("L", x.pts$pts.max))
    x.pts$pts <- ifelse(is.na(x.pts$pts.max), NA, paste0("L", x.pts$pts.max))
    if (in_vivo_qc == TRUE) { cat("'x.pts' (intermediate):\n"); print(x.pts, digits = 4) } ## IN VIVO QC ##

    x.pts <- x.pts[, c("school.id", "cat", "N", "ach.cm", "baseline", "target.baseline", "target.baseline2", "pts")]

    # if (in_vivo_qc == TRUE) cat("\n----------------\n'x.pts' (final):\n----------------\n"); print(x.pts, digits = 4) ## IN VIVO QC ##

    ## RETURN INPUT SUBGROUP'S COMPUTED CLOSING GAPS POINTS ON THE SPECIFIED 'subject' ##
    # res <- ifelse(keep_all == TRUE, list(labs = labs, x1 = x1, x2 = x2, df.target2 = df.target2,
    #                                      xx = xx, x.pts = x.pts),
    #               list(xx = xx, xpts = x.pts))
    return(list(xx = xx, xpts = x.pts))
}

# pander::pander(Rcgpts)

#'
#' -----
#'
#' # '`Rcgpts_post()`': Post-Processing of '`Rcgpts()`' Output Data
#'
#'
## [Rcgpts() POST-PROCESSING] =========================
Rcgpts_post <- function(x) {
    #, keep_all = getOption("Riley.qc.keep_all")
    cgm.subj0 <- x
    names(cgm.subj0) <- names(lgrps) ## CGM.SUBJ* ELEMENT NAMES CORRESPOND TO SUBGROUP LABELS ##

    ## ASSIGN SUBGROUP LABEL TO THE CLOSING GAPS POINTS COLUMN IN EACH OF CGM.ELA.ES0'S DATAFRAME ##
    cgm.subj01 <- lapply(seq_along(lgrps), function(x) plyr::rename(cgm.subj0[[x]], c("pts" = paste0("pts.", names(lgrps)[[x]]))))

    ## SUBSET EACH SUBGROUP'S DATAFRAME TO INCLUDE ONLY THE SCHOOL ID & THE SUBGROUP'S COMPUTED CG POINTS ##
    cgm.subj02 <- lapply(seq_along(lgrps), function(x) cgm.subj01[[x]][, c("school.id", paste0("pts.", names(lgrps)[[x]]))])
    cgm.subj03 <- lapply(seq_along(lgrps), function(x) {
        y <- cgm.subj02[[x]]
        y[, paste0("pts.", names(lgrps)[[x]])] <- as.character(y[, paste0("pts.", names(lgrps)[[x]])])
        return(y)
    })
    cgm.subj04 <- lapply(cgm.subj03, function(x) x[!duplicated(x), ])

    ## COMBINED ROWS OF ALL SUBGROUPS' COMPUTED CG POINTS DFs INTO A SINGLE DF ##
    cgm.subj05 <- Reduce(function(x, y) merge(x, y, all = TRUE), cgm.subj04)

    ## IMPLEMENT CLOSING GAPS POINT ASSIGNMENTS ACCORDING TO 'CG LEVEL' DETERMINED VIA THE ABOVE Rcgpts() IMPLEMENTATIONS BY SUBGROUP
    cgm.subj <- data.frame(school.id = cgm.subj05[, 1], apply(cgm.subj05[, -1], 2, Rrecwgt)) ### SEE 'zmeta.R' ###

    ### CNTS.ELA.ES - NUMBER OF CG FLAGS ####
    ## (CORRESPONDS TO EACH SCHOOL'S TOTAL '*POSSIBLE*' CG POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ...
    ## ... THE CURRENT SUBJECT'S FLAG COUNTS WILL BE COMBINED WITH THE REMAINING SUBJECTS' FLAG COUNTS FOR THE CURRENT GRADEBAND, RESULTING IN THE DENOMINATOR IN THE CURRENT GRADEBAND'S FINAL CLOSING GAPS COMPONENT SCORE CALCULATION) ##
    cnts.subj0 <- data.frame(school.id = cgm.subj05[, "school.id"], apply(cgm.subj05[, -1], 2, Rnflags)) ## SEE 'zmeta.R' ##
    cnts.subj <- data.frame(school.id = cnts.subj0[, 1], denom.subj = apply(cnts.subj0[, -1], 1, sum, na.rm = TRUE))

    ## RETURN CGM.SUBJ & CNTS.SUBJ ##
    # res <- ifelse(keep_all == TRUE, list(cgm.subj0 = cgm.subj0, cgm.subj01 = cgm.subj01,
    #                                      cgm.subj02 = cgm.subj02, cgm.subj03 = cgm.subj03,
    #                                      cgm.subj04 = cgm.subj04, cgm.subj05 = cgm.subj05,
    #                                      cgm.subj = cgm.subj[!duplicated(cgm.subj), ],
    #                                      cnts.subj0 = cnts.subj0,
    #                                      cnts.subj = cnts.subj[!duplicated(cnts.subj), ]),
    #               list(cgm.subj = cgm.subj[!duplicated(cgm.subj), ],
    #                    cnts.subj = cnts.subj[!duplicated(cnts.subj), ]))
    return(list(cgm.subj = cgm.subj[!duplicated(cgm.subj), ],
                cnts.subj = cnts.subj[!duplicated(cnts.subj), ]))
}

# pander::pander(Rcgpts_post)
#'


# CODEDUMP (Rcgpts) -------------------------------------------------------

## WROTE, TESTED, AND DEPRECATED THE BELOW [IF-ELSE-NO] SEQUENCE ON 2018-06-12 ##

#     pts <- if (is.na(baseline) | is.na(target.baseline) | is.na(ach.cm)) {
#         NA
#     } else if (ach.cm <= baseline) {
#         "L1"
#     } else if (ach.cm > baseline & ach.cm < target.baseline) {
#         "L2"
#     } else if (ach.cm >= target.baseline & ach.cm & is.na(target.baseline2)) {
#         "L3"
#     } else if (ach.cm >= target.baseline & ach.cm < target.baseline2) {
#         "L3"
#     } else if (ach.cm >= target.baseline2) {
#         "L4"
#     }
#
### FOR TESTING 'PTS' [IF-THEN-ELSE] SEQUENCE ABOVE ###
# ach.cm <- 82
# baseline <- 81
# target.baseline <- 83
# target.baseline2 <- NA
#
## DEPRECATED BELOW LINE IN Rcgpts() DEF ON 2018-06-12 - REPLACED WITH LINES TO PROGRAMMATICALLY, VIA REGEX, RENAME COLUMNS (TO AVOID POSSIBLE MIS-NAMING DUE TO INCORRECT COLUMN ORDERING) ##
# colnames(xx) <- c("school.id", "cat", "N", "ach.cm", "baseline", "target.baseline", "target.baseline2")



## DEPRECATED ON 2018-06-12 ##
# x.sgt1 <- lapply(names(x.sgt0), function(x) {
#     data.frame(school.id = as.character(xw.sg0[[x]]$school.id), cat = xw.sg0[[x]]$reporting.category, x.sgt0[[x]])
# })
# x.sgt2 <- lapply(x.sgt, function(x) {
#     y <- x
#     y$target.level <- gsub("(t\\d)\\..*?$", "\\1", rownames(x))
#     return(y)
# })
#
# x.sgt.bl <- lapply(x.sgt1, function(x) {
#     y <- x
#     # names(y) <- c("school.id", "cat", "baseline.ELA", "baseline.SS", "baseline.MATH", "baseline.SCI")
#     return(y)
# })
#
# x.sgt.tg0 <- lapply(x.sgt2, function(x) {
#     y <- x
#     # names(y) <- c("school.id", "cat", "target.baseline.ELA", "target.baseline.SS", "target.baseline.MATH", "target.baseline.SCI", "target.level")
#     return(y)
# })


### FOR TESTING Rcgpts() ####
# x <- xmath.hs.cm[[8]] ## USING 'ED' SUBGROUP AS TEST CASE ##
# df.baseline <- hs.sgt$x.sgt.bl[[8]]
# df.target <- hs.sgt$x.sgt.tg[[8]]
# subject <- "MATH"
