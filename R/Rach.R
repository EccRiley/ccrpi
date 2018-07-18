#' ---
#' title: "Fulton County Schools CCRPI 2017 Replicated using 2018 Scoring Model: Content Mastery Algorithms"
#' author: "Riley Smith-Hunter"
#' date: "Last Updated: `r format(Sys.Date(), '%d %b %Y')`"
#' ---
#'
#' -----
#'
#' # `Rach()`
#'
Rach <- function(x, gradeband, subject, groupvar = NULL, group = NULL, return_new_x = TRUE, ...) {
    ## Rach(): FUNCTION FOR DYNAMICALLY COMPUTING CONTENT MASTERY CCRPI COMPONENTS' SUBJECT-LEVEL INDICATOR'S SCORE (SUBJECT TO BE COMPUTED IS SPECIFIED BY USER, AS IS GRADEBAND). ALSO USED IN CLOSING GAPS COMPONENT'S SUBJECT-LEVEL INDICATOR SCORES FOR EACH SUBGROUP (SUBGROUP CAN BE SPECIFIED BY USER FOR THIS PURPOSE) ##


    ## WEIGHTS APPLIED TO EACH SCHOOL'S % OF STUDENTS SCORING AT EACH ACHIEVEMENT LEVEL ON THE INPUT SUBJ. ##
    rec.cmlvls <- c("'BEG' = 'L1'; 'DNM' = 'L1'; 'DEV' = 'L2'; 'PRO' = 'L3'; 'ADV' = 'L3'; 'DIS' = 'L4'") ## EOG/EOC/GAA ACHIEVEMENT LEVELS (ADDED ON 2018-05-09) ##
    rec.cmpnts <- c("'BEG' = 0; 'DNM' = 0; 'DEV' = 0.5; 'PRO' = 1; 'ADV' = 1; 'DIS' = 1.5") ## ACHIEVEMENT LEVEL POINT-VALUES (ADDED ON 2018-05-09) ##
    # rec.wgt <- c("'L1' = 0; 'L2' = 0.5; 'L3' = 1; 'L4' = 1.5; else = 0") ## 'rec.wgt' NOT CURRENTLY IMPLEMENTED (BC THIS IS FUNCTIONALLY THE SAME AS 'rec.cmpnts' ABOVE, BUT KEEPING HERE FOR REFERENCE) ##

    ## 'newxvars': LIST OF ALL ESSENTIAL VARS (INCL. SUBGROUPS' VARS) FOR CONTENT MASTERY OUTPUT DF ##
    newxvars <- c("school.id", "school.name", "school.year", "N_Students", "gtid",
                  "fay.participant", "student.grade.level", "gender.code", "race.code", "el", "ed", "swd", "all",
                  "assessment.type.code", "osa.performance.code", "osa.performance.lvl",
                  "SumPts", "AchPts", "AchPts_Cpd", "AchPts_Wgtd", "ccrpi.points")
    ## NOTE: ['SumPts', 'AchPts', 'AchPts_Cpd', 'AchPts_Wgtd', & 'CCRPI_POINTS'] ARE CREATED BELOW ##

    ### RESTRICT X TO SPECIFIED GROUP (IF APPLICABLE) ####
    if (!is.null(groupvar)) {
        x <- x[groupvar == group, ] %>% droplevels()
    }
    ### FILTER ON [FAY.PARTICIPANT == 'Y'] ####
    ## (... & CREATE 'new_x' AS A DATA.TABLE-CLASSED COPY OF 'x', RETAINING ONLY VARIABLES CM-RELEVANT) ##
    new_x <- x[x$fay.participant == "Y", names(x) %in% newxvars] %>% droplevels()
    library(data.table)
    setDT(new_x, key = c("school.id"))

    ### RESTRICT TO VALID ASSESSMENT TYPES ('assessment.type.code'): ####
    ## (MODFIFIED TO INCLUDE 'GAA' ASSESSMENT.TYPE.CODE ON 2018-05-09) ##
    new_x <- new_x[assessment.type.code %in% c("EOG", "EOC", "GAA")]

    ### RESTRICT TO USER-SPECIFIED GRADEBAND ####
    if (gradeband == "MS") {
        ## HIGH SCHOOLS ##
        new_x <- new_x[student.grade.level %in% c(6, 7, 8)]
    } else  if (gradeband == "HS") {
        ## MIDDLE SCHOOLS ##
        new_x <- new_x[student.grade.level >= 9]
    } else if (gradeband == "ES") {
        ## ELEMENTARY SCHOOLS ##
        new_x <- new_x[student.grade.level <= 5]
    } else stop("Unusable Grade Band")

    ### RESTRICT TO VALID ACHIEVEMENT LEVEL CODES ####
    ## (MODIFIED TO INCLUDE 'GAA' ACH.LVL. CODES ON 2018-05-09) ##
    new_x <- new_x[osa.performance.code %in% c("BEG", "DNM", "DEV", "PRO", "ADV", "DIS")] ## MODIFIED ON 2018-05-09 ##

    ### RECODE OSA.PERFORMANCE.CODE ####
    ## (... TO ENSURE THAT THE ACH-PT LEVELS ARE ORDERED CORRECTLY IN TABULATED OUTPUTS LATER) ##
    new_x[, osa.performance.lvl := car::recode(osa.performance.code, rec.cmlvls)] ## SEE 'rec.cmlvls' DEF ABOVE (MODIFIED ON 2018-05-09) ##

    ### CREATE NEW "ccrpi.points" COLUMN (PER D.JAFFE) <==> EACH STUDENT'S ACH. PTS. EARNED ON THE INPUT SUBJ. ##
    ## (... IMPLEMENTED ON 2018-05-09) ##
    new_x[, ccrpi.points := car::recode(osa.performance.code, rec.cmpnts)] ## SEE 'rec.cmpnts' DEF ABOVE ##

    cm.subj <- new_x[, .(N_Students = .N, ## COMPUTE NUMBER OF VALID TEST RESULTS FOR THE FOCAL SUBJ. ##
                         SumPts = sum(ccrpi.points), ## COMPUTE TOTAL ACH.PTS. EARNED ON THE FOCAL SUBJ. ##
                         AchPts = sum(ccrpi.points)/.N), ## COMPUTE SUBJ. INDICATOR'S CCRPI PTS [DEPRECATED ROUNDING OF 'AchPts' ON 20180608 (FOR QC PURPOSES)] ##
                     by = key(new_x)] ## DO ALL OF THE ABOVE BY SCHOOL ##

    ### ... (NOTE: SUBJ. INDICATOR CCRPI PTS ARE COMPUTED AS A FUNCTION OF [TOTAL ACH.PTS. EARNED] DIVIDED BY  [NUMBER OF VALID TEST RESULTS] FOR THE FOCAL SUBJ. - THE RESULTING SCHOOL-LEVEL INDICATOR SCORE IS ROUNDED TO 2 DECIMAL PLACES, PER 2018 CALCULATION GUIDE) [IMPLEMENTED ROUNDING ON 2018-06-02] ###

    ## CAP SUBJ. INDICATOR ACHIEVEMENT POINTS AT 100% (i.e., 1.0pts) ##
    cm.subj[, AchPts_Cpd := ifelse(AchPts >= 1.0000, 1, AchPts)]

    ## IMPLEMENT GRADEBAND-SPECIFIC RULES FOR CONTENT MASTERY COMPONENT SCORE WEIGHTS: ##
    if (gradeband == 'HS') {
        cmwgt <- 0.25
    } else if (gradeband %in% c("MS", "ES")) {
        if (subject %in% c("ELA", "MATH")) {
            cmwgt <- 0.375
        } else if (subject %in% c("SCI", "SS")) {
            cmwgt <- 0.125
        }
    }

    ### APPLY CONTENT MASTERY WEIGHT (BASED ON GRADEBAND) TO CAPPED SUBJ. INDICATOR SCORES ####
    cm.subj[, AchPts_Wgtd := AchPts_Cpd*cmwgt] ## [DEPRECATED ROUNDING ON 20180612 (FOR QC PURPOSES)]##

    ## APPEND SUBJ. LABEL TO COMPUTED VARIABLES' COLNAMES ##
    names(cm.subj)[-1] <- paste0(names(cm.subj)[-1], ".", subject)
    setnames(new_x, "ccrpi.points", paste0("ccrpi.points.", subject))

    ### RETURN OUTPUT(S) ####
    ## BY DEFAULT, 'new_x' DF IS RETURNED ALONG WITH CM.SUBJ. DF ##
    if (return_new_x == TRUE) {
        return(list(new_x = new_x, cm.subj = cm.subj))
    } else return(cm.subj)
}

pander::pander(Rach)
#'
#' -----
#'
#' # `Rach_validate()`
#'
Rach_validate <- function(new_x, orig_x, label.subject, label.grade) {
    library(dplyr)
    ### INCLUDE ALL ORIGINAL VALUES IN LEVELS OF NEW_X ####
    labs.fay <- unique(na.omit(orig_x$fay.participant))
    labs.osa <- unique(na.omit(orig_x$assessment.type.code))
    new_x$fay.participant <- factor(new_x$fay.participant,
                                    levels = labs.fay)
    new_x$assessment.type.code <- factor(new_x$assessment.type.code,
                                         levels = labs.osa)
    new_x$osa.performance.code <- factor(new_x$osa.performance.code,
                                         levels = c("BEG", "DNM", "DEV", "PRO", "ADV", "DIS"), ordered = TRUE)

    label.grade <- paste0("(", label.grade, ")")
    label.subjgr <- paste0(label.subject, " ", label.grade)

    ### VALIDATION OUTPUT TABLES (N = 7) ####
    table(orig_x$fay.participant) %>%
        pander(caption = paste0(label.subjgr, " 'FAY Participant' (_pre_-filtering)"),
               col.names = paste0("FAY Participant = '", labs.fay, "'"),
               justify = rep("right", 2))

    table(new_x$fay.participant, deparse.level = 2) %>%
        pander(caption = paste0(label.subjgr, " 'FAY Participant' (_post_-filtering)"),
               col.names = paste0("'FAY Participant' = '", labs.fay, "'"),
               justify = rep("right", length(labs.fay)))
    table(orig_x$assessment.type.code) %>%
        pander(caption = paste0(label.subjgr, " 'Assessment Type Codes' (_pre_-filtering)"),
               col.names = paste0("'Assessment Type Code' = '", labs.osa, "'"),
               justify = rep("right", length(labs.osa)))
    table(new_x$assessment.type.code) %>%
        pander(caption = paste0(label.subjgr, " 'Assessment Type Code' (_post_-filtering)"),
               justify = rep("right", length(labs.osa)))
    ftable(new_x$assessment.type.code, new_x$osa.performance.code,
           dnn = list("**'Assessment Type Code'**", "**'OSA Performance Code'**")) %>%
        pander(caption = paste0(label.subjgr, " 'Assessment Type Code' by ", label.subjgr, " 'OSA Performance Code' (_post_-filtering)"),
               justify = c("center", "center", rep("right", nlevels(new_x$osa.performance.code))))
    ftable(new_x$fay.participant, new_x$ccrpi.points, new_x$osa.performance.code,
           dnn = list("**'FAY Participant'**", "**'CCRPI Points'**", "**'OSA Performance Code'**")) %>%
        pander(caption = paste0("'FAY Participant' by ", label.subjgr, " 'OSA Performance Code' by ", label.subjgr, " 'CCRPI Points' (_post_-filtering)"),
               justify = c(rep("center", 3), rep("right", 6)))
    ftable(new_x$fay.participant, new_x$ccrpi.points, new_x$osa.performance.lvl,
           dnn = list("**'FAY Participant'**", paste0("**", label.subjgr, " 'CCRPI Points'**"),
                      "**_Recoded_ 'OSA Performance Code'**")) %>%
        pander(caption = paste0("'FAY Participant' by _Recoded_ ", label.subjgr, " 'OSA Performance Code' by ", label.subjgr, " 'CCRPI Points'"),
               justify = c("center", "center", "center", rep("right", 4)))
}

pander::pander(Rach_validate)
#'
#+ demo_rach, echo=FALSE
# **DEMO**: 'Rach()' QUALITY CONTROL CHECKS -----------------------------------------------------------


# Rach_validate(new_x = ach.ela.es0$new_x, orig_x = ela, label.subject = "ELA", label.grade = "ES")

## **DEMO**: Rach_validate() STEP-BY-STEP ================================
# table(x$fay.participant) %>% pander(caption = "ELA (ES) FAY Participant Values (pre-filtering)")
# x <- x[fay.participant == "Y"] ## RE-IMPLEMENTED ON 2018-05-09 ##
# table(x$fay.participant) %>% pander(caption = "ELA (ES) FAY Participant Values (_post_-filtering)")
# table(x$assessment.type.code) %>% pander(caption = "ELA (ES) Assessment Types")
# ftable(x$assessment.type.code, x$osa.performance.code, dnn = list("**Assessment Type**", "**OSA Performance Code**")) %>% pander(caption = "ELA (ES) Assessment Types by ELA (ES) Performance Codes")
# ftable(x$fay.participant, x$osa.performance.code, x$ccrpi.points, dnn = list("**FAY Participant**", "**OSA Performance Code**", "**CCRPI Points (_computed_)**")) %>% pander(caption = "Quality Check: FAY Participation by OSA Performance Code by Computed CCRPI Points")
# ftable(x$fay.participant, x$osa.performance.lvl, x$ccrpi.points, dnn = list("**FAY Participant**", "**_Recoded_ OSA Performance Code**", "**CCRPI Points (_computed_)**")) %>% pander(caption = "Quality Check: FAY Participation by Recoded OSA Performance Code by Computed CCRPI Points")
#'
#+ codedump_rach, echo=FALSE
# CODEDUMP (Rach()) -------------------------------------------------------

# Rach <- function(x, gradeband, groupvar = NULL, group = NULL, ...) { ## THIS VERSION DEPRECATED AS OF 2018-05-14 ##
#     setDT(x)
#     ## WEIGHTS TO BE APPLIED TO PERCENTAGES OF STUDENTS AT EACH ACHIEVEMENT LEVEL
#     ## (BEG/DNM = L1; DEV = L2; PRO/ADV = L3; DIS = L4) ##
#     rec.cmlvls <- c("'BEG' = 'L1'; 'DNM' = 'L1'; 'DEV' = 'L2'; 'PRO' = 'L3'; 'ADV' = 'L3'; 'DIS' = 'L4'") ## EOG/EOC/GAA PROFICIENCY LEVELS (ADDED ON 2018-05-09) ##
#     rec.cmpnts <- c("'BEG' = 0; 'DNM' = 0; 'DEV' = 0.5; 'PRO' = 1; 'ADV' = 1; 'DIS' = 1.5") ## EOG/EOC/GAA PROFICIENCY POINTS (ADDED ON 2018-05-09) ##
#     rec.wgt <- c("'L1' = 0; 'L2' = 0.5; 'L3' = 1; 'L4' = 1.5; else = 0")
#     cmwgts <- c(BEG = 0, DNM = 0, DEV = 0.5, PRO = 1, ADV = 0, DIS = 1.5) ## BEG/DNM = L1; DEV = L2; PRO/ADV = L3; DIS = L4 (MODIFIED ON 2018-05-09) ##
#
#     ## GRADEBAND INDEXES ##
#     if (gradeband == "MS") {
#         ## HIGH SCHOOLS ##
#         x <- x[student.grade.level %in% c(6, 7, 8)]
#     } else  if (gradeband == "HS") {
#         ## MIDDLE SCHOOLS ##
#         x <- x[student.grade.level >= 9]
#     } else if (gradeband == "ES") {
#         ## ELEMENTARY SCHOOLS ##
#         x <- x[student.grade.level <= 5]
#     } else stop("Unusable Grade Band")
#     ## FILTER ON 'fay.participant' == 'Y' ##
#     ## RE-IMPLEMENTED ON 2018-05-09 (PER D.JAFFE) ##
#     x <- x[fay.participant == "Y"]
#
#     ## IMPLEMENT GRADEBAND-SPECIFIC RULES:
#     ## ASSESSMENT TYPE (x$assessment.type.code):
#     ## ES & MS ('EM') == 'EOG', 'EOC' (MS ONLY), & 'GAA'##
#     if (gradeband %in% c("MS", "ES")) {
#         x <- x[assessment.type.code %in% c("EOG", "EOC", "GAA")]
#     } else if (gradeband == "HS") {
#         #> HS == 'EOC' & 'GAA' ##
#         x <- x[assessment.type.code %in% c("EOC", "GAA")]
#     }
#
#     ## FILTER ON GROUP IF SPECIFIED BY USER (SEE ARGS IN FUNCTION CALL ABOVE) ##
#     if (!is.null(groupvar)) {
#         x <- x[groupvar == group]
#     }
#
#     ## KEEP ONLY VALID PERFORMANCE CODES ##
#     x <- x[osa.performance.code %in% c("BEG", "DNM", "DEV", "PRO", "ADV", "DIS")]
#
#     ## ENSURE THAT THE ACH-PT LEVELS ARE ORDERED CORRECTLY IN REPORT TABLE OUTPUTS ##
#     x[, osa.performance.lvl := car::recode(x$osa.performance.code, rec.cmlvls)] ## SEE CM.LVLS DEF ABOVE ##
#
#     ## IDVARS ##
#     ids <- c("school.id", "gtid")
#
#     ## COMPUTE ACHIEVEMENT POINTS FOR EACH PERFORMANCE CODE ##
#     ## SELECT ONLY IDVARS AND THE COLUMN CONTAINING PERFORMANCE CODES DATA
#     ## ('x$osa.performance.lvl', DEFINED ABOVE) ##
#     xpcode <- x[, c(ids, "osa.performance.lvl"), with = FALSE]
#
#     ## RESHAPE SUBSETTED DATAFRAME TO WIDE FORMAT SO THAT:
#     ## THE FIRST COLUMN ('C1') CONTAINS THE 'gtid' FOR EACH STUDENT
#     ## EACH STUDENT HAS DATA ON ONE ROW
#     ## EACH SCHOOL HAS DATA IN ONE COLUMN
#     ## COLUMNS 2:N_SCHOOLS CONTAIN THE PERF. CODE ASSIGNED TO EACH STUDENT W/IN EACH SCHOOL ##
#     xpcodew <- reshape(xpcode,
#                        v.names = c("osa.performance.lvl"),
#                        idvar = "gtid",
#                        timevar = "school.id", direction = "wide")
#
#     ## TABULATE COUNTS OF EACH UNIQUE VALUE IN EACH COLUMN
#     ## ('L1' ('BEG'/'DNM'), 'L2' ('DEV'), 'L3' ('PRO'/'ADV'), 'L4' ('DIS'))
#     ## (EXCEPT C1, WHICH CONTAINS THE STUDENT IDs AND THEREFORE HAS AS MANY UNIQUE VALUES AS THERE ARE ROWS IN THE 'xpcodew`' DATAFRAME CREATED IN THE PREVIOUS LINE) ##
#     ## THE RESULTING TABLE OF COUNTS IS THEN TRANSPOSED ("`t()`") SO THAT
#     xpcode_n0 <- sapply(xpcodew[, -1], table) %>% t()
#     xpcode_n1 <- data.frame(school.id = gsub("osa\\.performance\\.lvl\\.", "",
#                                              rownames(xpcode_n0)), xpcode_n0)
#     xpcode_n2 <- merge(fcs_schools, xpcode_n1)
#     xpcode_n <- xpcode_n2[!duplicated(xpcode_n2), ]
#     xpcode_n$n <- apply(xpcode_n0, 1, sum)
#     xpcode_p <- data.frame(xpcode_n[, 1:2], round((xpcode_n0/xpcode_n$n)*100, 0))
#
#     xpcode_ptr <- t(xpcode_p[, -1:-2])
#     colnames(xpcode_ptr) <- gsub("osa\\.performance\\.code\\.", "", rownames(xpcode_n0))
#     xpcode_ptr1 <- xpcode_ptr*cmwgts
#     xpcode_pts <- t(xpcode_ptr1)
#     ach0 <- data.frame(school.id = rownames(xpcode_pts), ach = apply(xpcode_pts, 1, sum))
#     ach1 <- merge(fcs_schools, ach0)
#     ach <- ach1[!duplicated(ach1), ]
#
#     ## QUALITY CHECKS ##
#     # range(x[x$osa.performance.lvl == "L1", "assessment.scale.score"], na.rm = T)
#     # range(x[x$osa.performance.lvl == "L2", "assessment.scale.score"], na.rm = T)
#     # range(x[x$osa.performance.lvl == "L3", "assessment.scale.score"], na.rm = T)
#     # range(x[x$osa.performance.lvl == "L4", "assessment.scale.score"], na.rm = T)
#
#     ## PARTICIPATION RATE CALCULATION DEPRECATED AS OF 2018-05-09 (PER D.JAFFE) ##
#     ## PARTICIPATION RATE DENOMINATOR (NUMERATOR FOR ALL GRADEBANDS == 'x$test.participant'):
#     ## EM == 'x$test.window.enrollee' ##
#     # if (gradeband %in% c("MS", "ES")) {
#     #     prate_denom <- "test.window.enrollee"
#     #         ## HS == 'x$course.enrolled' ##
#     # } else if (gradeband == "HS") {
#     #     prate_denom <- "course.enrolled"
#     # }
#     # pr.p <- table(x$school.id, x$test.participant) %>% as.matrix
#     # pr.e <- table(x$school.id, x[, prate_denom]) %>% as.matrix
#     # pr <- data.frame(school.id = rownames(pr.p), prate = (pr.p[, "Y"]/pr.e[, "Y"])*100)
#     # ach3 <- merge(ach2, pr)
#     # ach <- within(ach3, {
#     #     ach.pr <- ifelse(prate < 95, ach*(prate/95), ach)
#     # })
#     return(ach)
# }
