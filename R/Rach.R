#' \emph{Content Mastery} Subject-Area Indicators' Points
#'
#' Compute subject-area indicator points for the \emph{Content Mastery CCRPI} component, overall (per individual school, or across multiple schools within the same gradeband) or by subgroup(s).
#'
#' @param x A \code{dataframe} or object coercible to a \code{dataframe} containing \emph{\strong{student-level} GA Milestones} data containing all necessary columns (see additional arguments below) for computing \emph{\strong{school-level} Content Mastery} points for a single subject-area indicator.
#' @param gradeband A character vector of length 1 specifying the focal gradeband as one of the following: "ES" for elementary schools (grades 3 - 5), "MS" for middle schools (grades 6 - 8), or "HS" (grades 9 - 12).
#' @param grade_var A character vector of length 1 specifying the column name in \code{x} containing students' grade level
#' @param subject A character vector of length 1 specifying the focal subject area s one of the following: "ELA" for English/Language Arts, "MATH" for Mathematics, "SCI" for Science, or "SS" for Social Studies.
#' @param subject_var A character vector of length 1 specifying the column name in \code{x} containing the appropriate subject area codes.
#' @param subject_code A character vector specifying the actual codes used in \code{x$subject_var} for the subject area specified in \code{subject}.
#' @param assessment_type_var A character vector of length 1 specifying the column name in \code{x} containing the assessment types (e.g., "EOG", "EOC", "GAA", etc.).
#' @param assessment_type_codes A character vector specifying the acceptable values of \code{assessment_type_var} for use in computing content mastery scores.
#' @param fay_var A character vector of length 1 specifying the column name in \code{x} containing the \emph{FAY Participant} filter.
#' @param fay_code A character vector specifying acceptable values of \code{fay_var} for the \emph{FAY} filter.
#' @param performance_code_var A character vector of length 1 specifying the column name in \code{x} containing the assessment performance codes (e.g., "BEG", "DEV", "PRO", "DIS").
#' @param valid_performance_codes A character vector specifying the acceptable performance code values under \code{performance_code_var}.
#' @param rec.cmlvls A character vector of (pseudo) length 1 providing the \code{'recodes'} string to be passed to \code{\link[car]{car::recode()}} for the re-labeling \emph{Content Mastery} achievement levels.
#' @param rec.cmpnts A character vector of (pseudo) length 1 providing the \code{'recodes'} string to be passed to \code{\link[car]{car::recode()}} for assigning point values corresponding to the \emph{Content Mastery} achievement levels.
#' @param group_var An \emph{optional} character vector specifying the column name in \code{x} containing the appropriate variable for the focal subgroup (see \emph{Details} and \code{\link{Rcgpts}}).
#' @param group A character vector specifying the appropriate subgroup under \code{group_var} (ignored if \code{group_var} is \code{NULL}).
#' @param return_new_x logical. If \code{TRUE} (the default), the final returned value is a list containing (1) the manipulated version of the input dataframe (`x`), for QC purposes, and (2) the computed subject-level \emph{Content Mastery} points dataframe.
#' @param ... Additional arguments \emph{not yet implemented}.
#'
#' @return A list containing the following components:
#'
#' \enumerate{
#'     \item \code{\strong{'new_x'}}: The final `dataframe` used in computing the subject-area's \emph{Content Mastery} scores. The list below provides the columns included in \strong{\code{'new_x'}} and whether each column is from the original input dataframe \strong{\code{'x'}}, and if so any manipulations performed on the data in the column, or was computed as part of the process for computing the final scores. The primary differences between \strong{\code{'new_x'}} and the input \strong{\code{'x'}} dataframe is the added \code{\emph{"osa.performance.lvl"}} and \code{\emph{"ccrpi.points"}} columns. The former is a relabeled version of \code{x[["osa.performance.code"]]}, while \code{\emph{"ccrpi.points"}} contains the point-value assignment corresponding to each students' achievement level (per user-specified .
#'     \item \code{\strong{'cm.subj'}}: A dataframe containing the computed \emph{Content Mastery} score(s) for the user-specified subject-area and gradeband. The table below provides additional details on the columns returned in this dataframe.
#' }
#'
#' \tabular{rlll}{
#'   [,1] \tab school.id \tab integer \tab School ID number from \code{\strong{'x'}} \cr
#'   [,2] \tab N_Students.SUBJ \tab integer \tab Number of students with valid test scores \cr
#'   [,3] \tab SumPts.SUBJ \tab double \tab Count of achievement points earned across test takers \cr
#'   [,4] \tab AchPts.SUBJ \tab double \tab Subject-area \emph{achievement points} computed as \eqn{\frac{"ccrpi.points"}{"N_Students.SUBJ"}{"ccrpi.points"/"N_Students.SUBJ"}} (see \code{'new_x[["ccrpi.points"]]'} description above) \cr
#'   [,5] \tab AchPts_Cpd.SUBJ \tab double \tab \code{AchPts.SUBJ}, capped at 1.000 point (analygous 100%) \cr
#'   [,6] \tab AchPts_Wgtd.SUBJ \tab double \tab \code{AchPts_Cpd.SUBJ} weighted according to gradeband-specific weighting rules for the focal subject-area indicator \cr
#' }
#'
#' @export
Rach <- function(x, gradeband, grade_var = "student.grade.level",
                 subject_var, subject, subject_code,
                 assessment_type_var,
                 assessment_type_codes = c("EOG", "EOC", "GAA"),
                 fay_var, fay_code = "Y",
                 performance_code_var = "osa.performance.code",
                 valid_performance_codes = c("BEG", "DNM", "DEV", "PRO", "ADV", "DIS"),
                 rec_cmlvls = c("'BEG' = 'L1'; 'DNM' = 'L1'; 'DEV' = 'L2'; 'PRO' = 'L3'; 'ADV' = 'L3'; 'DIS' = 'L4'"),
                 rec_cmpnts = c("'BEG' = 0; 'DNM' = 0; 'DEV' = 0.5; 'PRO' = 1; 'ADV' = 1; 'DIS' = 1.5"),
                 group_var = NULL, group = NULL, return_new_x = TRUE, ...) {

    ## 'newxvars': LIST OF ALL ESSENTIAL VARS (INCL. SUBGROUPS' VARS) FOR CONTENT MASTERY OUTPUT DF ##
    newxvars <- c("school.id", "school.name", "school.year", "N_Students", "gtid",
                  "fay.participant", grade_var, "gender.code", "race.code", "el", "ed", "swd", "all",
                  assessment_type_var, performance_code_var, "osa.performance.lvl",
                  "SumPts", "AchPts", "AchPts_Cpd", "AchPts_Wgtd", "ccrpi.points")
    ## NOTE: ['SumPts', 'AchPts', 'AchPts_Cpd', 'AchPts_Wgtd', & 'CCRPI_POINTS'] ARE CREATED BELOW ##

    ### RESTRICT X TO SPECIFIED GROUP (IF APPLICABLE) ####
    if (!is.null(group_var)) {
        x <- x[group_var == group, ] %>% droplevels()
    }
    ### FILTER ON FAY & FOCAL SUBJECT AREA ####
    ## (... & CREATE 'new_x' AS A DATA.TABLE-CLASSED COPY OF 'x', RETAINING ONLY VARIABLES CM-RELEVANT) ##
<<<<<<< HEAD
    new_x <- x[x[[fay_var]] == fay_code & x[[subject_var == subject_code]],
               names(x) %in% newxvars, drop = FALSE]
=======
    new_x <- x[x$fay.participant == "Y" & x$, names(x) %in% newxvars] %>% droplevels()
>>>>>>> a12f45da00829b039a73c02fb4c3fe01a543bc4d
    library(data.table)

    ### RESTRICT TO VALID ASSESSMENT TYPES ('assessment_type_var'): ####
    new_x <- new_x[new_x[[assessment_type_var]] %in% assessment_type_codes]

    ### RESTRICT TO APPROPRIATE GRADE LEVELS BASED ON USER-SPECIFIED GRADEBAND ####
    if (gradeband == "MS") {
        ## HIGH SCHOOLS ##
        new_x <- new_x[x[[grade_var]] %in% c(6, 7, 8)]
    } else  if (gradeband == "HS") {
        ## MIDDLE SCHOOLS ##
        new_x <- new_x[x[[grade_var]] >= 9]
    } else if (gradeband == "ES") {
        ## ELEMENTARY SCHOOLS ##
        new_x <- new_x[x[[grade_var]] <= 5]
    } else stop("Unusable Grade Band")

    ### RESTRICT TO VALID ACHIEVEMENT LEVEL CODES ####
    new_x <- new_x[x[[performance_code_var]] %in% valid_performance_codes]

    ### RECODE OSA.PERFORMANCE.CODE ####
    ## (... TO ENSURE THAT THE ACH-PT LEVELS ARE ORDERED CORRECTLY IN TABULATED OUTPUTS LATER) ##
    new_x[, performance.lvl := car::recode(x[[performance_code_var]], rec.cmlvls)] ## SEE 'rec.cmlvls' DEF ABOVE ##

    ### CREATE NEW "ccrpi.points" COLUMN (PER D.JAFFE) <==> EACH STUDENT'S ACH. PTS. EARNED ON THE INPUT SUBJ. ##
    new_x[, ccrpi.points := car::recode(x[[performance_code_var]], rec.cmpnts)] ## SEE 'rec.cmpnts' DEF ABOVE ##

    cm.subj <- new_x[, .(N_Students = .N, ## COMPUTE NUMBER OF VALID TEST RESULTS FOR THE FOCAL SUBJ. ##
                         SumPts = sum(ccrpi.points), ## COMPUTE TOTAL ACH.PTS. EARNED ON THE FOCAL SUBJ. ##
                         AchPts = sum(ccrpi.points)/.N), ## COMPUTE SUBJ. INDICATOR'S CCRPI PTS ##
                     by = key(new_x)] ## DO ALL OF THE ABOVE BY SCHOOL ##

    ### ... (NOTE: SUBJ. INDICATOR CCRPI PTS ARE COMPUTED AS A FUNCTION OF [TOTAL ACH.PTS. EARNED] DIVIDED BY  [NUMBER OF VALID TEST RESULTS] FOR THE FOCAL SUBJ. - THE RESULTING SCHOOL-LEVEL INDICATOR SCORE IS ROUNDED TO 2 DECIMAL PLACES, PER 2018 CALCULATION GUIDE) ###

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
    cm.subj[, AchPts_Wgtd := AchPts_Cpd*cmwgt]

    ## APPEND SUBJ. LABEL TO COMPUTED VARIABLES' COLNAMES ##
    names(cm.subj)[-1] <- paste0(names(cm.subj)[-1], ".", subject)
    setnames(new_x, "ccrpi.points", paste0("ccrpi.points.", subject))

    ### RETURN OUTPUT(S) ####
    ## BY DEFAULT, 'new_x' DF IS RETURNED ALONG WITH CM.SUBJ. DF ##
    if (return_new_x == TRUE) {
        return(list(new_x = new_x, cm.subj = cm.subj))
    } else return(cm.subj)
}


#'
#' # `Rach_validate()`
#'
#' @export
Rach_validate <- function(new_x, orig_x, label.subject, label.grade) {
    library(dplyr)
    ### INCLUDE ALL ORIGINAL VALUES IN LEVELS OF NEW_X ####
    labs.fay <- unique(na.omit(orig_x$fay.participant))
    labs.osa <- unique(na.omit(orig_x[[assessment_type_var]]))
    new_x$fay.participant <- factor(new_x$fay.participant,
                                    levels = labs.fay)
    new_x[[assessment_type_var]] <- factor(new_x[[assessment_type_var]],
                                         levels = labs.osa)
    new_x[[performance_code_var]] <- factor(new_x[[performance_code_var]],
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
    ftable(new_x$assessment.type.code, new_x[[performance_code_var]],
           dnn = list("**'Assessment Type Code'**", "**'OSA Performance Code'**")) %>%
        pander(caption = paste0(label.subjgr, " 'Assessment Type Code' by ", label.subjgr, " 'OSA Performance Code' (_post_-filtering)"),
               justify = c("center", "center", rep("right", nlevels(new_x[[performance_code_var]]))))
    ftable(new_x$fay.participant, new_x$ccrpi.points, new_x[[performance_code_var]],
           dnn = list("**'FAY Participant'**", "**'CCRPI Points'**", "**'OSA Performance Code'**")) %>%
        pander(caption = paste0("'FAY Participant' by ", label.subjgr, " 'OSA Performance Code' by ", label.subjgr, " 'CCRPI Points' (_post_-filtering)"),
               justify = c(rep("center", 3), rep("right", 6)))
    ftable(new_x$fay.participant, new_x$ccrpi.points, new_x$performance.lvl,
           dnn = list("**'FAY Participant'**", paste0("**", label.subjgr, " 'CCRPI Points'**"),
                      "**_Recoded_ 'OSA Performance Code'**")) %>%
        pander(caption = paste0("'FAY Participant' by _Recoded_ ", label.subjgr, " 'OSA Performance Code' by ", label.subjgr, " 'CCRPI Points'"),
               justify = c("center", "center", "center", rep("right", 4)))
}
