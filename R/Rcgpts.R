#' \emph{Closing Gaps} Points Earned by Subgroup (in progress)
#'
#' Compute a given subgroup's \emph{Closing Gaps} points for a given GA Milestones assessment subject & gradeband using the baselines & targets data generated via \code{\link{Rach}} for that subgroup on that assessment in that gradeband (i.e., must be implemented on each subgroup's data for each assessment subject within each gradeband).
#'
#' @param x todo
#' @param df.baseline todo
#' @param df.target todo
#' @param subject todo
#' @param subgroups todo
#' @param in_vivo_qc todo
#'
#' @export
Rcgpts <- function(x,
    df.baseline,
    df.target,
    subject,
    subgroups,
    cat_var = "reporting.caategory") {

    ## PREEMPTIVELY ENSURE THAT INPUT DATA ARE ONLY 'DATAFRAME' CLASS (NOT 'DATA.TABLE') ##
    x <- as.data.frame(x)
    df.baseline <- as.data.frame(df.baseline)
    df.target <- as.data.frame(df.target)

    ## SUBJECT-SPECIFIC LABELS FOR BASELINE & TARGETS COLUMNS ##
    labs <- c(paste0("baseline.", subject), paste0("target.baseline.", subject),
              paste0("target.baseline.", subject, "2"))
    
    ## COMBINE CURRENT ('x'), BASELINE ('df.baseline'), & TARGETS ('df.target') INPUT DFs ##
    x1 <- merge(x, df.baseline[, c("school.id", cat_var, labs[1])], all = TRUE) ## [CURRENT]+[BASELINE] DATA ##
    x2 <- merge(x1, df.target[df.target$target.level == "t1", c("school.id", cat_var, labs[2])], all = TRUE) ## [CURRENT+BASELINE]+[3% TARGET ('t1')] ##

    ## RENAME INPUT TARGET DATA'S  [TARGET VALUES] COLUMN TO REPRESENT 't2' TARGET (FOR MERGE IN THE SUBSEQUENT LINES RESULTING IN 'xx') ##
    df.target2 <- df.target
    names(df.target2)[which(names(df.target2) == labs[2])] <- labs[3]

    ## [[CURRENT+BASELINE]+[3% TARGET ('t1')]]+[6%TARGET ('t2')] DATA ##
    xx <- merge(x2[, c("school.id", cat_var,
                       paste0(c("N_Students.",
                                "AchPts_Cpd.",
                                "baseline.",
                                "target.baseline."), subject))],
                df.target2[df.target2$target.level == "t2",
                           c("school.id", cat_var, labs[3])], all = TRUE)
    
    ## PROGRAMMATICALLLY, VIA REGEX, RENAME XX'S COLUMNS TO AVOID POSSIBLE MIS-NAMING DUE TO MISMATCHED COLUMN ORDERING ##
    ### NOTE: THE BELOW LINES COULD ACTUALLY BE TAKEN CARE IN ONLY TWO LINES, AND PROBABLY MORE ELEGANTLY, ...
    ### ... BUT I'M TAKING THE LONG ROUTE HERE FOR CLARITY/TRANSPARENCY, GIVEN THE (OBNOXIOUS) COMPLEXITY OF THE CLOSING GAPS COMPONENT ###
    colnames(xx) <- gsub("^N_Students\\.[A-Z]+$", "N", colnames(xx))
    colnames(xx) <- gsub("^baseline\\.[A-Z]+$", "baseline", colnames(xx))
    colnames(xx) <- gsub("^target\\.baseline\\.[A-Z]+$", "target.baseline", colnames(xx))
    colnames(xx) <- gsub("^target\\.baseline\\.[A-Z]+2$", "target.baseline2", colnames(xx))
    colnames(xx) <- gsub("^AchPts_Cpd\\.[A-Z]+$", "ach.cm", colnames(xx))
    ## NOTE: REPLACED ALL INSTANCES OF 'ach.pr' WITH 'ach.cm' ON 2018-06-12 ##
    ### ... 'ach.pr' WAS USED IN THE ORIGINAL VERSION OF Rcgpts() (WRITTEN & IMPLEMENTED IN 2018-02) ###
    ### ... TO DENOTE ACHIEVEMENT POINTS WEIGHTED BASED ON PARTICIPATION RATE ...
    ### ... HOWEVER, AS OF 2018-05-??, THE PARTICIPATION RATE BUSINESS RULE IS NOT CURRENTLY IMPLEMENTED ###

    ## CONDITIONALLY ASSIGN POINT VALUES BASED ON USER-SPECIFIED OPTION FOR 'both_targets' ARG ...
    ### ... SEE x.sgt.tg DEF IN Rcgpts_sgt(), WHICH AUTO-FILTERS WHETHER ONE OR BOTH TARGETS SHOULD BE USED ###
    x.pts <- within(xx, {
        baseline <- ifelse(baseline == -1, NA, baseline)
        target.baseline <- ifelse(is.na(baseline), NA, target.baseline)
        target.baseline2 <- ifelse(is.na(baseline), NA, target.baseline2)

        ## DON'T APPLY POINTS TO SUBGROUPS WITH LESS THAN 15 RESULTS ##
        ach.cm <- ifelse(N < 15, NA, ach.cm)

        ## MODIFIED AND TESTED 'PTS' ASSIGNMENT LOGIC & FLOW ON 2018-06-12 ##
        pts.l1 <- ifelse((ach.cm <= baseline), 1, 0)
        pts.l2 <- ifelse((ach.cm > baseline) & (ach.cm < target.baseline), 2, 0)
        pts.l3 <- ifelse((ach.cm >= target.baseline), 3, 0)
        pts.l4 <- ifelse((ach.cm >= target.baseline2), 4, 0)
    })

    ## ADDED (AS PART OF MODIFIED 'PTS' ASSIGNMENT FLOW) ON 2018-06-02 ##
    x.pts$pts.max <- apply(x.pts[, c("pts.l1", "pts.l2", "pts.l3", "pts.l4")], 1, max, na.rm = TRUE)
    x.pts$pts.max <- ifelse(is.infinite(x.pts$pts.max), NA, x.pts$pts.max) ## IN CASE ANY ROWS CONTAIN ALL 'NAs', THEREFORE CAUSING '-Inf' AS RESULT OF 'max(...)' IMPLEMENTATION ABOVE ##
    x.pts$pts <- ifelse(is.na(x.pts$pts.max), NA, paste0("L", x.pts$pts.max))
    if (in_vivo_qc == TRUE) { cat("'x.pts' (intermediate):\n"); print(x.pts, digits = 4) } ## IN VIVO QC ##

    x.pts <- x.pts[, c("school.id", cat_var, "N", "ach.cm", "baseline", "target.baseline", "target.baseline2", "pts")]

    ## RETURN INPUT SUBGROUP'S COMPUTED CLOSING GAPS POINTS ON THE SPECIFIED 'subject' ##
    return(list(xx = xx, xpts = x.pts))
}
