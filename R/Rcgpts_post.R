#' # Post-Process results of '`Rcgpts()`' Output Data
#'
#' @param x todo
#'
## [Rcgpts() POST-PROCESSING] =========================
Rcgpts_post <- function(x) {
    cgm.subj0 <- x
    names(cgm.subj0) <- names(subgroups) ## CGM.SUBJ* ELEMENT NAMES CORRESPOND TO SUBGROUP LABELS ##

    ## ASSIGN SUBGROUP LABEL TO THE CLOSING GAPS POINTS COLUMN IN EACH OF CGM.ELA.ES0'S DATAFRAME ##
    cgm.subj01 <- lapply(seq_along(subgroups), function(x) plyr::rename(cgm.subj0[[x]], c("pts" = paste0("pts.", names(subgroups)[[x]]))))

    ## SUBSET EACH SUBGROUP'S DATAFRAME TO INCLUDE ONLY THE SCHOOL ID & THE SUBGROUP'S COMPUTED CG POINTS ##
    cgm.subj02 <- lapply(seq_along(subgroups), function(x) cgm.subj01[[x]][, c("school.id", paste0("pts.", names(subgroups)[[x]]))])
    cgm.subj03 <- lapply(seq_along(subgroups), function(x) {
        y <- cgm.subj02[[x]]
        y[, paste0("pts.", names(subgroups)[[x]])] <- as.character(y[, paste0("pts.", names(subgroups)[[x]])])
        return(y)
    })
    cgm.subj04 <- lapply(cgm.subj03, function(x) x[!duplicated(x), ])

    ## COMBINED ROWS OF ALL SUBGROUPS' COMPUTED CG POINTS DFs INTO A SINGLE DF ##
    cgm.subj05 <- Reduce(function(x, y) merge(x, y, all = TRUE), cgm.subj04)

    ## IMPLEMENT CLOSING GAPS POINT ASSIGNMENTS ACCORDING TO 'CG LEVEL' DETERMINED VIA THE ABOVE Rcgpts() IMPLEMENTATIONS BY SUBGROUP
    cgm.subj <- data.frame(school.id = cgm.subj05[, 1], apply(cgm.subj05[, -1], 2, function(x) car::recode(x, rec.wgt))) ### SEE 'zmeta.R' ###

    ### CNTS.ELA.ES - NUMBER OF CG FLAGS ####
    ## (CORRESPONDS TO EACH SCHOOL'S TOTAL '*POSSIBLE*' CG POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ...
    ## ... THE CURRENT SUBJECT'S FLAG COUNTS WILL BE COMBINED WITH THE REMAINING SUBJECTS' FLAG COUNTS FOR THE CURRENT GRADEBAND, RESULTING IN THE DENOMINATOR IN THE CURRENT GRADEBAND'S FINAL CLOSING GAPS COMPONENT SCORE CALCULATION) ##
    cnts.subj0 <- data.frame(school.id = cgm.subj05[, "school.id"], apply(cgm.subj05[, -1], 2, function(x) ifelse(x == "L0", 0, 1))) ## SEE 'zmeta.R' ##
    cnts.subj <- data.frame(school.id = cnts.subj0[, 1], denom.subj = apply(cnts.subj0[, -1], 1, sum, na.rm = TRUE))

    ## RETURN CGM.SUBJ & CNTS.SUBJ ##
    return(list(cgm.subj = cgm.subj[!duplicated(cgm.subj), ],
                cnts.subj = cnts.subj[!duplicated(cnts.subj), ]))
}
