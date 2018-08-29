#' Generate Subgroups' \emph{Content Mastery} Baseline Data (todo)
#'
#' @export
Rcmbl <- function(bl.subj, gradecluster, gradevar, subject, subject_var, subject_code, assessment_type_var = subject_var, 
                  assessment_type_codes = subject_code, fay_var = subject_var, fay_code = subject_code, performance_code_var,
                  lgrps <- list(all = "all", ai = "I", as = "S", bl = "B", hp = "H", 
                                mr = "M", wh = "W", ed = "Y", el = "Y", swd = "Y"),
                  lgrpvars <- list(all = "all", ai = "race.code", as = "race.code", 
                                   bl = "race.code", hp = "race.code", mr = "race.code", 
                                   wh = "race.code", ed = "ed", el = "el", swd = "swd"),
                  lbsg <- list(ball = "all", bai = "ai", bas = "as", bbl = "bl", bhp = "hp", 
                               bmr = "mr",bwh = "wh", bed = "ed", bel = "el", bswd = "swd"),
                  ...) { ## '...' NOT CURRENTLY IMPLEMENTED ##
    
    ### PREMPTIVELY ENSURE THAT "BL.SUBJ" IS ONLY A DATA.FRAME OBJECT (NOT A DATA.TABLE OBJ.) ###
    bl.subj <- as.data.frame(bl.subj)
    
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
