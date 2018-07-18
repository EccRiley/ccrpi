Rlxmid <- function(x, grades, gradevar = "student.grade.level",
                   midpoints, fayfilter = TRUE) { ## NOTE: 'MIDPOINTS' & 'GRADES' SHOULD BE VECTORS OF THE SAME LENGTH ##
    ## RESTRICT TO SPECIFIED GRADES ##
    x0 <- x[x[, gradevar] %in% c(grades), , drop = FALSE] %>% droplevels()

    ## (CONDITIONALLY, BASED ON INPUT ARG) FILTER ON FAY PARTICIPANT == 'Y' ##
    ## SPECIFYING AS CONDITIONAL TO ALLOW EASIER ON/OFF OF FAY.PARTICIPANT FILTER (BC THIS FILTER KEEPS BEING TURNED ON & OFF) ##
    if (fayfilter == TRUE) {
        xnew <- x0[x0$fay.participant == "Y", , drop = FALSE] %>% droplevels()
    }
    ## ADD COLUMN CONTAINING GRADE-SPECIFIC LEXILE MIDPOINTS (FOR REFERENCE) ##
    rec.midpoints <- paste0("'", grades, "'='", midpoints, "'", collapse = "; ")
    xnew$lexile.midpoint <- car::recode(xnew[, gradevar], rec.midpoints)
    ## IMPLEMENT BIZ RULES FOR LEXILE POINTS ##
    ## ASSIGN VALUE OF '1' TO STUDENTS WITH LEXILE SCORE GREATER THAN OR EQUAL TO SPECIFIED MIDPOINT FOR THEIR GRADEBAND,
    ## OTHERWISE, ASSIGN VALUE OF '0' ##
    xnew$lexile.readiness.point <- ifelse(xnew$lexile.scale.score >= xnew$lexile.midpoint, 1, 0)

    ## RETURN NEW DF CONTAINING ONLY THE ESSENTIALS FOR LEXILE READINESS INDICATOR ##
    return(xnew[, c("school.id", "gtid", gradevar, "fay.participant", "lexile.scale.score", "lexile.midpoint", "lexile.readiness.point")])
}
