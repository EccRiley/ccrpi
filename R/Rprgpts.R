#' Compute Student Growth Points (SGPs) for one subject area and across all grade levels at once (todo).
#'
#' @export
Rprgpts <- function(x, subjectarea, fayfilter = TRUE, assessmenttype = c("EOG", "EOC", "GAA"), ids = c("school.id", "gtid"), ptsvar = "x2018.points") {
    ## FILTER ON SPECIFIED SUBJECT AREA ##
    x0 <- x[x$assessmentsubjectareacode == subjectarea, ] %>% droplevels()
    ## INCLUDE ONLY VALID ASSESSMENT TYPES ##
    x0 <- x0[x0$assessmenttypecode %in% assessmenttype, , drop = FALSE] %>% droplevels()
    # x0 <- x0[x0$gradecluster == gradecluster, ] %>% droplevels()
    x0 <- merge(fcs_schools, x0) %>% droplevels()
    xnew <- x0[!duplicated(x0), ]

    ## FILTER ON FAY.PARTICIPANT IF APPLICABLE ##
    if (fayfilter == TRUE) {
        xnew <- xnew[xnew$fayparticipant == "Y", , drop = FALSE] %>% droplevels()
    }

    ## SUBSET TO INCLUDE ID COLUMNS AND SPECIFIED POINTS COLUMN ##
    xpts <- xnew[, c(ids, "gradecluster", "assessmentsubjectcode", ptsvar)]
    names(xpts) <- c(ids, "gradecluster", "assessmentsubjectcode", "pts")
    xpts$pts <- factor(xpts$pts)
    xpts <- xpts[!duplicated(xpts), ]
    xptsw <- reshape(xpts, v.names = c("pts"), idvar = c("gtid", "gradecluster", "assessmentsubjectcode"), timevar = "school.id", direction = "wide")

    xpts_n0 <- sapply(xptsw[, -1:-3], table) %>% t()

    xpts_n1 <- data.frame(school.id = gsub("pts.", "", rownames(xpts_n0)), xpts_n0)
    xpts_n2 <- merge(fcs_schools, xpts_n1)
    xpts_n <- xpts_n2[!duplicated(xpts_n2), ]
    xpts_n$n <- apply(xpts_n0, 1, sum)
    xpts_p <- data.frame(xpts_n[, 1:2], round((xpts_n0/xpts_n$n)*100, 2)) ## MODIFIED TO ROUND TO 2 DEC. PLACES, PER 2018 CALC. GUIDE, ON 2018-06-03 ##

    xpts_p1 <- data.frame(xpts_n[, 1:2],
                          apply(format(round((xpts_n0/xpts_n$n)*100, 2), nsmall = 2),
                                2, Ras.percent))
    return(list(xpts_n = xpts_n, xpts_p = xpts_p, xpts_p1 = xpts_p1))
}

Rprg <- function(xpts_n, xpts_p) {
    xpts_ptr0 <- t(xpts_p[, c("school.id", grep("[XL]\\d", names(xpts_p), value = TRUE))])
    colnames(xpts_ptr0) <- xpts_ptr0[1, ]
    xpts_ptr01 <- xpts_ptr0[-1, ]
    xpts_ptr <- apply(xpts_ptr01, 2, as.numeric)
    wgts <- c(0, 0.5, 1, 1.5)
    xpts_ptr1 <- xpts_ptr*wgts
    xpts_pts <- t(xpts_ptr1)
    prg.subj0 <- data.frame(school.id = rownames(xpts_pts), prg.subj = apply(xpts_pts, 1, sum))
    # prg.subj1 <- merge(xpts_n[, c("school.id", "n")], prg.subj0)
    prg.subj1 <- merge(fcs_schools, prg.subj0)
    prg.subj <- prg.subj1[!duplicated(prg.subj1), ]
    prg.subj$school.id <- as.numeric(as.character(prg.subj$school.id))
    return(list(xpts_pts = xpts_pts, prg.subj = prg.subj))
}

## DEMO (SGP - MATH - ALL SCHOOLS) ##

# sgp <- Riley::Rrdcsv("data/PRG-ALL-SGP.csv")
#
# ptsmath <- Rprgpts(x = sgp, subjectarea = "M")
#
# Rdt2(ptsmath$xpts_n[, -1], caption = "All Schools' Mathematics SGP Counts by School", cnames = c("School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band", "N Total"))
# Rdt2(ptsmath$xpts_p1[, -1], caption = "All Schools' Mathematics SGP %'s by School", cnames = c("School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band"))
#
# prgmath <- Rprg(xpts_n = ptsmath$xpts_n, xpts_p = ptsmath$xpts_p)
# Rdt2(prgmath$prg.subj[, -1], caption = "All Schools' Mathematics Student Growth", cnames = c("School", "Mathematics SGPs"))
