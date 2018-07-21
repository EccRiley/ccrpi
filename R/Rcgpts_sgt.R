#' Pre-Process Input Data for \code{\link{Rcgpts}} (todo)
#'
#' @param xw
#' @param category_var
#' @param baseline_vars_pattern
#' @param group_labs
#' @param ... Additional arguments \emph{not yet implemented}.
#'
#' @export
Rcgpts_sgt <- function(xw,
                       category_var = "reporting.category",
                       baseline_vars_pattern = "baseline\\.",
                       group_labs = c("all", "ai", "as", "bl", "hp",
                                      "mr", "wh", "ed", "el", "swd")) {

    xw <- as.data.frame(xw) ## ENSURE THAT THE INPUT ('xw') IS A DATAFRAME ... ##
    ## ... IN CASE INPUT OBJECT IS AN OBJECT OF A NON-DATA.FRAME CLASS (E.G., MATRIX, DATA.TABLE) ##

    ## CREATE LIST OF CONTAINING A SUBSET OF 'xw' FOR EACH SUBGROUP ##
    x.sgt.bl <- lapply(group_labs, function(x) xw[xw[[category_var]] == x, ])

    ## GET LIST OF COLUMN NAMES CONTAINING BASELINES FOR EACH SUBGROUP ##
    baseline_vars <- lapply(names(x.sgt.bl),
                            function(x) grep(baseline_vars_pattern,
                                             names(x.sgt.bl[[x]]),
                                             value = TRUE))
    ## COMPUTE 3% ('t1') & 6% ('t2') TARGETS FOR ALL SUBJECT AREAS BY SUBGROUP ##
    x.sgt <- lapply(1:length(x.sgt.bl), function(x)
        data.frame(school.id = x.sgt.bl[[x]]$school.id,
                   cat = x.sgt.bl[[x]][[category_var]],
                   target =
                       apply(x.sgt.bl[[x]][, baseline_vars[[x]]], 2,
                             function(z) {
                                 t1 <- ifelse(z < 90.00, ((100 - z)*0.03) + z, 90.00)
                                 t2 <- ifelse(z < 95.00, ((100 - z)*0.06) + z, 95.00)
                                 return(c(t1 = t1, t2 = t2))
                             }
                       ))
    )

    ## LABEL TARGET LEVELS ("T1" = 3%; "T2" = 6%) ##
    x.sgt.tg0 <- lapply(x.sgt, function(x) {
        y <- x
        y$target.level <- gsub("(t\\d)\\..*?$", "\\1", rownames(x))
        return(y)
    })

    ## ONLY ASSIGN LEVEL 2 TARGET FOR ED, EL, & SWD SUBGROUPS ##
    x.sgt.tgw <- lapply(x.sgt.tg0, function(x) {
        w <- reshape(x[!duplicated(x), ], idvar = c("school.id", "cat"),
                     timevar = "target.level", direction = "wide")
        return(w)
    })

    library(data.table)
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

    names(x.sgt.bl) <- group_labs
    names(x.sgt.tg) <- group_labs

    return(list(x.sgt.bl = x.sgt.bl, x.sgt.tg = x.sgt.tg))
}
