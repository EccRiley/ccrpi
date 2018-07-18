Rindicators <- function(x, idvar, v.names, timevar, benchmark = 1){
    x[, v.names] <- factor(x[, v.names])
    xpw <- reshape(x, v.names = v.names, idvar = idvar, timevar = timevar, direction = "wide")
    xp_n0 <- sapply(xpw[, -1], table) %>% t() %>% as.data.frame()
    xp_n0$n <- apply(xp_n0, 1, sum)
    xp_n1 <- data.frame(school.id = gsub(paste0(v.names, "\\."), "", rownames(xp_n0)), xp_n0)
    xp_n2 <- merge(fcs_schools, xp_n1)
    xp_n <- xp_n2[!duplicated(xp_n2), ]

    ## COMPUTE INDICATOR PERFORMANCE POINT POINTS ##
    pts.indicator <- round((xp_n[, 4]/xp_n$n)*100, 0)

    ## ADDED BENCHMARK OPTION AND IMPLEMENTATION ON 20180607 ##
    xp_p <- data.frame(xp_n[, 1:2], N.met.indicator = xp_n[, 3], N.students.total = xp_n$n,
                       pts = pts.indicator, benchmark = benchmark, pts.adj = pts.indicator/benchmark)
    return(xp_p)
}
