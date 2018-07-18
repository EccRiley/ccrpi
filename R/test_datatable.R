library(data.table)
x <- data.table(ltrs = sample(letters, size = length(letters), replace = TRUE),
                num_1 = 1:length(letters),
                num_2 = rnorm(length(letters), mean = 0.5))
x
testvar <- expression(ltrs)
f <- function(x, v, val) {
    return(copy(x[x$v == val]))
}
f(x = x, v = ltrs, val = "b")


# x <- data.frame(ltrs = sample(letters, size = length(letters), replace = TRUE),
#                 num_1 = 1:length(letters),
#                 num_2 = rnorm(length(letters), mean = 0.5))
# x

f <- function(x, v, val) {
    x[x[[v]] == val]
}
f(x = x, v = "ltrs", val = "w")

newvar <- "new_var"

x[[newvar]] <- "newvar222"

x[, LTRS := car::recode(x[["ltrs"]], c("'w'=99"))]
