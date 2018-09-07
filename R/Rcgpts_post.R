#' Post-Process \emph{Closing Gaps} Points Data Resulting from \code{\link{Rcgpts}} (\code{DRAFT RD})
#'
#' @param x todo
#' @param lgrps todo
#' @param lgrpvars todo
#' @param lbsg todo
#'
#' @section See Also:
#'
#' \code{\link{Rcgpts}}, \code{\link{Rcgpts_sgt}}, \code{\link{Rach}}.
#'
#' @export
Rcgpts_post <- function(x,
                        lgrps = list(all = "all", ai = "I", as = "S", bl = "B", hp = "H",
                                     mr = "M", wh = "W", ed = "Y", el = "Y", swd = "Y"),
                        lgrpvars = list(all = "all", ai = "race.code", as = "race.code",
                                        bl = "race.code", hp = "race.code", mr = "race.code",
                                        wh = "race.code", ed = "ed", el = "el", swd = "swd"),
                        lbsg = list(ball = "all", bai = "ai", bas = "as", bbl = "bl", bhp = "hp",
                                    bmr = "mr",bwh = "wh", bed = "ed", bel = "el", bswd = "swd")) {
  #, keep_all = getOption("Riley.qc.keep_all")
  ### Rrecwgt() DEF ####
  rec.wgt <- c("'L1' = 0; 'L2' = 0.5; 'L3' = 1; 'L4' = 1.5; else = NA")

  Rrecwgt <- function(x) {
    car::recode(x, rec.wgt)
  }

  ### Rnflags() DEF
  Rnflags <- function(x) {
    ifelse(x == "L0", 0, 1)
  }

  cgm.subj0 <- x
  names(cgm.subj0) <- names(lgrps) ## CGM.SUBJ* ELEMENT NAMES CORRESPOND TO SUBGROUP LABELS ##

  ## ASSIGN SUBGROUP LABEL TO THE CLOSING GAPS POINTS COLUMN IN EACH OF CGM.ELA.ES0'S DATAFRAME ##
  cgm.subj01 <- lapply(seq_along(lgrps), function(x) plyr::rename(cgm.subj0[[x]], c("pts" = paste0("pts.", names(lgrps)[[x]]))))

  ## SUBSET EACH SUBGROUP'S DATAFRAME TO INCLUDE ONLY THE SCHOOL ID & THE SUBGROUP'S COMPUTED CG POINTS ##
  cgm.subj02 <- lapply(seq_along(lgrps), function(x) cgm.subj01[[x]][, c("school.id", paste0("pts.", names(lgrps)[[x]]))])
  cgm.subj03 <- lapply(seq_along(lgrps), function(x) {
    y <- cgm.subj02[[x]]
    y[, paste0("pts.", names(lgrps)[[x]])] <- as.character(y[, paste0("pts.", names(lgrps)[[x]])])
    return(y)
  })
  cgm.subj04 <- lapply(cgm.subj03, function(x) x[!duplicated(x), ])

  ## COMBINED ROWS OF ALL SUBGROUPS' COMPUTED CG POINTS DFs INTO A SINGLE DF ##
  cgm.subj05 <- Reduce(function(x, y) merge(x, y, all = TRUE), cgm.subj04)

  ## IMPLEMENT CLOSING GAPS POINT ASSIGNMENTS ACCORDING TO 'CG LEVEL' DETERMINED VIA THE ABOVE Rcgpts() IMPLEMENTATIONS BY SUBGROUP
  cgm.subj <- data.frame(school.id = cgm.subj05[, 1], apply(cgm.subj05[, -1], 2, Rrecwgt)) ### SEE 'zmeta.R' ###

  ### CNTS.ELA.ES - NUMBER OF CG FLAGS ####
  ## (CORRESPONDS TO EACH SCHOOL'S TOTAL '*POSSIBLE*' CG POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ...
  ## ... THE CURRENT SUBJECT'S FLAG COUNTS WILL BE COMBINED WITH THE REMAINING SUBJECTS' FLAG COUNTS FOR THE CURRENT GRADEBAND, RESULTING IN THE DENOMINATOR IN THE CURRENT GRADEBAND'S FINAL CLOSING GAPS COMPONENT SCORE CALCULATION) ##
  cnts.subj0 <- data.frame(school.id = cgm.subj05[, "school.id"], apply(cgm.subj05[, -1], 2, Rnflags)) ## SEE 'zmeta.R' ##
  cnts.subj <- data.frame(school.id = cnts.subj0[, 1], denom.subj = apply(cnts.subj0[, -1], 1, sum, na.rm = TRUE))

  ## RETURN CGM.SUBJ & CNTS.SUBJ ##
  # res <- ifelse(keep_all == TRUE, list(cgm.subj0 = cgm.subj0, cgm.subj01 = cgm.subj01,
  #                                      cgm.subj02 = cgm.subj02, cgm.subj03 = cgm.subj03,
  #                                      cgm.subj04 = cgm.subj04, cgm.subj05 = cgm.subj05,
  #                                      cgm.subj = cgm.subj[!duplicated(cgm.subj), ],
  #                                      cnts.subj0 = cnts.subj0,
  #                                      cnts.subj = cnts.subj[!duplicated(cnts.subj), ]),
  #               list(cgm.subj = cgm.subj[!duplicated(cgm.subj), ],
  #                    cnts.subj = cnts.subj[!duplicated(cnts.subj), ]))
  return(list(cgm.subj = cgm.subj[!duplicated(cgm.subj), ],
              cnts.subj = cnts.subj[!duplicated(cnts.subj), ]))
}
