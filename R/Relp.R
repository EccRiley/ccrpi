#' Compute \emph{Progress} Component EL Indicator Points (todo)
#'
#' @export
Relp <- function(x, ids = c(school = "school.id", student = "gtid", group = "student.grade.level")) {
    colnames(x) <- tolower(colnames(x)) ## PREEMPTIVELY FORCE ALL COLNAMES TO LOWER CASE ##
    colnames(x) <- gsub("[\\.]{2,}", ".", colnames(x), perl = TRUE) ## PREEMPTIVELY CLEAN EXCESS CONSECUTIVE "."s (CAUSED BY EXCESS CONSECUTIVE WHITESPACES) IN x'S COLNAMES ##
    ## NOTE: DEF OF 'elpcols' BELOW ASSUMES THE FOLLOWING COLUMNS EXIST IN 'x': 'cscore.prior' = 'Prior Year Composite Score' ('Y'); 2. 'Prior Year Performance Band' ('Z'); 3. 'Current Year Composite Score' ('AA'); 4. 'Current Year Performance Band' ('AB') ##
    ## ALSO NOTE: THE BELOW-DEFINED 'elpcols' VAR IS PURELY 'COSMETIC', BUT THE ADDED CODE-READABILITY WILL BE USEFUL FOR ANY DEBUGGING NEEDED FOR THIS FUNCTION IN THE FUTURE ##
    ## ALSO ALSO NOTE: THE BELOW CODE CREATES A COPY OF THE INPUT DF, 'x', WHICH ALSO SERVES A PRIMARILY DEBUGGING-SPECIFIC PURPOSE ##
    xnew <- plyr::rename(x, c("prior.year.composite.score" = "cscore.prior",
                              "prior.year.performance.band" = "pband.prior",
                              "current.year.composite.score" = "cscore.current",
                              "current.year.performance.band" = "pband.current"))

    ## IMPLEMENT ELP BUSINESS RULES (IT SHOULD BE CLEAR AT THIS POINT WHAT'S GOING ON IN THE NEXT CODE-CHUNK, IF NOT, THEN YOU HAVE NO BUSINESS EVALUATING/PROOFING THIS, OR ANY OTHER, CODE (IN ANY PROGRAMMING/SCRIPTING LANG)) ##
    xnew <- within(xnew, {
        ELP <- ifelse((pband.current <= pband.prior) & (cscore.current <= cscore.prior),
                      "L1", NA)
        ELP <- ifelse((pband.current == pband.prior) & (cscore.current > cscore.prior),
                      "L2", ELP)
        ELP <- ifelse((pband.current - pband.prior) == 1,
                      "L3", ELP)
        ELP <- ifelse((pband.current - pband.prior) > 1,
                      "L4", ELP)
    })

    ## THE REST IS ESSENTIALLY REPETITIVE OF PROCEDURES DONE VIA 'Rprgpts()', EXCEPT WITH NAMING CONVENTIONS SPECIFIC TO ELP, FOR NOW (TOOD: COMBINE Relp() & Rprgpts()) ##

    # ## REMOVE ANY RESULTING DUPLICATES ##
    xnew <- xnew[!duplicated(xnew), ]

    ## CREATE SUBSET DF ('xpts') OF 'xnew' CONTAINING ONLY ID VARS (SEE DEFAULT VALUES FOR THE 'ids' ARG FUN CALL LINE ABOVE) AND ELP POINT VAR (CREATED IN PREVIOUS CODE-CHUNK) ##
    xpts <- xnew[, c(Runname(ids), "ELP")]

    ## SET 'ELP' POINTS VAR AS A FACTOR, TO RESTRICT TO ONLY VALID VALUES (ALL OTHERS WILL BE COERCED TO 'NA's, WHICH WILL RESULT IN A WARNING THAT CAN BE USED FOR DEBUGGING AS NEEDED, WHICH IS, IMO, THE ONLY REAL REASON FOR SETTING A VAR TO A FACTOR, SO LONG AS YOU HAVE A PREDETERMINED SET OF VALID VALUES, FYI) ##
    xpts$ELP <- factor(xpts$ELP, levels = c(paste0("L", 1:4))) ## AGAIN, SEE 'ELP' VAR DEF ABOVE FOR REFERENCE ##

    ## RESHAPE 'xpts' TO WIDE FORMAT, WITH EACH COLUMN CONTAINING DATA FOR A UNIQUE SCHOOL (VIA 'school.id' BY DEFAULT), AND EACH ROW REPRESENTING A UNIQUE STUDENT'S (VIA 'gtid' BY DEFAULT) ELP POINT CONTRIBUTION (VIA 'ELP') PER GRADELEVEL/GROUP (VIA 'student.grade.level' BY DEFAULT) WITHIN A SCHOOL (AGAIN VIA 'school.id' BY DEFAULT)
    xptsw <- reshape(xpts, v.names = c("ELP"), idvar = Runname(c(ids[c("student", "group")])), timevar = "school.id", direction = "wide")
    ## FYI [TLDR: 'Runname()' IS A UTILITY FUN IN MY 'Riley' R-PKG ... RUN '?is.atomic' FOR MORE)]: 'Runname() IS A UTILITY FUN FROM MY 'Riley' R-PKG THAT STRIPS THE NAMES ATTRIBUTE FROM A GIVEN OBJECT (WHICH PARTICULARLY IS HELPFUL IN SITUATIONS LIKE THIS WHERE YOU WANT TO BE ABLE TO CALL, INLINE/EN-VIVO, AN 'ATOMIC' OBJECT'S ELEMENTS BY NAME, WHILE MAINTAINING THE OBJECT'S 'ATOMIC' IMPLEMENTATION IN THE CONTEXT OF THE THE SURROUNDING CALL. RUN '?is.atomic' IN THE R CONSOLE FOR MORE) ##

    ## COMPUTE NUMERATOR FOR EACH SCHOOL'S FINAL ELP POINTS, I.E., TABULATED COUNTS OF UNIQUE 'ELP' VALUES (SEE PREVIOUS DEF OF 'ELP' COLUMN FOR REFERENCE) WITHIN EACH COLUMN (I.E., SCHOOL), EXCEPT FOR THE STUDENT & GROUP ID VARS (AGAIN, SEE ABOVE) ##
    xpts_n0 <- sapply(xptsw[, -1:-2], table) %>%
        t() ## TRANSPOSE THE RESULTING TABULATIONS DF SO THAT STUDENT-GRADE COMBOSE ARE NOW ON COLUMNS, AND SCHOOLS ON ROWS ##

    ## CREATE NEW DF CONTAINING (1) SCHOOL.IDs (BY STIPPING R'S DEFAULT ROWNAMES PREFIX RESULTING FROM THE ABOVE TWO PROCEDURES), & (2) THE TABULATIONS RESULTING FROM THE IMMEDIATELY-ABOVE CODE-CHUNK ##
    xpts_n1 <- data.frame(school.id = gsub("ELP\\.", "", rownames(xpts_n0)), xpts_n0)

    xpts_n1$n <- apply(xpts_n0, 1, sum) ## COMPUTE DENOMINATOR FOR EACH SCHOOL'S FINAL ELP POINTS, I.E., TOTAL NUMBER OF STUDENTS WITH VALID ELP POINTS PER SCHOOL ##

    ## COMPUTE EACH SCHOOL'S FINAL ELP POINTS USING THE ABOVE COMPUTED NUMERFATOR AND DENOMINATOR PER SCHOOL ##
    xpts_p <- data.frame(school.id = xpts_n1[, 1], round((xpts_n0/xpts_n1$n)*100, 2))

    ## CLEAN UP FOR PRESENTATION ##
    xpts_p1 <- data.frame(school.id = xpts_n1[, 1], apply(format(round((xpts_n0/xpts_n1$n)*100, 2), nsmall = 2), 2, Ras.percent)) ## RECODE EACH SCHOOL'S COMPUTED ELP POINTS AS A PRECENTAGE ##
    # xpts_p1[, 3:6] <- apply(xpts_p1[, c], 2, function(x) ifelse(x == "NaN%", NA, x))

    ## MERGE THE RESULTING DF WITH THE 'fcs_schools' DF (SEE 'zmeta.R'), TO BRING IN SCHOOL NAMES ##
    xpts_n <- merge(fcs_schools, xpts_n1, all.y = TRUE)
    xpts_n <- xpts_n[!is.na(xpts_n$school.name), ] ## REMOVE ROWS WITH BLANK SCHOOL NAMES (CAUSED BY MERGE WITH FCS_SCHOOLS) ##
    xpts_p <- merge(fcs_schools, xpts_p, all.y = TRUE)
    xpts_p <- xpts_p[!is.na(xpts_p$school.name), ] ## REMOVE ROWS WITH BLANK SCHOOL NAMES (CAUSED BY MERGE WITH FCS_SCHOOLS) ##
    xpts_p1 <- merge(fcs_schools, xpts_p1, all.y = TRUE)
    xpts_p1 <- xpts_p[!is.na(xpts_p$school.name), ] ## REMOVE ROWS WITH BLANK SCHOOL NAMES (CAUSED BY MERGE WITH FCS_SCHOOLS) ##

    return(list(xpts_n = xpts_n, xpts_p = xpts_p, xpts_p1 = xpts_p1))
} ## THE RETURNED RESULTS MUST BE RUN THROUGH THE 'Rprg()', DEFINED BELOW FUN TO IMPLEMENT ACTUAL POINT WEIGHTS FOR ACTUAL FINAL PRG.ELP INDICATOR CALCULATION ##

# DEMO USAGE ----------------------------------------------------------
# source(zmeta.R) ## !!! WILL CAUSE ERROR (BC THIS FILE SOURCED IN 'zmeta.R') !!! - THIS LINE IS ONLY INCLUDED IN DEMO CODE HERE AS A REMINDER THAT 'zmeta.R' NEEDS TO BE SOURCED (TO SAVE SPACE / BC I'M LAZY AT THIS POINT) FOR DEMO CASE TO WORK (SPECIFICALLY NEEDED FROM zmeta.R ARE (1) THE 'fcs_schools' DF DEF & (2) LOADING OF THE 'Riley' & 'dplyr' (for the pipe only [TODO: INCLUDE PIPE IN Riley TO BYPASS TIDYVERSE DEPENDENCY]) R-PKGS ) ##

## TEST CASE: MS ELP - NOTE: I'M USING MS INSTEAD OF ES FOR DEMO BELOW, MY USUAL DEMO CASE, BC ELP POINTS WERE PREVIOUSLY CALCULATED BY D.JAFFE IN THE 2017 INPUT FILE, AND I WANT TO TEST THIS FUN FIRST ON A CASE WITHOUT PRE-CALC-ED ELP PT VALUES, THEN RE-VALIDATE USING THE PRE-CALC-ED DATA (I.E., ES ELP DATA)) ##

# elp.ms <- Rrdcsv("data/PRG-MS-ELP.csv")
#
# elppts.ms <- Relp(x = elp.ms)
# prgelp.ms <- Rprg(xpts_n = elppts.ms$xpts_n, xpts_p = elppts.ms$xpts_p)
# prg.elp.ms <- merge(prgelp.ms$prg.subj, elppts.ms$xpts_n[, c("school.id", "n")])
# names(prg.elp.ms)[ncol(prg.elp.ms)] <- "n_elp"
# ### (DEMO OUTPUT PRESENTATION TABLES) ####
# Rdt2(elppts.ms$xpts_n[, -1], caption = "ELP Band Counts by School", cnames = c("School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band", "N Total"))
# Rdt2(elppts.ms$xpts_p1[, -1], caption = "ELP Band Percentages by School", cnames = c("School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band"))
# Rdt2(prgelp.ms$prg.subj[, -1], caption = "Middle Schools' Progress toward ELP Indicator Points", cnames = c("School", "ELP Indicator Points"))
