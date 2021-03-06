#' @param df The input dataframe from the CSVs.


clean_currcond = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$current_medical_conditions_log
    dim(df)
    names(df)
    str(df)

    # Is there duplication at the patient level? Check # of records per patient id.
    dupes = df %>% group_by(patno) %>% summarize(pat_dupes=n())
    # Review duplication by patient (grouped):
    table(dupes$pat_dupes)
    # Unique patients:
    cat("Unique patients:", nrow(dupes),
        "Duplication %",  round((1 - nrow(dupes) / nrow(df))*100, 2), "\n")
    # Append dupes to dataframe.
    dupes = df %>% group_by(patno) %>% mutate(pat_dupes=n()) %>% arrange(patno)
    # Review duplication by observation:
    table(dupes %>% ungroup() %>% select(pat_dupes))
    # Review records with duplication, looking at key administrative fields.
    subset(dupes, pat_dupes > 1, c(patno, pat_dupes, event_id, f_status,
                                   orig_entry, last_update, site_aprv))

    # Visually review full data that has duplication.
    View(subset(dupes, pat_dupes > 1))
  }


  ################################
  # Remove fields that we don't want to keep.
  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name, seqno,
                              dxyrest, condterm, orig_entry, last_update, query, site_aprv,
                             resyr, llt_name, pt_code, pt_name, verbatim,
                              vmeddra, hlt_name, hlgtname))


  ################################
  # Clean up important variables.

  # Condition category.
  df$condcat = as.factor(df$condcat)

  # System organ class 1
  df$socabbr1[df$socabbr1 == ""] = NA
  df$socabbr1 = as.factor(df$socabbr1)
  ################################

    # reshape wide by  condcat
  df <- reshape(df, timevar = c("condcat"), idvar = c("patno"),
                direction = "wide")

  dim(df)

##still need to get this working
# need to make these all indicator variables
#for (i in  c(socabbr1.1P, socabbr1.1E)) {
   # i[ is.na(i) ] <- 0
    #i[]}




  ################################
  # Return the cleaned result.
  df
}
