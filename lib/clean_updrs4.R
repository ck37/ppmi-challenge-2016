#' @param df The input dataframe from the CSVs.
clean_updrs4 = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$mds_updrs_part_iv
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

  df$updrs_score_p4 = with(df, rowSums(cbind(np4wdysk, np4dyski, np4off,
                                               np4flcti, np4flctx, np4dystn)))
  summary(df$updrs_score_p4)
  hist(df$updrs_score_p4)


  # Save the latest UPDRS part 4 combined score, as well as the # of UPDRS4
  # records each patient has.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>%
    mutate(final_updrs_score_p4  = last(updrs_score_p4),
           updrs4_records = n())


  ################################
  # Ensure only one observation per patient.

  # Keep the first (earliest) record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(row_number() == 1)

  ################################
  #  fields that we want to keep.
  df = subset(df,select = c(patno, np4wdysk, np4dyski, np4off,
                             np4flcti, np4flctx, np4dystn, updrs_score_p4,
                            updrs4_records, final_updrs_score_p4))


  ################################
  # Return the cleaned result.
  df
}