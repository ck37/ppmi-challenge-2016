#' @param df The input dataframe from the CSVs.
clean_updrs2 = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$mds_updrs_part_ii__patient_questionnaire
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
  # Clean up important variables.

  # Create summary score
  df$updrs_score_p2 <- with(df, rowSums(cbind(np2spch, np2salv, np2swal, np2eat,
                  np2dres, np2hygn, np2hwrt, np2hobb, np2turn, np2trmr, np2rise,
                  np2walk, np2frez)))

  summary(df$updrs_score_p2)
  hist(df$updrs_score_p2)


  # Save the latest UPDRS part 2 combined score, as well as the # of UPDRS2
  # records each patient has.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>%
    mutate(final_updrs_score_p2  = last(updrs_score_p2),
           updrs2_records = n())

  ################################
  # Ensure only one observation per patient.

  # Keep the first (earliest) record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(row_number() == 1)

  ################################
  # Remove fields that we don't want to keep.
  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name,
                              nupsourc, infodt,
                              orig_entry, last_update, query, site_aprv))


  ################################
  # Return the cleaned result.
  df
}