#' Clean up the prodromal diagnostic questionnaire table.
#' @param df The input dataframe from the CSVs.
clean_prodromal_dq = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$prodromal_diagnostic_questionnaire
    dim(df)
    names(df)
    str(df)

    # Is there duplication at the patient level? Check # of records per patient id.
    dupes = df %>% group_by(patno) %>% summarize(pat_dupes=n())
    # Review duplication by patient (grouped):
    table(dupes$pat_dupes)
    sum(table(dupes$pat_dupes)[2:9])
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


  ################################
  # Ensure only one observation per patient.

  # Keep BL record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(event_id == "BL")

  ################################
  # Remove fields that we don't want to keep.

  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name,
                              orig_entry, last_update, query, site_aprv))


  ################################
  # Return the cleaned result.
  df
}
