#' Clean up the geriatric depression scale table.
#' @param df The input dataframe from the CSVs.
clean_depressionscale = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$geriatric_depression_scale__short_
    dim(df)
    names(df)
    str(df)
  }

  ################################
  # Clean up important variables.


  ################################
  # Ensure only one observation per patient.
  # Is there duplication at the patient level? Check # of records per patient id.
  dupes = df %>% group_by(patno) %>% summarize(pat_dupes=n())
  # Review duplication by patient (grouped):
  table(dupes$pat_dupes)
  # Unique patients:
  cat("Unique patients:", nrow(dupes),
      "Duplication %",  round((1 - nrow(dupes) / nrow(df))*100, 2), "\n")


  # Keep the first (earliest) record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(row_number() == 1)

  dim(df)
  ################################
  # Remove fields that we don't want to keep.
  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name,
                              infodt,
                              orig_entry, last_update, query, site_aprv))


  ################################
  # Return the cleaned result.
  df
}
