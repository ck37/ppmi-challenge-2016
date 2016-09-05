#' @param df The input dataframe from the CSVs.

clean_lineornt = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$benton_judgment_of_line_orientation
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
  # Derived variables.

  # Benton Judgment of Line Orientation Score
  col_names = paste0("bjlot", 1:30)
  df$benton_score = rowSums(df[, col_names], na.rm=T)
  hist(df$benton_score)
  if (F) {
    summary(df$benton_score)
  }

  ################################
  # Ensure only one observation per patient.

  # Keep the BASELINE record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(event_id %in% c("BL", "SCBL"))

  ################################
  # Remove fields that we don't want to keep.
  df = subset(df, select = c(patno, jlo_totcalc, dvs_jlo_mssae, benton_score))

  ################################
  # Return the cleaned result.
  df
}
