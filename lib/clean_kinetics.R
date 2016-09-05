#' @param df The input dataframe from the CSVs.

clean_kinetics = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$`tap-pd_kinetics_device_testing`
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
  # Clean up key variables.

  df$testdate = as.Date(paste0(df$testdate, "/15"), "%m/%Y/%d")

  ################################
  # Ensure only one observation per patient.

  # Keep the first (earliest) record for each patient.
  df = df %>% group_by(patno) %>% arrange(testdate) %>% filter(row_number() == 1)

  ################################
  # Remove fields that we don't want to keep.

  # Too conservative:
  #df = subset(df, select = c(patno, qmat_keydown, qmat_pegcycle, qmat_keytran,
  #                           dom_side, aff_side))

  df = subset(df, select = -c(testdate, revision, dom_side, devicesn,
                              aff_side, sex, control, notes))

  ################################
  # Return the cleaned result.
  df
}
