#' @param df The input dataframe from the CSVs.

clean_biospecimen_analysis = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$biospecimen_analysis_results
    dim(df)
    names(df)
    str(df)

    table(df$clinical_event)

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
  # Clean up variables.

  # Rundate actually has full Y-%m-%d already! No need to paste0 a "/15".
  df$rundate = as.Date(df$rundate, "%Y-%m-%d")

  # Restrict to baseline data early, to speed up processing.
  df = df[df$clinical_event == "Baseline Collection", ]
  dim(df)

  ################################
  # Reshape

  # Remove unnecessary fields prior to reshaping.
  df = subset(df, select = -c(gender, diagnosis,
                              type, units, projectid,
                              pi_name, pi_institution, update_stamp))

  # Patients are duplicated because there are multiple observations for
  # each subject, which correspond to different tests.

  # Fix capitalization for this test for some units.
  df[df$testname == "APOE GENOTYPE", "testname"] = "ApoE Genotype"

  table(df$testname)

  colnames(df)
  # Give a clearer name to the test value.
  colnames(df)[colnames(df) == "testvalue"] = "biospec"
  colnames(df)

  # Restrict to tests with reasonable coverage.
  #df = df[df$testname %in% c("ApoE GenoType", "CSF Alpha-synuclein",
  #           "CSF Hemoglobin", "EGF ELISA", "GLT25D1", "rs114138760"), ]

  # Partially de-duplicate by selecting the first rundate within
  # the patno-event-testname group.
  if (F) {
  df = df %>% group_by(patno, clinical_event, testname) %>% arrange(rundate) %>%
    filter(row_number() == 1) #%>% ungroup()
  }

  table(df$testname)
  start_df = df

  # testname = name of the test (multiple tests per patno)
  # clinical_event and rundate = identify the date and the patient visit
  df = reshape(df, timevar = c("testname"),
    idvar = c("patno", "clinical_event", "rundate"), direction = "wide")

  dim(df)
  colnames(df)


  colnames(df)

  # Convert from character string to numeric.
  target_cols = 4:ncol(df)
  # TODO: remove certain cols from this list that should become factors.
  # NOTE: this will cause many warnings about NAs!
  df[, target_cols] = sapply(df[, target_cols], as.numeric)

  summary(df)
  dim(df)
  summary(df)

  ################################
  # Ensure only one observation per patient.

  # Restrict to baseline, then keep the earliest record for each patient.
  df = df %>% group_by(patno) %>% arrange(rundate) %>%
    filter(row_number() == 1)

  dim(df)
  summary(df)

  ################################
  # Remove fields that we don't want to keep.

  df = subset(df, select=-c(rundate, clinical_event))

  ################################
  # Return the cleaned result.
  df
}
