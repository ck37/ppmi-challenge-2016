#' Clean up the screening table.
#' @param screen The screening table as a dataframe.
clean_hematology = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$blood_chemistry___hematology
    dim(df)
    colnames(df)
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
  }

  # Fix date
  df$lrecdt = as.Date(paste0(df$lrecdt, "/15"), "%m/%Y/%d")

  if (F) {
    min(df$lrecdt)
    max(df$lrecdt)
  }

  # Remove fields that we don't want to keep, esp. before reshaping.
  names(df)
  df = subset(df, select = -c( colltm, lgroup, ltstcode,
                               lvistype, lcolldt, rpttm, pag_name, labcode,
                               # Units are constant for each test
                               lsiunit, lusunit,
                               # Don't need the reference ranges:
                               lsilorng, lsihirng, luslorng, lushirng,
                               # Don't need the US unit resull because we have the SI ones.
                               lusres,
                               ltstcomm, lrptdt, rectm))

  names(df)

  #############################
  # Reshape

  # Patients are duplicated because there are multiple observations for
  # each subject, which correspond to different tests.

  # ltstname = name of the test (multiple tests per patno)
  # event_id and lrecdt = identify the date and the event (whether first or second test,
  # etc.)
  df <- reshape(df, timevar = c("ltstname"), idvar = c("patno", "event_id", "lrecdt"),
                direction = "wide")
  # NOTE: this currently generates ~36 warnings about "multiple rows match".
  if (F) {
    warnings()

    # Check results
    dim(df)
    str(df)
    #View(df)
  }

  # Convert from character string to numeric.
  target_cols = 4:ncol(df)
  # TODO: remove certain cols from this list that should become factors.
  # NOTE: this will cause many warnings about NAs!
  df[, target_cols] = sapply(df[, target_cols], as.numeric)
  summary(df)

  ################################
  # Ensure only one observation per patient.

  # Keep the first (earliest) record for each patient.
  df = df %>% group_by(patno) %>% arrange(lrecdt) %>% filter(row_number() == 1)

  ################################


    # Final variable removal.

  df = df %>% select(-event_id, -lrecdt)

  ################################
  # Return the cleaned result.
  df
}
