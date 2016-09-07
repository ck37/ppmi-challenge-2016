#' @param df The input dataframe from the CSVs.

clean_genmedical_hx = function(df) {

  # Run manually:
  if (F) {
    df = files$general_medical_history
    dim(df)
    names(df)
    str(df)
    table(df$event_id)

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

  df$infodt = as.Date(paste0(df$infodt, "/15"), "%m/%Y/%d")

  # De-duplicate.

  ################################
  # Remove fields that we don't want to keep.


  df = data.frame(patno = df$patno, year_of_dx = df$mhdiagyr, mhcat = df$mhcat)

  if (F) {
    df = subset(df, select = c(patno, mhdiagyr, mhcat))
  }


  ################################
  # Reshape data

  # patient remains the unique observation identifier.
  # This is ok because event_id is either BL or SC.
  # health category goes into a column
  # Diagnosis year goes into the cell.
  df = reshape(df, timevar = "mhcat", idvar = "patno", direction = "wide")

  #df = df %>% group_by(patno, mhcat) %>% arrange(infodt) %>%
  #  filter(row_number() == 1) %>% ungroup()


  # Keep the first (earliest) record for each patient.

  # Return the cleaned result.
  df
}
