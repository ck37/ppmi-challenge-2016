#' @param df The input dataframe from the CSVs.

clean_general_physical_exam = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$general_physical_exam
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
  # Clean up variables.

  df$infodt = as.Date(paste0(df$infodt, "/15"), "%m/%Y/%d")

  # Take out unneeded fields before we merge.
  df = subset(df, select = -c(f_status, pag_name,  orig_entry,
                              abnormcm,
                              last_update, query, site_aprv))

  df[df$pecat == "Cardiovascular (including peripheral vascular)", "pecat"] = "cardiovasc"
  df[df$pecat == "Other (Specify location and describe)", "pecat"] = "other"
  df$pecat = tolower(df$pecat)
  table(df$pecat, df$peseq)

  df = subset(df, select=-c(peseq, rec_id))

  # name of the test (multiple tests per patno) = pecat
  # clinical_event and rundate = identify the date and the patient visit
  # This will generate some warnings.
  df = reshape(df, timevar = c("pecat"),
               idvar = c("patno", "event_id", "infodt"), direction = "wide")

  ################################
  # Ensure only one observation per patient.

  # Keep the first (earliest) record for each patient.
  df = df %>% group_by(patno) %>% filter(event_id %in% c("BL", "SC")) %>%
    arrange(infodt) %>% filter(row_number() == 1)

  table(df$event_id)

  ################################
  # Remove fields that we don't want to keep.

  df = subset(df, select=-c(event_id, infodt))

  ################################
  # Return the cleaned result.
  df
}
