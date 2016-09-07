#' Clean up the prodromal diagnostic questionnaire table.
#' @param df The input dataframe from the CSVs.

clean_updrs1_both = function(df, df_patient) {

  ################################
  # Manual review.
  if (F) {
    df = files$mds_updrs_part_i
    dim(df)
    names(df)
    str(df)
    df_patient = files$mds_updrs_part_i__patient_questionnaire
    dim(df_patient)
    colnames(df_patient)

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
  # Merge NUPDRS1 and NUPDRS1P

  # First remove some fields we won't want to be duplicated in the merged data.

  df = subset(df, select = -c(f_status, pag_name, orig_entry,
                              last_update, query, site_aprv))

  df_patient = subset(df_patient, select = -c(rec_id, f_status, pag_name,
                          orig_entry, last_update, query, site_aprv))

  head(df)
  data = dplyr::left_join(df, df_patient, by = c("patno", "event_id", "infodt"),
                          suffix = c("_updrs_i", "_updrs_i_pat"))
  dim(data)
  summary(data)

  ################################
  # Clean up important variables.

  colnames(data)

  # Create summary score between the two tables of part 1 (non patient and patient)
  data$updrs_score_p1 = with(data, rowSums(cbind(np1cog, np1hall, np1dprs, np1anxs,
        np1apat, np1dds, np1slpn, np1slpd, np1pain, np1urin, np1cnst, np1lthd,
        np1fatg)))

  summary(data$updrs_score_p1)
  hist(data$updrs_score_p1)


  ##### months of follow up
#  df$infodt = as.Date(paste0(df$infodt, "/15"), "%m/%Y/%d")
#  df = df %>% group_by(patno) %>% max(infodt)


  # Save the latest UPDRS part 1 combined score, as well as the # of UPDRS1
  # records each patient has.
  data = data %>% group_by(patno) %>% arrange(rec_id) %>%
    mutate(final_updrs_score_p1  = last(updrs_score_p1),
           updrs1_records = n())

  ################################
  # Ensure only one observation per patient.

  # Keep the first (earliest) record for each patient.
  data = data %>% group_by(patno) %>% arrange(rec_id) %>% filter(row_number() == 1)

  ################################
  # Remove fields that we don't want to keep.
  data = subset(data, select = -c(event_id, infodt, rec_id))

  ################################
  # Return the cleaned result.
  data
}
