#' Clean up the screening table.
#' @param screen The screening table as a dataframe.
clean_hemotology = function(df) {

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

    # This is duplicated because there are multiple observations for
    # each subject, which correspond to different tests.

#first, remove fields that we don't want to keep.

    # Keep excluded and declined for verification after the merge.
    df = subset(df, select = -c(rec_id, f_status, event_id, pag_name,
                                orig_entry, last_update, query, site_aprv))

    #now, reshape

    df <- reshape(df, varying = "",timevar = "ltstname", idvar = "patno", direction = "wide")
    #check
    dim(df)



  }


  ################################
  # Ensure only one observation per patient.


  # This subsets to the first row for each patient, which may not be the best
  # row choice. However this ensures that we don't join multiple results for a
  # patient to our main data frame. Ideally we would figure out which row is
  # best for a given patient.
  # df = df %>% dplyr::distinct(patno, .keep_all = T)



  ################################


  ################################
  # Return the cleaned result.
  df
}
