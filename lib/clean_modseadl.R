#' @param df The input dataframe from the CSVs.

clean_modseadl = function(df) {
  
  ################################
  # Manual review.
  if (F) {
    df = files$`modified_schwab_+_england_adl`
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
  # Ensure only one observation per patient.
  
  # Keep the first (earliest) record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(row_number() == 1)
  
  # This subsets to the first row for each patient, which may not be the best
  # row choice. However this ensures that we don't join multiple results for a
  # patient to our main data frame. Ideally we would figure out which row is
  # best for a given patient.
  # df = df %>% distinct(patno, .keep_all = T)
  
  ################################
  # Remove fields that we don't want to keep.
  df = subset(df, select = c(patno, mseadlg))
  
  ################################
  # Return the cleaned result.
  df
}