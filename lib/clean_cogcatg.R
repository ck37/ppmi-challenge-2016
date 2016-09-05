#' @param df The input dataframe from the CSVs.

clean_cogcatg = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$cognitive_categorization
    dim(df)
    names(df)
    str(df)
    summary(df)


    #clean date
    df$infodt = as.Date(paste0(df$infodt, "/15"), "%m/%Y/%d")
    class(df$infodt)
    head(df$infodt)
    min(df$infodt)
    max(df$infodt)

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
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(event_id == "BL")

  ################################
  # Remove fields that we don't want to keep.
  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name,
                              infodt, ptcgboth, orig_entry, last_update,
                              query, site_aprv))


  ################################
  # Return the cleaned result.
  df
}
