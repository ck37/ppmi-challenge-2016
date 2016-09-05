#' @param df The input dataframe from the CSVs.
clean_pasels = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$`pase_-_leisure_time_activity`
    dim(df)
    names(df)
    str(df)

    # Reshape version:
    # Not sure if this will work either.
    wide = reshape(df, v.names = "questno", idvar = c("patno", "event_id"), direction = "wide")

    # Clean up prior to reshaping.
    df2 = df %>% arrange(patno, rec_id) %>%
      subset(select=-c(rec_id, f_status, pag_name,# orig_entry,
                       last_update, query, site_aprv, actvspec))

    # Is there duplication at the patient level? Check # of records per patient id.
    dupes = df %>% group_by(patno, event_id) %>% summarize(pat_dupes=n())
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

  # Convert orig_entry from string to a date class.
  df$orig_entry = as.Date(paste0(df$orig_entry, "/15"), "%m/%Y/%d")

  # Clean up prior to reshaping.
  df2 = df %>% arrange(patno, rec_id) %>%
    subset(select=-c(rec_id, f_status, pag_name,# orig_entry,
                     last_update, query, site_aprv, actvspec))

  # Grouping by patno & event_id, convert questno to columns
  # and put actvoft into the cell value.
  wide_df = df2 %>% group_by(patno, event_id) %>%  select(-hrdayfrq) %>%
    tidyr::spread(questno, actvoft)
  # Make better column names.
  colnames(wide_df)[4:9] = paste0("pase_q", 1:6, "_act")

  # Repeat this but now put hrdayfrq into the cell columns.
  wide_df2 = df2 %>% group_by(patno, event_id) %>% select(-actvoft) %>%
    tidyr::spread(questno, hrdayfrq) %>% select(-orig_entry)
  # Update column names - this is hours per day.
  colnames(wide_df2)[3:8] =  paste0("pase_q", 1:6, "_hours")

  # Join df2 results back into the first dataframe.
  wide_df = left_join(wide_df, wide_df2, by = c("patno", "event_id"))

  if (F) {
    head(wide_df)
    str(df2)
    #wide_df = df2 %>% group_by(patno, event_id) %>%
    #  tidyr::spread(questno, actvoft)

  }
  # This reshape version also works:
  #wide_df = reshape2::dcast(df2, patno + event_id ~ questno, value.var = "actvoft", fill = 0)

  ################################
  # Ensure only one observation per patient.

  # Keep the first (earliest) record for each patient.
  # We need to ungroup() and then regroup() without event_id in order
  # To choose the earliest record for each patient.
  wide_df = wide_df %>% ungroup() %>% group_by(patno) %>%
    arrange(orig_entry) %>% filter(row_number() == 1) %>% arrange(patno)
  wide_df

  ################################
  # Remove fields that we don't want to keep.

  wide_df = subset(wide_df, select = -c(event_id, orig_entry))

  ################################
  # Return the cleaned result.
  wide_df
}
