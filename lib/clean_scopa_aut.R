#' Clean up the family history table.
#' @param df The input dataframe from the CSVs.
clean_scopa_aut = function(df) {

  # Run manually:
  if (F) {
    df = files$`scopa-aut`
    dim(df)
    names(df)
    str(df)
    nrow(df)

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
  # Clean up columns if needed.

  # Remove string variables
  df = subset(df, select = -c(scau23at, scau26at, scau26bt, scau26ct, scau26dt))

  ################################
  # Derived variables.

  # Variables where we add 3 if it has a "9"
  add_three = df[, paste0("scau", 1:21)]
  add_three[add_three == 9] = 3

  add_zero = df[, paste0("scau", 22:25)]
  add_zero[add_zero == 9] = 0

  df$scopa_aut_total = rowSums(cbind(add_three, add_zero), na.rm=T)
  hist(df$scopa_aut_total)
  if (F) {
    summary(df$scopa_aut_total)
  }

  ################################
  # Ensure only one observation per patient.

  # Keep the BASELINE record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(event_id %in% "BL")

  ################################
  # Remove fields that we don't want to keep.

  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name, infodt,
                              orig_entry, last_update, query, site_aprv))

  # Return the cleaned result.
  df
}
