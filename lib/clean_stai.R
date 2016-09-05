#' @param df The input dataframe from the CSVs.

clean_stai = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$`state-trait_anxiety_inventory`
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
  # Derived variable creation.

  cols = paste0("staiad", 1:40)

  # These columns are normal, per PPMI_Derived_Variables pdf.
  pos_cols = paste0("staiad", c(3, 4, 6, 7, 9, 12, 13, 14, 17, 18, 22, 24, 25, 28, 29, 31, 32, 35, 37, 38, 40))

  # These remainder need reverse scoring:
  # code of 1 = 4
  # code of 2 = 3
  # code of 3 = 2
  # code of 4 = 1
  # Formula to do this: (code - 5) * -1
  neg_cols = setdiff(cols, pos_cols)

  df$stai_total = with(df, rowSums(df[, pos_cols]) +
                         rowSums((df[, neg_cols]) - 5) * -1)
  if (F) {
    cat(pos_cols)
    cat(neg_cols)
    summary(df$stai_total)
    hist(df$stai_total)
  }

  # State subscore - again see PPMI_Derived_Variables pdf.
  # Just uses questions 1-20
  cols = paste0("staiad", 1:20)
  pos_cols = paste0("staiad", c(3, 4, 6, 7, 9, 12, 13, 14, 17, 18))
  neg_cols = setdiff(cols, pos_cols)
  df$stai_state_subscore = with(df, rowSums(df[, pos_cols]) +
                            rowSums((df[, neg_cols] - 5) * -1))

  if (F) {
    hist(df$stai_state_subscore)
    summary(df$stai_state_subscore)
  }

  # Trait subscore - again see PPMI_Derived_Variables pdf.
  # Just uses quesitons 21-40
  cols = paste0("staiad", 21:40)
  pos_cols = paste0("staiad", c(22, 24, 25, 28, 29, 31, 32, 35, 37, 38, 40))
  neg_cols = setdiff(cols, pos_cols)
  df$stai_trait_subscore = with(df, rowSums(df[, pos_cols]) +
                                  rowSums((df[, neg_cols] - 5) * -1))

  if (F) {
    hist(df$stai_trait_subscore)
    summary(df$stai_trait_subscore)
  }

  ################################
  # Ensure only one observation per patient.

  # Keep BL record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(event_id == "BL")

  # This subsets to the first row for each patient, which may not be the best
  # row choice. However this ensures that we don't join multiple results for a
  # patient to our main data frame. Ideally we would figure out which row is
  # best for a given patient.
  # df = df %>% distinct(patno, .keep_all = T)

  ################################
  # Remove fields that we don't want to keep.

  df = subset(df, select = -c(rec_id, f_status, event_id,
                              pag_name, infodt, orig_entry,
                              last_update, query, site_aprv))

  ################################
  # Return the cleaned result.
  df
}
