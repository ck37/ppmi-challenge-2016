#' @param df The input dataframe from the CSVs.
clean_updrs3 = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$mds_updrs_part_iii__post_dose_
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
  # Clean up important variables.

  #create summary score

  df$updrs_score_p3 = with(df, rowSums(cbind(np3spch, np3facxp, np3rign, np3rigru, np3riglu, pn3rigrl,
                np3rigll, np3ftapr, np3ftapl, np3hmovr, np3hmovl, np3prspr,
                np3prspl, np3ttapr, np3ttapl, np3lgagr, np3lgagl, np3risng,
                np3gait, np3frzgt, np3pstbl, np3postr, np3brady, np3ptrmr, np3ptrml,
                np3ktrmr, np3ktrml, np3rtaru, np3rtalu, np3rtarl, np3rtall, np3rtalj,
                np3rtcon)))

  summary(df$updrs_score_p3)
  hist(df$updrs_score_p3)

  # Save the latest UPDRS part 3 combined score, as well as the # of UPDRS3
  # records each patient has.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>%
    mutate(final_updrs_score_p3  = last(updrs_score_p3),
           updrs3_records = n())

  ################################
  # Ensure only one observation per patient.

  # Keep the first (earliest) record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(row_number() == 1)

  ################################
  # Remove fields that we don't want to keep.
  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name, infodt,
                              cmedtm, examtm, annual_time_btw_dose_nupdrs,
                              on_off_dose, pd_med_use, orig_entry, last_update, query, site_aprv))


  ################################
  # Return the cleaned result.
  df
}