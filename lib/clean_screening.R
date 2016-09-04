#' Clean up the screening table.
#' @param screen The screening table as a dataframe.
clean_screening = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$screening___demographics
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
    # Review records with duplication, looking at key administrative fields.
    subset(dupes, pat_dupes > 1, c(patno, pat_dupes, event_id, f_status,
                                   orig_entry, last_update, site_aprv))

    # Visually review full data that has duplication.
    View(subset(dupes, pat_dupes > 1))

    #############
    # More review

    table(df$f_status)

    # All are "CONSENT", so we can remove this variable.
    table(df$event_id, useNA="ifany")

    # All are "SCREEN", so we can remove this variable.
    table(df$pag_name, useNA="ifany")

    # Birthdate is just the year in this table, so use the birthdate in the
    # randomization table instead.
    class(df$birthdt)

    # Unclear if we should keep this field.
    table(df$query, useNA="ifany")

    # All are 1, so we can remove this field.
    table(df$signcnst, useNA="ifany")

    # Is there duplication at the patient level? Check # of records per patient id.
    review_duplicates = df %>% group_by(patno) %>% summarize(pat_dupes=n())
    # Duplication: none
    table(review_duplicates$pat_dupes)

  }

  ################################
  # Clean up columns if needed.


  ################################
  # Ensure only one observation per patient.


  # This subsets to the first row for each patient, which may not be the best
  # row choice. However this ensures that we don't join multiple results for a
  # patient to our main data frame. Ideally we would figure out which row is
  # best for a given patient.
  # df = df %>% dplyr::distinct(patno, .keep_all = T)

  if (F) {
    # Possible new version:
    # 1. Group at the patient level.
    # 2. Sort in descending by tie-breaking fields within the patient.
    # 3. Keep the first observation.
    # df = df %>% group_by(patno) %>% arrange(desc()) %>% filter(row_number() == 1)
  }

  ################################
  # Remove fields that we don't want to keep.

  # Keep excluded and declined for verification after the merge.
  # Remove gender because that's in the randomization table.
  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name,
                              orig_entry, last_update, query, site_aprv,
                              birthdt, gender, signcnst))


  ################################
  # Return the cleaned result.
  df
}