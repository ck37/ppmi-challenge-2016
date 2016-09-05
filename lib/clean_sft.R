#' @param df The input dataframe from the CSVs.

clean_sft = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$semantic_fluency
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
  # Derived variables.
  df$sft_semantic_total = rowSums(df[, c("vltanim", "vltveg", "vltfruit")], na.rm=T)
  hist(df$sft_semantic_total)

  if (F) {
    summary(df$sft_semantic_total)
  }

  ################################
  # Ensure only one observation per patient.

  # Keep BL obs
  df = df[df$event_id == "BL",]

  ################################
  # Remove fields that we don't want to keep.

  # Too restrictive:
  # df = subset(df, select = c(patno, dvt_sftanim))

  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name,
                              infodt,
                              orig_entry, last_update, query, site_aprv))


  ################################
  # Return the cleaned result.
  df
}
