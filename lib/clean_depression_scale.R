#' Clean up the geriatric depression scale table.
#' @param df The input dataframe from the CSVs.
clean_depressionscale = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$geriatric_depression_scale__short_
    dim(df)
    names(df)
    str(df)

    ################################
    # Ensure only one observation per patient.
    # Is there duplication at the patient level? Check # of records per patient id.
    dupes = df %>% group_by(patno) %>% summarize(pat_dupes=n())
    # Review duplication by patient (grouped):
    table(dupes$pat_dupes)
    # Unique patients:
    cat("Unique patients:", nrow(dupes),
        "Duplication %",  round((1 - nrow(dupes) / nrow(df))*100, 2), "\n")
  }

  ################################
  # Clean up important variables.

  ################################
  # Derived variables.

  df$gds_score = with(df,
    # Add 1 point for each response of “No” (0) to any of the following variables
    rowSums(cbind(gdssatis, gdsgspir, gdshappy, gdsalive, gdsenrgy) == 0, na.rm=T) +
    # Add 1 point for each response of “Yes” (1) to any of the following variables:
    rowSums(cbind(gdsdropd, gdsempty, gdsbored, gdsafrad, gdshlpls, gdshome, gdsmemry, gdswrtls, gdshopls, gdsbeter) == 1, na.rm=T)
  )
  hist(df$gds_score)

  if (F) {
    summary(df$gds_score)
  }

  ################################
  # Ensure only one observation per patient.

  # Keep BL record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(event_id == "BL")

  ################################
  # Remove fields that we don't want to keep.
  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name,
                              infodt,
                              orig_entry, last_update, query, site_aprv))


  ################################
  # Return the cleaned result.
  df
}
