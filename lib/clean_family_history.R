#' Clean up the family history table.
#' @param df The input dataframe from the CSVs.
clean_family_history = function(df) {

  # Run manually:
  if (F) {
    df = files$family_history__pd_
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

  # Change number of fam members with PD to indicator variable

  ################################
  # Derived variables.

  df$any_fam_hist_pd = apply(subset(df,
    select=c(biomompd, biodadpd, fulsibpd, hafsibpd, magparpd, pagparpd, mataupd, pataupd, kidspd)),
    MARGIN=1,
    FUN = function(row) { max(as.numeric(row == 1), na.rm=T) }
  )

  if (F) {
    table(df$any_fam_hist_pd, useNA="ifany")
    summary(df$any_fam_hist_pd)
    hist(df$any_fam_hist_pd)
  }

  ################################
  # Ensure only one observation per patient.

  # Keep the first (earliest) record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(event_id == "SC")

  ################################
  # Remove fields that we don't want to keep.

  df$biomompd = df$biomompd/df$biomom
  df$biomompd[is.nan(df$biomompd)] = 0
  df$biodadpd = df$biodadpd/df$biodad
  df$biodadpd[is.nan(df$biodadpd)] = 0
  df$fulsibpd = df$fulsibpd/df$fulsib
  df$fulsibpd[is.nan(df$fulsibpd)] = 0
  df$hafsibpd = df$hafsibpd/df$hafsib
  df$hafsibpd[is.nan(df$hafsibpd)] = 0
  df$magparpd = df$magparpd/df$magpar
  df$magparpd[is.nan(df$magparpd)] = 0
  df$pagparpd = df$pagparpd/df$pagpar
  df$pagparpd[is.nan(df$pagparpd)] = 0
  df$mataupd = df$mataupd/df$matau
  df$mataupd[is.nan(df$mataupd)] = 0
  df$pataupd = df$pataupd/df$patau
  df$pataupd[is.nan(df$pataupd)] = 0
  df$kidsnumpd = df$kidspd/df$kidsnum
  df$kidsnumpd[is.nan(df$kidsnum)] = 0
  
  #It wouldn't let me apply is.nan to every column because it's a list :(
  
  
  df = subset(df, select = -c(biomom, biodad, fulsib, hafsibpd, magpar, pagpar,
                              matau, patau, kidsnum, rec_id, f_status, event_id, pag_name, infodt,
                              orig_entry, last_update, query, site_aprv))
  
  # Return the cleaned result.
  df
}
