#' Clean up the pd_features table.
#' @param df The input dataframe from the CSVs.

clean_pd_features = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$pd_features
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

  # CK: are we ok about this applying to every column? Just double-checkin
  # LM: For this df, yes
  df[df == "U" | df == "u"] = NA

  # Convert diagnosis date from string to a true date object.
  # variablex only includes year & month, so impute day to the 15th.
  # We need this variable to calculate disease duration in merge-data.rmd
  df$pddxdt = as.Date(paste0(df$pddxdt, "/15"), "%m/%Y/%d")

  # Make variable that counts the days from symptom onset to diagnosis.
  # Again impute days of sympton onset to 15.
  df$days_symptom_to_dx = as.numeric(df$pddxdt -
    as.Date(paste0(df$sxyear, "-", df$sxmo, "-", "15")))

  table(df$dxtremor, useNA="ifany")
  # This variable stores raw text notes of symptoms.
  # Could do some text processing but don't have a huge sample.
  table(df$dxothcm, useNA="ifany")

  # Convert these character variables to numerics. They were characters because
  # the CSVs used "U" as missing data, which has now been converted to an NA.
  to_numeric_cols = c("dxtremor", "dxrigid", "dxbrady", "dxposins","dxothsx",
                      "domside")
  df[, to_numeric_cols] = sapply(df[, to_numeric_cols], FUN=function(x) as.numeric(x))

  ################################
  # Ensure only one observation per patient.

  # Keep the first (earliest) record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(row_number() == 1)

  ################################
  # Remove fields that we don't want to keep.
  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name, infodt,
                              pddxest, dxothsx, dxothcm, sxmo, sxyear,
                              orig_entry, last_update, query, site_aprv))

  ################################
  # Return the cleaned result.
  df
}