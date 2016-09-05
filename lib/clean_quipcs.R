#' @param df The input dataframe from the CSVs.

clean_quipcs = function(df) {

  ################################
  # Manual review.
  if (F) {
    df = files$quip_current_short
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
  # Clean up variables.

  # Clean up two string variables and convert to numeric.

  df[df$tmdismed == "N", "tmdismed"] = ""
  df$tmdismed = as.numeric(df$tmdismed)

  df[df$cntrldsm == "N", "cntrldsm"] = ""
  df$cntrldsm = as.numeric(df$cntrldsm)

  if (F) {
    table(df$tmdismed, useNA="ifany")
    table(df$cntrldsm, useNA="ifany")
  }

  ################################
  # Derived variables.

  df$quip = as.numeric(df$cntrlgmb == 1 | df$tmgamble == 1) +
    as.numeric(df$cntrlsex == 1 | df$tmsex == 1) +
    as.numeric(df$cntrlbuy == 1 | df$tmbuy == 1) +
    as.numeric(df$cntrleat == 1 | df$tmeat == 1) +
    as.numeric(df$tmtoract == 1) + as.numeric(df$tmtmtact == 1) +
    as.numeric(df$tmtrwd == 1)

  ################################
  # Ensure only one observation per patient.

  # Keep BL record for each patient.
  df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(event_id == "BL")

  ################################
  # Remove fields that we don't want to keep.

  df = subset(df, select = -c(rec_id, f_status, event_id, pag_name, infodt,
                              orig_entry, last_update, query, site_aprv))

  ################################
  # Return the cleaned result.
  df
}
