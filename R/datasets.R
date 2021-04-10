#' breast
#'
#' For the data example we used data from a study by Dikmans et al (2017).
#' This data is based on photographs of breasts of 50 women after breast
#' reconstruction. The photographs are  independently scored by 5 surgeons,
#' the patient, and three mothers. They each rated the quality of the
#' reconstruction on a 5 point ordinal scale with the verbal anchors on the
#' left side ‘very dissatisfied’ on the left end and on the right end ‘very
#' satisfied’ on the right end. They specifically rated the volume, shape,
#' symmetry, scars and nipple.
#'
#' @format A data frame with 50 rows and 55 variables:
#' \describe{
#'   \item{Qnumber}{id number}
#'   \item{Patient_volume}{Rate for volume of breast on 5pt likert by patient}
#'   \item{Patient_shape}{Rate for shape of breast on 5pt likert by patient}
#'   \item{Patient_symmetry}{Rate for symmetry of breast on 5pt likert by patient}
#'   \item{Patient_scars}{Rate for scars of breast on 5pt likert by patient}
#'   \item{Patient_nipple}{Rate for nipple of breast on 5pt likert by patient}
#'   \item{Patient_score}{Mean score breast satisfaction by patient}
#'   \item{PCH1_volume}{Rate for volume of breast on 5pt likert by surgeon1}
#'   \item{PCH1_shape}{Rate for shape of breast on 5pt likert by surgeon1}
#'   \item{PCH1_symmetry}{Rate for symmetry of breast on 5pt likert by surgeon1}
#'   \item{PCH1_scars}{Rate for scars of breast on 5pt likert by surgeon1}
#'   \item{PCH1_nipple}{Rate for nipple of breast on 5pt likert by surgeon1}
#'   \item{PCH1_score}{Mean score breast satisfaction by surgeon1}
#'   \item{PCH2_volume}{Rate for volume of breast on 5pt likert by surgeon2}
#'   \item{PCH2_shape}{Rate for shape of breast on 5pt likert by surgeon2}
#'   \item{PCH2_symmetry}{Rate for symmetry of breast on 5pt likert by surgeon2}
#'   \item{PCH2_scars}{Rate for scars of breast on 5pt likert by surgeon2}
#'   \item{PCH2_nipple}{Rate for nipple of breast on 5pt likert by surgeon2}
#'   \item{PCH2_score}{Mean score breast satisfaction by surgeon2}
#'   \item{PCH3_volume}{Rate for volume of breast on 5pt likert by surgeon3}
#'   \item{PCH3_shape}{Rate for shape of breast on 5pt likert by surgeon3}
#'   \item{PCH3_symmetry}{Rate for symmetry of breast on 5pt likert by surgeon3}
#'   \item{PCH3_scars}{Rate for scars of breast on 5pt likert by surgeon3}
#'   \item{PCH3_nipple}{Rate for nipple of breast on 5pt likert by surgeon3}
#'   \item{PCH3_score}{Mean score breast satisfaction by surgeon3}
#'   \item{PCH4_volume}{Rate for volume of breast on 5pt likert by surgeon4}
#'   \item{PCH4_shape}{Rate for shape of breast on 5pt likert by surgeon4}
#'   \item{PCH4_symmetry}{Rate for symmetry of breast on 5pt likert by surgeon4}
#'   \item{PCH4_scars}{Rate for scars of breast on 5pt likert by surgeon4}
#'   \item{PCH4_nipple}{Rate for nipple of breast on 5pt likert by surgeon4}
#'   \item{PCH4_score}{Mean score breast satisfaction by surgeon4}
#'   \item{PCH5_volume}{Rate for volume of breast on 5pt likert by surgeon5}
#'   \item{PCH5_shape}{Rate for shape of breast on 5pt likert by surgeon5}
#'   \item{PCH5_symmetry}{Rate for symmetry of breast on 5pt likert by surgeon5}
#'   \item{PCH5_scars}{Rate for scars of breast on 5pt likert by surgeon5}
#'   \item{PCH5_nipple}{Rate for nipple of breast on 5pt likert by surgeon5}
#'   \item{PCH5_score}{Mean score breast satisfaction by surgeon5}
#'   \item{Mam1_volume}{Rate for volume of breast on 5pt likert by mother1}
#'   \item{Mam1_shape}{Rate for shape of breast on 5pt likert by mother1}
#'   \item{Mam1_symmetry}{Rate for symmetry of breast on 5pt likert by mother1}
#'   \item{Mam1_scars}{Rate for scars of breast on 5pt likert by mother1}
#'   \item{Mam1_nipple}{Rate for nipple of breast on 5pt likert by mother1}
#'   \item{Mam1_score}{Mean score breast satisfaction by mother1}
#'   \item{Mam2_volume}{Rate for volume of breast on 5pt likert by mother2}
#'   \item{Mam2_shape}{Rate for shape of breast on 5pt likert by mother2}
#'   \item{Mam2_symmetry}{Rate for symmetry of breast on 5pt likert by mother2}
#'   \item{Mam2_scars}{Rate for scars of breast on 5pt likert by mother2}
#'   \item{Mam2_nipple}{Rate for nipple of breast on 5pt likert by mother2}
#'   \item{Mam2_score}{Mean score breast satisfaction by mother2}
#'   \item{Mam3_volume}{Rate for volume of breast on 5pt likert by mother3}
#'   \item{Mam3_shape}{Rate for shape of breast on 5pt likert by mother3}
#'   \item{Mam3_symmetry}{Rate for symmetry of breast on 5pt likert by mother3}
#'   \item{Mam3_scars}{Rate for scars of breast on 5pt likert by mother3}
#'   \item{Mam3_nipple}{Rate for nipple of breast on 5pt likert by mother3}
#'   \item{Mam3_score}{Mean score breast satisfaction by mother3}
#'   }
#'   @source data-raw/R/breast.R
"breast"

#' diagnoses
#'
#' For the nominal data example we use a data set that was used in a paper by
#' Fleis (1971). In this data patients are diagnosed in 5 categories:
#' Depression, Personality Disorder, Schizophrenia, Neurosis, and Other by six
#' raters.
.
#'
#' @format A data frame with 30 rows and 6 variables:
#' \describe{
#'   \item{rater5}{diagnoses of rater 5}
#'   \item{rater1}{diagnoses of rater 1}
#'   \item{rater3}{diagnoses of rater 3}
#'   \item{rater2}{diagnoses of rater 2}
#'   \item{rater6}{diagnoses of rater 6}
#'   \item{rater4}{diagnoses of rater 4}
#' }
#' @source data-raw/R/diagnoses.R
"diagnoses"
