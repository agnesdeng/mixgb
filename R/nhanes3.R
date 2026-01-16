#' A small subset of the NHANES III (1988-1994) newborn data
#'
#' This dataset is a small subset of \code{newborn}. It is for demonstration purposes only. More information on NHANES III data can be found on \url{https://wwwn.cdc.gov/Nchs/Data/Nhanes3/7a/doc/mimodels.pdf}
#' @docType  data
#' @usage data(nhanes3)
#' @format A data frame of 500 rows and 6 variables. Three variables have missing values.
#' \describe{
#'  \item{age_months}{Age at interview (screener), in months. An integer variable ranging from 2 to 11. The original variable name in the NHANES III dataset is \code{HSAGEIR}.}
#'  \item{sex}{Sex of the subject. A factor variable with levels \code{Male} and \code{Female}. The original variable name in the NHANES III dataset is \code{HSSEX}.}
#'  \item{ethnicity}{Ethnicity of the subject. A factor variable with levels \code{Mexican-American}, \code{Other Hispanic}, and \code{Not Hispanic}. The original variable name in the NHANES III dataset is \code{DMAETHNR}.}
#'  \item{head_circumference_cm}{Head circumference, in centimetres. Numeric. The original variable name in the NHANES III dataset is \code{BMPHEAD}.}
#'  \item{recumbent_length_cm}{Recumbent length, in centimetres. Numeric. The original variable name in the NHANES III dataset is \code{BMPRECUM}.}
#'  \item{weight_kg}{Body weight, in kilograms. Numeric. The original variable name in the NHANES III dataset is \code{BMPWT}.}
#' }

#' @references U.S. Department of Health and Human Services
#' (DHHS).  National Center for Health Statistics.  Third National
#' Health and Nutrition Examination Survey (NHANES III, 1988-1994):
#' Multiply Imputed Data Set. CD-ROM, Series 11, No. 7A.
#' Hyattsville, MD: Centers for Disease Control and Prevention,
#' 2001. Includes access software: Adobe Systems, Inc. Acrobat
#' Reader version 4.
#' @source \url{https://wwwn.cdc.gov/nchs/nhanes/nhanes3/datafiles.aspx}
"nhanes3"
