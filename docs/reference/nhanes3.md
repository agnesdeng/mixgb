# A small subset of the NHANES III (1988-1994) newborn data

This dataset is a small subset of `nhanes3_newborn`. It is for
demonstration purposes only. More information on NHANES III data can be
found on <https://wwwn.cdc.gov/Nchs/Data/Nhanes3/7a/doc/mimodels.pdf>

## Usage

``` r
data(nhanes3)
```

## Format

A data frame of 500 rows and 6 variables. Three variables have missing
values.

- HSAGEIR:

  Age at interview (screener) - qty (months). An integer variable from 2
  to 11.

- HSSEX:

  Sex. A factor variable with levels 1 (Male) and 2 (Female).

- DMARETHN:

  Race-ethnicity. A factor variable with levels 1 (Non-Hispanic white),
  2 (Non-Hispanic black), 3 (Mexican-American) and 4 (Other).

- BMPHEAD:

  Head circumference (cm). Numeric.

- BMPRECUM:

  Recumbent length (cm). Numeric.

- BMPWT:

  Weight (kg). Numeric.

## Source

<https://wwwn.cdc.gov/nchs/nhanes/nhanes3/datafiles.aspx>

## References

U.S. Department of Health and Human Services (DHHS). National Center for
Health Statistics. Third National Health and Nutrition Examination
Survey (NHANES III, 1988-1994): Multiply Imputed Data Set. CD-ROM,
Series 11, No. 7A. Hyattsville, MD: Centers for Disease Control and
Prevention, 2001. Includes access software: Adobe Systems, Inc. Acrobat
Reader version 4.
