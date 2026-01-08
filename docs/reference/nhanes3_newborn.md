# NHANES III (1988-1994) newborn data

This dataset is extracted from the NHANES III (1988-1994) for the age
class `Newborn (under 1 year)`. Please note that this example dataset
only contains selected variables and is for demonstration purposes only.

## Usage

``` r
data(nhanes3_newborn)
```

## Format

A data frame of 2107 rows and 16 variables. Nine variables have missing
values.

- HSHSIZER:

  Household size. An integer variable from 1 to 10.

- HSAGEIR:

  Age at interview (screener) - qty (months). An integer variable from 2
  to 11.

- HSSEX:

  Sex. A factor variable with levels 1 (Male) and 2 (Female).

- DMARACER:

  Race. A factor variable with levels 1 (White), 2 (Black) and 3
  (Other).

- DMAETHNR:

  Ethnicity. A factor variable with levels 1 (Mexican-American), 2
  (Other Hispanic) and 3 (Not Hispanic).

- DMARETHN:

  Race-ethnicity. A factor variable with levels 1 (Non-Hispanic white),
  2 (Non-Hispanic black), 3 (Mexican-American) and 4 (Other).

- BMPHEAD:

  Head circumference (cm). Numeric.

- BMPRECUM:

  Recumbent length (cm). Numeric.

- BMPSB1:

  First subscapular skinfold (mm). Numeric.

- BMPSB2:

  Second subscapular skinfold (mm). Numeric.

- BMPTR1:

  First triceps skinfold (mm). Numeric.

- BMPTR2:

  Second triceps skinfold (mm). Numeric.

- BMPWT:

  Weight (kg). Numeric.

- DMPPIR:

  Poverty income ratio. Numeric.

- HFF1:

  Does anyone who lives here smoke cigarettes in the home? A factor
  variable with levels 1 (Yes) and 2 (No).

- HYD1:

  How is the health of subject person in general? An ordinal factor with
  levels 1 (Excellent), 2 (Very good), 3 (Good), 4 (Fair) and 5 (Poor).

## Source

<https://wwwn.cdc.gov/nchs/nhanes/nhanes3/datafiles.aspx>

## References

U.S. Department of Health and Human Services (DHHS). National Center for
Health Statistics. Third National Health and Nutrition Examination
Survey (NHANES III, 1988-1994): Multiply Imputed Data Set. CD-ROM,
Series 11, No. 7A. Hyattsville, MD: Centers for Disease Control and
Prevention, 2001. Includes access software: Adobe Systems, Inc. Acrobat
Reader version 4.
