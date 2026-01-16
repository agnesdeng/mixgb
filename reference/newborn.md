# NHANES III (1988-1994) newborn data

This dataset is extracted from the NHANES III (1988-1994) for the age
class `Newborn (under 1 year)`. Please note that this example dataset
only contains selected variables and is for demonstration purposes only.

## Usage

``` r
data(newborn)
```

## Format

A data frame of 2107 rows and 16 variables, adapted from the NHANES III
dataset. Nine variables contain missing values. Variable names and
factor levels have been renamed for clarity and easier interpretation.

- household_size:

  Household size. An integer variable ranging from 1 to 10. The original
  variable name in the NHANES III dataset is `HSHSIZER`.

- age_months:

  Age at interview (screener), in months. An integer variable ranging
  from 2 to 11. The original variable name in the NHANES III dataset is
  `HSAGEIR`.

- sex:

  Sex of the subject. A factor variable with levels `Male` and `Female`.
  The original variable name in the NHANES III dataset is `HSSEX`.

- race:

  Race of the subject. A factor variable with levels `White`, `Black`,
  and `Other`. The original variable name in the NHANES III dataset is
  `DMARACER`.

- ethnicity:

  Ethnicity of the subject. A factor variable with levels
  `Mexican-American`, `Other Hispanic`, and `Not Hispanic`. The original
  variable name in the NHANES III dataset is `DMAETHNR`.

- race_ethinicity:

  Combined race–ethnicity classification. A factor variable with levels
  `Non-Hispanic White`, `Non-Hispanic Black`, `Mexican-American`, and
  `Other`. The original variable name in the NHANES III dataset is
  `DMARETHN`.

- head_circumference_cm:

  Head circumference, in centimetres. Numeric. The original variable
  name in the NHANES III dataset is `BMPHEAD`.

- recumbent_length_cm:

  Recumbent length, in centimetres. Numeric. The original variable name
  in the NHANES III dataset is `BMPRECUM`.

- first_subscapular_skinfold_mm:

  First subscapular skinfold thickness, in millimetres. Numeric. The
  original variable name in the NHANES III dataset is `BMPSB1`.

- second_subscapular_skinfold_mm:

  Second subscapular skinfold thickness, in millimetres. Numeric. The
  original variable name in the NHANES III dataset is `BMPSB2`.

- first_triceps_skinfold_mm:

  First triceps skinfold thickness, in millimetres. Numeric. The
  original variable name in the NHANES III dataset is `BMPTR1`.

- second_triceps_skinfold_mm:

  Second triceps skinfold thickness, in millimetres. Numeric. The
  original variable name in the NHANES III dataset is `BMPTR2`.

- weight_kg:

  Body weight, in kilograms. Numeric. The original variable name in the
  NHANES III dataset is `BMPWT`.

- poverty_income_ratio:

  Poverty income ratio. Numeric. The original variable name in the
  NHANES III dataset is `DMPPIR`.

- smoke:

  Whether anyone living in the household smokes cigarettes inside the
  home. A factor variable with levels `Yes` and `No`. The original
  variable name in the NHANES III dataset is `HFF1`.

- health:

  General health status of the subject. An ordered factor with levels
  `Excellent`, `Very Good`, `Good`, `Fair`, and `Poor`. The original
  variable name in the NHANES III dataset is `HYD1`.

## Source

<https://wwwn.cdc.gov/nchs/nhanes/nhanes3/datafiles.aspx>

## References

U.S. Department of Health and Human Services (DHHS). National Center for
Health Statistics. Third National Health and Nutrition Examination
Survey (NHANES III, 1988-1994): Multiply Imputed Data Set. CD-ROM,
Series 11, No. 7A. Hyattsville, MD: Centers for Disease Control and
Prevention, 2001. Includes access software: Adobe Systems, Inc. Acrobat
Reader version 4.
