# A small subset of the NHANES III (1988-1994) newborn data

This dataset is a small subset of `newborn`. It is for demonstration
purposes only. More information on NHANES III data can be found on
<https://wwwn.cdc.gov/Nchs/Data/Nhanes3/7a/doc/mimodels.pdf>

## Usage

``` r
data(nhanes3)
```

## Format

A data frame of 500 rows and 6 variables. Three variables have missing
values.

- age_months:

  Age at interview (screener), in months. An integer variable ranging
  from 2 to 11. The original variable name in the NHANES III dataset is
  `HSAGEIR`.

- sex:

  Sex of the subject. A factor variable with levels `Male` and `Female`.
  The original variable name in the NHANES III dataset is `HSSEX`.

- ethnicity:

  Ethnicity of the subject. A factor variable with levels
  `Mexican-American`, `Other Hispanic`, and `Not Hispanic`. The original
  variable name in the NHANES III dataset is `DMAETHNR`.

- head_circumference_cm:

  Head circumference, in centimetres. Numeric. The original variable
  name in the NHANES III dataset is `BMPHEAD`.

- recumbent_length_cm:

  Recumbent length, in centimetres. Numeric. The original variable name
  in the NHANES III dataset is `BMPRECUM`.

- weight_kg:

  Body weight, in kilograms. Numeric. The original variable name in the
  NHANES III dataset is `BMPWT`.

## Source

<https://wwwn.cdc.gov/nchs/nhanes/nhanes3/datafiles.aspx>

## References

U.S. Department of Health and Human Services (DHHS). National Center for
Health Statistics. Third National Health and Nutrition Examination
Survey (NHANES III, 1988-1994): Multiply Imputed Data Set. CD-ROM,
Series 11, No. 7A. Hyattsville, MD: Centers for Disease Control and
Prevention, 2001. Includes access software: Adobe Systems, Inc. Acrobat
Reader version 4.
