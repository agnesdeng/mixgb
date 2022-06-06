## Resubmission
This is a resubmission. In this version I have:
the feedback from CRAN.

* Converted XGBoost to 'XGBoost' in the DESCRIPTION title.

* Created another smaller datasets so that all examples can be executable within 5 seconds.

* Replaced \dontrun{} with \donttest{} for examples > 5 sec.

* Changed cat() to if(verbose) cat(..)

* Checked my functions do not write in the user's home filespace. 
  However, I can't find out which one causes this note. 
  When I ran devtools::check_rhub(), it shows the following:
```
checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
```
It's only found on Windows Server 2022, R-devel, 64 bit but not on others. 
It seems to be a bug in MiKTex as noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503)
  
## Changes according to the feedback from CRAN
> Please also write package names, software names and API (application
programming interface) names in single quotes in the title. e.g: -->
'XGBoost'
Please note that package names are case sensitive.

I converted XGBoost to 'XGBoost' in the DESCRIPTION title.

> Please add a few more small executable examples in your Rd-files to
illustrate the use of the exported function but also enable automatic
testing if possible.

I created another smaller datasets so that all examples can be executable within 5 seconds.

> \dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Does not seem necessary. Please unwrap the examples if they are executable in less than 5 sec,
or replace \dontrun{} with \donttest{}.

I have adjusted examples so that most of them take less than 5 seconds. 
I replaced \dontrun{} with \donttest{} for examples > 5 sec.

> You write information messages to the console that cannot be easily
suppressed. It is more R like to generate objects that can be used to
extract the information a user is interested in, and then print() that
object.
Instead of cat() rather use message()/warning()  or if(verbose)cat(..)
(or maybe stop()) if you really have to write text to the console.
(except for print, summary, interactive functions)

I changed cat() to if(verbose) cat(..).

> Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your
examples/vignettes/tests you can write to tempdir().
Please fix and resubmit.

I checked my functions, they do not seems to write to user's home filespace.
I am not sure why this occurs. Is this related to the following note 
when I ran devtools::check_rhub()?
```
checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
```
It's only found on Windows Server 2022, R-devel, 64 bit but not on others. 
It seems to be a bug in MiKTex as noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503)

## R CMD check results

  Maintainer: ‘Yongshi Deng <yongshi.deng@auckland.ac.nz>’
  
  New submission

0 errors ✔ | 0 warnings ✔ | 1 note ✖
