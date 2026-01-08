# Run this once to publish your site regularly
#usethis::create_github_token()
#usethis::use_pkgdown_github_pages()

devtools::load_all()
devtools::document()
# Run once to configure your package to use and deploy pkgdown

pkgdown::build_site()
pkgdown::deploy_to_branch(branch = "gh-pages")

devtools::spell_check()
urlchecker::url_check()
devtools::build_manual()
devtools::build_readme()

devtools::check()
devtools::check_rhub()
devtools::check_win_devel(email = "agnes.yongshideng@gmail.com")
usethis::use_github_action()

devtools::release()

usethis::use_pipe()

citation("mixgb")
usethis::use_citation()
devtools::build_manual()

devtools::check()
devtools::check(manual = TRUE)
devtools::check_win_devel(email = "agnes.yongshideng@gmail.com")
pkgdown::build_site()


#R CMD check mixgb_1.5.2.tar.gz
#R CMD check --as-cran mixgb_1.5.2.tar.gz

devtools::build()
devtools::release()
devtools::spell_check()

#usethis::use_test()
devtools::test()



#devtools::check_rhub()
rhub::rhub_setup()
rhub::rhub_doctor()
usethis::create_github_token()

rhub::rhub_doctor()
rhub::rhub_check()

rhub::check_for_cran()


usethis::use_github_action("check-standard")
usethis::use_github_action("check-release")

