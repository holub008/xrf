# Contributing
All forms of contribution to xrf are greatly appreciated:

* feature additions
* bug fixes
* optimizations
* documentation
* vignettes
* anything else you can think of

Your interest in the project fuels mine, so please do not hesitate to reach out with proposals. Please propose your changes via:

* Filing a [github issue](https://github.com/holub008/xrf/issues)
* Emailing me (karljholub at gmail.com)

## Process
Ideally, changes are made according to the following process:

* Fork the repository
* Make your changes
* Ensure that xrf tests succeed (run via `devtools::test()`)
* Submit a pull request (this can be done via github UI)
    * Maintainers will provide a code review. Every substantive comment must be addressed before the PR is accepted.
        * Any follow-on commits to the fork will be reflected in the PR
* Please bump version numbers (`major.minor.patch`) in `DESCRIPTION` according to the final change made
    * major number for any substantial API or backwards incompatible changes
    * minor number for any standard change not touching API or compatiility
    * patch number for any bug fixes

### Code style suggestions
No strict style at current, but please attempt to follow suit with the rest of the project. If in doubt, defer to [Wickham](http://r-pkgs.had.co.nz/r.html#style).

### Help with R package development
If you're new to R package development but want to develop on xrf, both of the following are great resources:

* [Wickham's R packages book](http://r-pkgs.had.co.nz/)
* The [devtools](https://cran.r-project.org/web/packages/devtools/devtools.pdf) package

Also feel free to open a correspondence with me (karljholub at gmail.com).

## Thank you!
