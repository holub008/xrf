## Submission
This is a patch of `xrf` from 0.2.0 to 0.2.1. Changes:

* Cleared xgboost unused parameters warning
* Replaced use of deprecated xgboost objective function

## Test environments

* macOS Catalina, R-release
* Debian Linux, R-devel (https://builder.r-hub.io/status/xrf_0.2.1.tar.gz-9af3f255b2c706689b5ce8a9df3eea24)
* Windows, R-release (https://win-builder.r-project.org/94M9t7445SVe/00check.log)

## R CMD check results
There were no ERRORs or WARNINGs on all platforms.

win-builder contained NOTEs on example code runtime & an HTTP 500 in DOI lookup, which appear environment-specific.

## Reverse Dependencies
Were tested on macOS Catalina using `tools::check_packages_in_dir`; no ERRORs or WARNINGs were found.
