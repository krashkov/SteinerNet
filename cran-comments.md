## Test environments

* local macOS 10.14.6, R 4.0.2
* travis-ci ubuntu 16.04.6 LTS (devel and release)
* win-builder (devel, release, oldrelease)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

  It is a known [issue](https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time). Resource, which is internally used in R source code, http://worldclockapi.com/ is currently not available.

## Downstream dependencies

No ERRORs or WARNINGs found (no downstream dependencies). Full details at https://github.com/krashkov/SteinerNet/tree/master/revdep
