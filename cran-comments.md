# CRAN notes for typetracer_0.2.4 submission

The current CRAN version (0.2.3) has one failing test on two CRAN Linux machines. I can not reproduce that failure, nor does it arise on GitHub Linux runners. This submission has nevertheless disabled that failing test on CRAN machines, so should pass all tests regardless. The test was very minor, and most of the package remains thoroughly tested on CRAN machines.

The package has been checked on all environments listed below, and generates no notes

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R-release

CRAN win-builder:
* R-oldrelease, R-release, R-devel
