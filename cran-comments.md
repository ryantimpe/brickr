## Test environments
* local Windows 10 install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs or NOTES. 

This is a small update to fix a breaking error introduced with 'tidyr' 1.1.1.

Most examples that produce the mosaics or 3D models take more than 5 seconds to calculate.
Those lines of script have been wrapped in donttest. There are a few cases where I got the examples to run quickly without donttest.

## Downstream dependencies
No downstream dependencies