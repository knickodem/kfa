# kfa 0.2.1

## Test environments

* local windows 10, R 4.2.0: 0 errors | 0 warnings  | 0 notes
* Ubuntu Linux 20.04.1 LTS, R-release, GCC: Success
* Fedora Linux, R-devel, clang, gfortran: Success
* Windows Server 2022, R-devel, 64 bit:  0 errors | 0 warnings  | 2 notes

Notes are:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kyle Nickodem <kyle.nickodem@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1037/1082-989X.1.2.130
    From: man/find_k.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.1037/1082-989X.1.2.130
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
  DOI: 10.1037/cbs0000069
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

* I have checked the DOIs on Google Chrome and Microsoft Edge browsers and they are accurate.
* I have no clue what file the second note refers to, nor can I find it in the package


## Downstream dependencies

There are currently no downstream dependencies for this package.