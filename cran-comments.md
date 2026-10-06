# CRAN submission comments

## Test environments

- local macOS, R 4.5.1
- (add the win-builder / R-hub results here before submitting)

## R CMD check results

0 errors | 0 warnings | n notes

### Note: possibly invalid URL `https://api.togoid.dbcls.jp/`

This is a false positive. The URL is the base of the TogoID REST API, which has
no resource at its root path and therefore answers 404 by design; every endpoint
the package actually calls answers 200:

```
https://api.togoid.dbcls.jp/                              404  (no root resource)
https://api.togoid.dbcls.jp/convert?route=...&ids=...     200
https://api.togoid.dbcls.jp/config/dataset                200
https://api.togoid.dbcls.jp/config/relation               200
```

The human-readable documentation for the same service is at
<https://togoid.dbcls.jp/apidoc> (200).

### Note: new submission

This is a first submission.

## Internet resources

The package is a client for the TogoID web API, so per the CRAN Repository
Policy on internet resources:

- Examples that contact the API are wrapped in `\dontrun{}` and are not run
  during checks.
- Tests that contact the API call `skip_on_cran()` and `skip_if_offline()`, so
  they are skipped on CRAN and when the host is offline.
- The vignette does not evaluate its chunks, so building it makes no network
  calls.

Failures of the remote service therefore cannot turn into check errors or
warnings.
