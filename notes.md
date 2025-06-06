- `devtools::document()`: generate docmentation
- `devtools::build_manual(path = "~/Code/ffa-package")`: generate manual
- `devtools::test()`: run unit tests

Generating a coverage report:

```
cov <- covr::package_coverage()
covr::report(cov, file = "coverage.html")
```

