![stringsort](media/logo.png)
============

[![CI Status](https://github.com/jacobwilliams/stringsort/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/stringsort/actions)
[![GitHub release](https://img.shields.io/github/release/jacobwilliams/stringsort.svg?style=plastic)](https://github.com/jacobwilliams/stringsort/releases/latest)
[![codecov](https://codecov.io/gh/jacobwilliams/stringsort/branch/master/graph/badge.svg?token=43HK33CSMY)](https://codecov.io/gh/jacobwilliams/stringsort)

### Description

Just some Fortran sorting routines for strings.

### Building

Stringsort and the test programs will build with any modern Fortran compiler. A [Fortran Package Manager](https://github.com/fortran-lang/fpm) manifest file (`fpm.toml`) is included, so that the library and tests cases can be compiled with FPM. For example:

```
fpm build --profile release
fpm test --profile release
```

To use `stringsort` within your fpm project, add the following to your `fpm.toml` file:
```toml
[dependencies]
stringsort = { git="https://github.com/jacobwilliams/stringsort.git" }
```

To generate the documentation using [ford](https://github.com/Fortran-FOSS-Programmers/ford), run:

```
  ford ford.md
```

### Documentation

The API documentation for the current ```master``` branch can be found [here](https://jacobwilliams.github.io/stringsort/).  This is generated by processing the source files with [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

### License

This code and related files and documentation are distributed under a permissive free software [license](https://github.com/jacobwilliams/stringsort/blob/master/LICENSE) (BSD-style).

### See also

  * [Natural Sorting](https://degenerateconic.com/natural-sorting.html) [degenerateconic.com]