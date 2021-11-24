
![Build Status](https://github.com/jacobwilliams/stringsort/actions/workflows/CI.yml/badge.svg)

### Description

Just some Fortran sorting routines for strings.

### Building

Stringsort and the test programs will build with any modern Fortran compiler. A [Fortran Package Manager](https://github.com/fortran-lang/fpm) manifest file (`fmp.toml`) is included, so that the library and tests cases can be compiled with FPM. For example:

```
fpm build --profile release
fpm test --profile release
```

To generate the documentation using [ford](https://github.com/Fortran-FOSS-Programmers/ford), run:

```
  ford stringsort.md
```

**Note: Stringsort builds and runs fine with gfortran 7, but seems not to work with some later versions. Everything works fine with the Intel Fortran compiler**


### Documentation

 * The API documentation for the current ```master``` branch can be found [here](https://jacobwilliams.github.io/stringsort/).  This is generated by processing the source files with [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

### License

This code and related files and documentation are distributed under a permissive free software [license](https://github.com/jacobwilliams/stringsort/blob/master/LICENSE) (BSD-style).

### See also

  * [Natural Sorting](https://degenerateconic.com/natural-sorting.html) [degenerateconic.com]