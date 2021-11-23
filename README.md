
### Description

Just some sorting routines for strings.

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

### License

This code and related files and documentation are distributed under a permissive free software [license](https://github.com/jacobwilliams/stringsort/blob/master/LICENSE) (BSD-style).
