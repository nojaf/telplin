# Changelog

## [Unreleased]

### Fixed

* Flexible parameter generation issue. [#47](https://github.com/nojaf/telplin/issues/47)

## 0.4.0 - 28-04-23

### Fixed
* Add support for enums.
* Add support for type delegates.

### Added
* Also generate an `.rsp` file from the input `.fsproj` file using `--record`.
* Only record an `.rsp` file using `--record-only`.

### Changed
* The main input also accepts a `.rsp` file or `.binlog` file next to an `.fsproj`.
* Replace `--write` with `--dry-run`. The behaviour is now inverted. By default files will be written.

## 0.3.2 - 28-04-23

### Fixed
* Override val not respected. [#38](https://github.com/nojaf/telplin/issues/38)
* Constraint is missing from binding. [#39](https://github.com/nojaf/telplin/issues/39)

## 0.3.1 - 25-04-23

### Fixed
* Wildcard array should not be used from untyped tree. [#30](https://github.com/nojaf/telplin/issues/30)
* Inline keyword in type extension is not preserved. [#31](https://github.com/nojaf/telplin/issues/31)
* with get,set is lost. [#33](https://github.com/nojaf/telplin/issues/33)
* Generic type argument should be preserved. [#32](https://github.com/nojaf/telplin/issues/32)

## 0.3.0 - 12-04-23

### Changed
* The console application no longer takes an MSBuild binary log file as input, but a `fsproj`instead.
* Update to Fantomas.Core v6.0.0-beta-001.
* Update to FSharp.Compiler.Service v43.7.300-preview.23167.4

## 0.2.0 - 17-12-22

### Fixed
* `[<Class>]` should not be added twice. [#1](https://github.com/nojaf/telplin/issues/1)
* Member constraints are not properly processed. [#9](https://github.com/nojaf/telplin/issues/9)

### Changed
* Update to Fantomas.Core v5.1.0-beta-002.

## 0.1.0 - 30-09-22

### Added

* Initial release
