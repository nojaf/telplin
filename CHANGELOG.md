# Changelog

## 0.9.6 - 2024-02-22

### Fixed
* Use `__NonExistentSubDir__` to force design time build.

## 0.9.5 - 2024-01-31

### Fixed
* Very minor tweaks in the `dotnet msbuild` command execution.

## 0.9.4 - 2024-01-29

### Fixed
* `[<Class>]` attribute is missing when constructor is private. [#177](https://github.com/nojaf/telplin/issues/177)
* Use /restore for design time build. [#187](https://github.com/nojaf/telplin/pull/187)

## 0.9.3 - 2023-12-12

### Fixed
* Pass in dummy `/p:Version` during design time build, to avoid target `CoreCompile` being skipped.

## 0.9.2 - 2023-12-01

### Fixed
* Allow project cracking when dotnet sdk (via global.json) is lowered than 8.

## 0.9.1 - 2023-11-22

### Fixed
* Recursively find project references.

## 0.9.0 - 2023-11-22

### Changed
* Update to FCS 43.8.100
* Update tfm to `net8.0`

## 0.8.3 - 2023-06-28

### Fixed
* Private keyword lost in getter. [#87](https://github.com/nojaf/telplin/issues/87)
* Inline keyword is missing. [#90](https://github.com/nojaf/telplin/issues/90)
* Generic type with static member. [#89](https://github.com/nojaf/telplin/issues/89)

## 0.8.2 - 2023-06-14

### Fixed
* Order of generic parameters don't match. [#82](https://github.com/nojaf/telplin/issues/82)

## 0.8.1 - 2023-06-13

### Fixed
* C# Override member generation [#84](https://github.com/nojaf/telplin/issues/84)

## 0.8.0 - 2023-06-12

### Changed
* Exclude private constructors by default. [#70](https://github.com/nojaf/telplin/issues/70)

### Fixed
* Private constructor in struct. [#79](https://github.com/nojaf/telplin/issues/79)
* Generic type parameter doesn't parse internally. [#68](https://github.com/nojaf/telplin/issues/68)
* Optional function type cannot be parsed. [#78](https://github.com/nojaf/telplin/issues/78)
* Property member with function return type. [#66](https://github.com/nojaf/telplin/issues/66)
* Mutable is lost from let binding. [#67](https://github.com/nojaf/telplin/issues/67)
* Non comparable struct should have `[<NoComparison>]` attribute [#80](https://github.com/nojaf/telplin/issues/80)

## 0.7.0 - 2023-06-05

### Changed
* Better error handling and partial signature generation. [#72](https://github.com/nojaf/telplin/issues/72)
* Update FCS to 43.7.400-preview.23302.5

## 0.6.0 - 2023-06-01

### Changed
* Private let bindings are no longer included by default. Use `--include-private-bindings` to include them. [#70](https://github.com/nojaf/telplin/issues/70)

## 0.5.2 - 2023-05-22

### Changed
* Update FCS to 43.7.400-preview.23271.1

### Fixed
* Wrong signature for member getter, setter with extra parameters. [#52](https://github.com/nojaf/telplin/issues/52)
* Setter with different input than return type. [#61](https://github.com/nojaf/telplin/issues/61)
* Types in a recursive module require the and keyword. [#62](https://github.com/nojaf/telplin/issues/62)

## 0.5.1 - 2023-11-05

### Fixed
* static getter/setter should remain static. [#51](https://github.com/nojaf/telplin/issues/51)
* Member with abstract decl and default impl uses "member" instead of "override" for impl signature entry [#53](https://github.com/nojaf/telplin/issues/53)
* Don't add [<Class>] attribute for interface. [#55](https://github.com/nojaf/telplin/issues/55)
* Access modifier from member is missing. [#56](https://github.com/nojaf/telplin/issues/56)
* Duplicate constraints should be avoided. [#57](https://github.com/nojaf/telplin/issues/57)

## 0.5.0 - 2023-05-02

### Added
* Accept additional arguments for building .fsproj file. (Example: `telplin MyProject.fsproj -- -c Release`)

### Fixed
* Take compiler defines into account when parsing the syntax tree.
* Flexible parameter generation issue. [#47](https://github.com/nojaf/telplin/issues/47)

## 0.4.0 - 2023-04-28

### Fixed
* Add support for enums.
* Add support for type delegates.

### Added
* Also generate an `.rsp` file from the input `.fsproj` file using `--record`.
* Only record an `.rsp` file using `--record-only`.

### Changed
* The main input also accepts a `.rsp` file or `.binlog` file next to an `.fsproj`.
* Replace `--write` with `--dry-run`. The behaviour is now inverted. By default files will be written.

## 0.3.2 - 2023-04-28

### Fixed
* Override val not respected. [#38](https://github.com/nojaf/telplin/issues/38)
* Constraint is missing from binding. [#39](https://github.com/nojaf/telplin/issues/39)

## 0.3.1 - 2023-04-25

### Fixed
* Wildcard array should not be used from untyped tree. [#30](https://github.com/nojaf/telplin/issues/30)
* Inline keyword in type extension is not preserved. [#31](https://github.com/nojaf/telplin/issues/31)
* with get,set is lost. [#33](https://github.com/nojaf/telplin/issues/33)
* Generic type argument should be preserved. [#32](https://github.com/nojaf/telplin/issues/32)

## 0.3.0 - 2023-04-12

### Changed
* The console application no longer takes an MSBuild binary log file as input, but a `fsproj`instead.
* Update to Fantomas.Core v6.0.0-beta-001.
* Update to FSharp.Compiler.Service v43.7.300-preview.23167.4

## 0.2.0 - 2022-12-17

### Fixed
* `[<Class>]` should not be added twice. [#1](https://github.com/nojaf/telplin/issues/1)
* Member constraints are not properly processed. [#9](https://github.com/nojaf/telplin/issues/9)

### Changed
* Update to Fantomas.Core v5.1.0-beta-002.

## 0.1.0 - 2022-09-30

### Added

* Initial release
