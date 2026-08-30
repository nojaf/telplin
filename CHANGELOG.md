# Changelog

## [0.16.1] - 2026-08-30

### Changed
* A nested module with nothing left in its signature, and a `private` nested module, are left out of the signature file. `--include-private-bindings` keeps the private module. [#376](https://github.com/nojaf/telplin/issues/376)

### Fixed
* The verify step no longer fails on a binding the new signature hides when the project promotes the unused-value warning (FS1182) to an error, as Fantomas does with `<WarningsAsErrors>FS1182</WarningsAsErrors>`. The code compiled before the signature was added, so that warning stays a warning for the check.

## [0.16.0] - 2026-08-30

### Added
* The XML doc comments of the declarations the signature has are removed from the implementation file, so the docs live in the signature only. `--keep-xml-docs` opts out. [#374](https://github.com/nojaf/telplin/pull/374)

### Fixed
* A file that already has a signature file no longer loses every let binding: uses of its bindings were attributed to the signature and so counted as unused. [#374](https://github.com/nojaf/telplin/pull/374)

## [0.15.0] - 2026-08-30

### Changed
* An exception while converting one declaration no longer stops the whole run: the declaration is left out and the rest of the file is still produced. The CLI reports every declaration it left out as `file(line,col): error: message`, followed by the source around it with the declaration underlined. Progress lines and warnings go to stderr, so stdout holds only results. [#72](https://github.com/nojaf/telplin/issues/72)

### Fixed
* A member with a `[<DefaultParameterValue>]` whose value is the default of a struct, such as `Nullable<TimeSpan>()`, no longer fails to parse. [#348](https://github.com/nojaf/telplin/issues/348)
* A type declared in a module with `ModuleSuffix` (explicit, or implicit when a type shares the module's name) is no longer qualified with the module name inside that module. [#71](https://github.com/nojaf/telplin/issues/71)
* A backtick inside a backticked name, such as ```` ```a` b`` ````, is escaped correctly. Fixed by the FCS update, now covered by a test. [#88](https://github.com/nojaf/telplin/issues/88)

## [0.14.0] - 2026-08-30

### Changed
* Let bindings that no other file of the project uses are left out by default. `--keep-unused` keeps them, and `--include-private-bindings` implies it. The `--only-used` flag is gone.

### Added
* The input can be a source file (.fs). The nearest project above it that has the file as a Compile item is used, and only that file is processed. [#372](https://github.com/nojaf/telplin/pull/372)

## [0.13.0] - 2026-08-30

### Added
* `--only-used` leaves out let bindings that no other file of the project uses. Types, members and bindings with an attribute are kept. Only the project is looked at, so this is for files internal to a project. [#371](https://github.com/nojaf/telplin/pull/371)
* The project is type checked before a run, and again with the new signatures in place before anything is written. `--no-verify` skips the second check, `--force` writes despite it.
* Signature files are listed in the project file, directly before their implementation file. `--no-project` opts out.
* The `private` keyword is removed from let bindings the signature leaves out. `--keep-private` opts out.
* `--files` accepts a path relative to the project, such as `Api.fs` or `App/Api.fs`, from any working directory.
* The input can be a folder that holds exactly one project file.

### Changed
* A project that does not compile is refused with its diagnostics.
* The generated `AssemblyInfo.fs` and `AssemblyAttributes.fs` no longer get a signature file.

## [0.12.0] - 2026-08-30

### Changed
* Update to FCS 43.12.400 and Fantomas.Core 8.0.0-alpha-026. [#370](https://github.com/nojaf/telplin/pull/370)
* A property whose accessors differ in accessibility is now a single member with per-accessor accessibility, `member Y: int with private get, set`, instead of one member per accessor.
* Records in generated signatures follow the Fantomas 8 default bracket style, braces on their own lines.
* New `--help` page and `--version` flag, Argu is no longer used. An unknown flag suggests the closest one, and errors go to standard error.

## [0.11.0] - 2026-04-10

### Changed
* Update tfm to `net10.0`

## [0.10.0] - 2025-02-10

### Changed
* Update to FCS 43.9.101

## [0.9.6] - 2024-02-22

### Fixed
* Use `__NonExistentSubDir__` to force design time build.

## [0.9.5] - 2024-01-31

### Fixed
* Very minor tweaks in the `dotnet msbuild` command execution.

## [0.9.4] - 2024-01-29

### Fixed
* `[<Class>]` attribute is missing when constructor is private. [#177](https://github.com/nojaf/telplin/issues/177)
* Use /restore for design time build. [#187](https://github.com/nojaf/telplin/pull/187)

## [0.9.3] - 2023-12-12

### Fixed
* Pass in dummy `/p:Version` during design time build, to avoid target `CoreCompile` being skipped.

## [0.9.2] - 2023-12-01

### Fixed
* Allow project cracking when dotnet sdk (via global.json) is lowered than 8.

## [0.9.1] - 2023-11-22

### Fixed
* Recursively find project references.

## [0.9.0] - 2023-11-22

### Changed
* Update to FCS 43.8.100
* Update tfm to `net8.0`

## [0.8.3] - 2023-06-28

### Fixed
* Private keyword lost in getter. [#87](https://github.com/nojaf/telplin/issues/87)
* Inline keyword is missing. [#90](https://github.com/nojaf/telplin/issues/90)
* Generic type with static member. [#89](https://github.com/nojaf/telplin/issues/89)

## [0.8.2] - 2023-06-14

### Fixed
* Order of generic parameters don't match. [#82](https://github.com/nojaf/telplin/issues/82)

## [0.8.1] - 2023-06-13

### Fixed
* C# Override member generation [#84](https://github.com/nojaf/telplin/issues/84)

## [0.8.0] - 2023-06-12

### Changed
* Exclude private constructors by default. [#70](https://github.com/nojaf/telplin/issues/70)

### Fixed
* Private constructor in struct. [#79](https://github.com/nojaf/telplin/issues/79)
* Generic type parameter doesn't parse internally. [#68](https://github.com/nojaf/telplin/issues/68)
* Optional function type cannot be parsed. [#78](https://github.com/nojaf/telplin/issues/78)
* Property member with function return type. [#66](https://github.com/nojaf/telplin/issues/66)
* Mutable is lost from let binding. [#67](https://github.com/nojaf/telplin/issues/67)
* Non comparable struct should have `[<NoComparison>]` attribute [#80](https://github.com/nojaf/telplin/issues/80)

## [0.7.0] - 2023-06-05

### Changed
* Better error handling and partial signature generation. [#72](https://github.com/nojaf/telplin/issues/72)
* Update FCS to 43.7.400-preview.23302.5

## [0.6.0] - 2023-06-01

### Changed
* Private let bindings are no longer included by default. Use `--include-private-bindings` to include them. [#70](https://github.com/nojaf/telplin/issues/70)

## [0.5.2] - 2023-05-22

### Changed
* Update FCS to 43.7.400-preview.23271.1

### Fixed
* Wrong signature for member getter, setter with extra parameters. [#52](https://github.com/nojaf/telplin/issues/52)
* Setter with different input than return type. [#61](https://github.com/nojaf/telplin/issues/61)
* Types in a recursive module require the and keyword. [#62](https://github.com/nojaf/telplin/issues/62)

## [0.5.1] - 2023-11-05

### Fixed
* static getter/setter should remain static. [#51](https://github.com/nojaf/telplin/issues/51)
* Member with abstract decl and default impl uses "member" instead of "override" for impl signature entry [#53](https://github.com/nojaf/telplin/issues/53)
* Don't add [<Class>] attribute for interface. [#55](https://github.com/nojaf/telplin/issues/55)
* Access modifier from member is missing. [#56](https://github.com/nojaf/telplin/issues/56)
* Duplicate constraints should be avoided. [#57](https://github.com/nojaf/telplin/issues/57)

## [0.5.0] - 2023-05-02

### Added
* Accept additional arguments for building .fsproj file. (Example: `telplin MyProject.fsproj -- -c Release`)

### Fixed
* Take compiler defines into account when parsing the syntax tree.
* Flexible parameter generation issue. [#47](https://github.com/nojaf/telplin/issues/47)

## [0.4.0] - 2023-04-28

### Fixed
* Add support for enums.
* Add support for type delegates.

### Added
* Also generate an `.rsp` file from the input `.fsproj` file using `--record`.
* Only record an `.rsp` file using `--record-only`.

### Changed
* The main input also accepts a `.rsp` file or `.binlog` file next to an `.fsproj`.
* Replace `--write` with `--dry-run`. The behaviour is now inverted. By default files will be written.

## [0.3.2] - 2023-04-28

### Fixed
* Override val not respected. [#38](https://github.com/nojaf/telplin/issues/38)
* Constraint is missing from binding. [#39](https://github.com/nojaf/telplin/issues/39)

## [0.3.1] - 2023-04-25

### Fixed
* Wildcard array should not be used from untyped tree. [#30](https://github.com/nojaf/telplin/issues/30)
* Inline keyword in type extension is not preserved. [#31](https://github.com/nojaf/telplin/issues/31)
* with get,set is lost. [#33](https://github.com/nojaf/telplin/issues/33)
* Generic type argument should be preserved. [#32](https://github.com/nojaf/telplin/issues/32)

## [0.3.0] - 2023-04-12

### Changed
* The console application no longer takes an MSBuild binary log file as input, but a `fsproj`instead.
* Update to Fantomas.Core v6.0.0-beta-001.
* Update to FSharp.Compiler.Service v43.7.300-preview.23167.4

## [0.2.0] - 2022-12-17

### Fixed
* `[<Class>]` should not be added twice. [#1](https://github.com/nojaf/telplin/issues/1)
* Member constraints are not properly processed. [#9](https://github.com/nojaf/telplin/issues/9)

### Changed
* Update to Fantomas.Core v5.1.0-beta-002.

## [0.1.0] - 2022-09-30

### Added

* Initial release
