# Telplin

![Logo](https://raw.githubusercontent.com/nojaf/telplin/main/Telplin.png)

Telplin helps F# users to generate matching signature files for implementation files.

## Usage

```bash
dotnet tool install -g telplin
telplin src/App/App.fsproj --files App/Api.fs --only-used
```

This writes `Api.fsi` next to `Api.fs`, lists it in the project, and keeps only what other files of the project use: a let binding nothing else calls is left out, which makes it private. The project is type checked before and after, so a signature that does not match is never written.

The same, from the file itself: Telplin finds the project it belongs to.

```bash
telplin src/App/Api.fs --only-used
```

Leave out `--only-used` to get everything the file exposes, and `--files` to do the whole project. Run `telplin --help` for the other flags.

Checkout our [documentation](https://nojaf.com/telplin/docs/) for more information.

## Local development

### Run online tool

```bash
dotnet fsi build.fsx -- -p Watch
```

### Format code

```bash
dotnet fantomas src tests docs tool
```
