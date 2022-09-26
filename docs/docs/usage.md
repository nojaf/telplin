---
index: 2
---
# Usage

Telplin was designed to create a starting point when introducing signature files to a code base.  
It is meant to be used **one time**, and was designed to keep your signature files up to date.

## Installation

The recommendation is to install Telplin globally:

> dotnet tool install -g telplin

As the project has not yet matured, it is advised to run `telplin --help` to see what options are available.

## Post processing

Once your signature files are generated, you want to include them in your `fsproj`.
Inspect each file to see if anything can be trimmed.  
Signature files are more effective if they contain only the information the module/namespace truly needs to expose.  
All implementation details should be stripped from your signature file.

<tp-nav previous="./motivation.html" next="./technical-overview.html"></tp-nav>