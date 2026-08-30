module Telplin.Core.TypedTree.Resolver

open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics

[<Class>]
type TypedTreeInfoResolver =
    /// Tries to find a FSharpSymbol for the given name and range.
    /// Returns the signature text for the found symbol.
    member GetValText :
        name : string * range : range * ?predicate : (FSharpMemberOrFunctionOrValue -> bool) -> Result<string, string>

    /// Similar to `GetValText` but look for a symbol with `.ctor` as CompiledName
    member GetValTextForConstructor : range : range -> Result<string, string>
    /// Verifies if an FSharpEntity is a struct without the IComparison interface.
    member IsStructWithoutComparison : range : range -> Result<bool, string>
    member Defines : string list
    member IncludePrivateBindings : bool
    /// Whether the module-level binding whose name has this range belongs in the signature.
    member KeepBinding : nameRange : range -> bool

/// Every binding is kept.
val keepEveryBinding : range -> bool

/// The module-level bindings of `sourceFile` that some other file of the project uses, as a test
/// on the range of a binding's name.
val bindingsUsedElsewhere : projectResults : FSharpCheckProjectResults -> sourceFile : string -> (range -> bool)

val mkResolverFor :
    checker : FSharpChecker ->
    sourceFileName : string ->
    sourceText : ISourceText ->
    projectOptions : FSharpProjectOptions ->
    includePrivateBindings : bool ->
    keepBinding : (range -> bool) ->
        TypedTreeInfoResolver

val mkResolverForCode :
    projectOptions : FSharpProjectOptions -> includePrivateBindings : bool -> code : string -> TypedTreeInfoResolver

/// A resolver for `code` as the first file of an in-memory project, keeping only the bindings the
/// other files (name, content) use.
val mkResolverForCodeOnlyUsed :
    projectOptions : FSharpProjectOptions ->
    includePrivateBindings : bool ->
    code : string ->
    otherFiles : (string * string) list ->
        TypedTreeInfoResolver

val typeCheckForPair :
    projectOptions : FSharpProjectOptions -> implementation : string -> signature : string -> FSharpDiagnostic array

val typeCheckForImplementation :
    projectOptions : FSharpProjectOptions -> sourceCode : string -> Choice<unit, FSharpDiagnostic array>

/// Type check the whole project with each signature (implementation path, signature text) placed
/// in front of its implementation. Returns the errors, an empty array when the pair compiles.
val typeCheckProjectWithSignatures :
    projectOptions : FSharpProjectOptions -> signatures : (string * string) list -> FSharpDiagnostic array

val FCSSignature : options : FSharpProjectOptions -> implementation : string -> Choice<unit, string>
