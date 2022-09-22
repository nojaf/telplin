namespace Autograph.Common

type RangeProxy =
    struct
        val StartLine : int
        val StartColumn : int
        val EndLine : int
        val EndColumn : int
        new : startLine : int * startColumn : int * endLine : int * endColumn : int -> RangeProxy
        override ToString : unit -> string
    end

type GenericConstraintForParameter =
    {
        ParameterName : string
        IsHeadType : bool
        IsCompilerGenerated : bool
        Constraints : GenericConstraint list
    }

and GenericConstraint =
    {
        IsEqualityConstraint : bool
        IsReferenceTypeConstraint : bool
        CoercesToTarget : string option
    }

type BindingInfo = string * GenericConstraintForParameter list

type TypeInfoResponse =
    {
        IsClass : bool
        ConstructorInfo : BindingInfo option
    }

type TypedTreeInfoResolver =
    abstract member GetTypeInfo : range : RangeProxy -> TypeInfoResponse
    abstract member GetFullForBinding : bindingNameRange : RangeProxy -> BindingInfo

type FSharpDiagnosticInfoSeverity =
    | Warning = 0
    | Error = 1

type FSharpDiagnosticInfo =
    {
        Severity : FSharpDiagnosticInfoSeverity
        Message : string
        ErrorNumber : string
        Range : RangeProxy
    }
