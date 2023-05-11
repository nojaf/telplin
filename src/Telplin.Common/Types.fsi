namespace Telplin.Common

type RangeProxy =
    struct
        val StartLine : int
        val StartColumn : int
        val EndLine : int
        val EndColumn : int
        new : startLine : int * startColumn : int * endLine : int * endColumn : int -> RangeProxy
        override ToString : unit -> string
    end

type GenericParameter =
    {
        ParameterName : string
        IsHeadType : bool
        IsCompilerGenerated : bool
        Constraints : GenericConstraint list
    }

and GenericConstraint =
    {
        IsEqualityConstraint : bool
        IsComparisonConstraint : bool
        IsReferenceTypeConstraint : bool
        IsSupportsNullConstraint : bool
        CoercesToTarget : string option
        MemberConstraint : MemberConstraintData option
    }

and MemberConstraintData =
    {
        IsStatic : bool
        MemberName : string
        Type : string
    }

type BindingInfo =
    {
        /// The return type of a FSharpMemberOrFunctionOrValue
        ReturnTypeString : string
        /// Generic parameters found in the FSharpMemberOrFunctionOrValue
        BindingGenericParameters : GenericParameter list
        /// Generic parameters found in the (optional) DeclaringEntity (FSharpEntity option)
        TypeGenericParameters : GenericParameter list
    }

type TypeInfoResponse =
    {
        NeedsClassAttribute : bool
        ConstructorInfo : BindingInfo option
    }

type PropertyWithIndexResponse =
    {
        IndexType : string
        SetType : string
        ReturnType : string
    }

type TypedTreeInfoResolver =
    abstract member GetTypeInfo : range : RangeProxy -> TypeInfoResponse
    abstract member GetFullForBinding : bindingNameRange : RangeProxy -> BindingInfo
    abstract member GetTypeTyparNames : range : RangeProxy -> string list
    abstract member GetPropertyWithIndex : range : RangeProxy -> PropertyWithIndexResponse

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
