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

type TypeInfoResponse = { IsClass : bool }

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
    }

type TypedTreeInfoResolver =
    abstract member GetTypeInfo : range : RangeProxy -> TypeInfoResponse
    abstract member GetFullForBinding : bindingNameRange : RangeProxy -> string * GenericConstraintForParameter list
