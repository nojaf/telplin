namespace Autograph.Common

type RangeProxy =
    struct
        val StartLine : int
        val StartColumn : int
        val EndLine : int
        val EndColumn : int

        new(startLine : int, startColumn : int, endLine : int, endColumn : int) =
            {
                StartLine = startLine
                StartColumn = startColumn
                EndLine = endLine
                EndColumn = endColumn
            }

        override this.ToString () =
            $"({this.StartLine}, {this.StartColumn}) ({this.EndLine},{this.EndColumn})"
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
