namespace Autograph.Common

[<RequireQualifiedAccess>]
type ParameterTypeName =
    | SingleIdentifier of name : string
    | FunctionType of types : ParameterTypeName list
    | GenericParameter of name : string * isSolveAtCompileTime : bool
    | PostFix of mainType : ParameterTypeName * postType : ParameterTypeName
    | WithGenericArguments of name : string * args : ParameterTypeName list
    | Tuple of types : ParameterTypeName list

[<Struct>]
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
    end

type TypedTreeInfoResolver =
    abstract member GetTypeNameFor : range : RangeProxy -> ParameterTypeName
    abstract member GetReturnTypeFor : range : RangeProxy -> ParameterTypeName
