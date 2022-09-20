namespace Autograph.Common

[<RequireQualifiedAccess>]
type ParameterTypeName =
    | SingleIdentifier of name : string
    | FunctionType of types : ParameterTypeName list
    | GenericParameter of name : string * isSolveAtCompileTime : bool
    | PostFix of mainType : ParameterTypeName * postType : ParameterTypeName
    | WithGenericArguments of name : ParameterTypeName * args : ParameterTypeName list
    | Tuple of types : ParameterTypeName list

type RangeProxy =
    struct
        val StartLine : int
        val StartColumn : int
        val EndLine : int
        val EndColumn : int
        new : startLine : int * startColumn : int * endLine : int * endColumn : int -> RangeProxy
    end

type ReturnTypeResponse =
    {
        FullType : ParameterTypeName
        ReturnParameter : ParameterTypeName
    }

type TypeInfoResponse = { IsClass : bool }

type TypedTreeInfoResolver =
    abstract member GetTypeInfo : range : RangeProxy -> TypeInfoResponse
    abstract member GetFullForBinding : bindingNameRange : RangeProxy -> string
