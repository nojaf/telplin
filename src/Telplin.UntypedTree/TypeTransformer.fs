namespace Telplin.UntypedTree

open Fantomas.Core.SyntaxOak

[<AbstractClass>]
type TypeTransformerBase() =
    member x.TransformType (t : Type) : Type =
        match t with
        | Type.Funs node -> x.TransformFuns node |> Type.Funs
        | Type.Tuple node -> x.TransformTuple node |> Type.Tuple
        | Type.HashConstraint node -> x.TransformHashConstraint node |> Type.HashConstraint
        | Type.MeasurePower node -> x.TransformMeasurePower node |> Type.MeasurePower
        | Type.StaticConstant node -> x.TransformStaticConstant node |> Type.StaticConstant
        | Type.StaticConstantExpr node -> x.TransformStaticConstantExpr node |> Type.StaticConstantExpr
        | Type.StaticConstantNamed node -> x.TransformStaticConstantNamed node |> Type.StaticConstantNamed
        | Type.Array node -> x.TransformArray node |> Type.Array
        | Type.Anon node -> x.TransformAnon node |> Type.Anon
        | Type.Var node -> x.TransformVar node |> Type.Var
        | Type.AppPostfix node -> x.TransformAppPostfix node |> Type.AppPostfix
        | Type.AppPrefix node -> x.TransformAppPrefix node |> Type.AppPrefix
        | Type.StructTuple node -> x.TransformStructTuple node |> Type.StructTuple
        | Type.WithSubTypeConstraint node -> x.TransformWithSubTypeConstraint node |> Type.WithSubTypeConstraint
        | Type.WithGlobalConstraints node -> x.TransformWithGlobalConstraints node |> Type.WithGlobalConstraints
        | Type.LongIdent node -> x.TransformLongIdent node |> Type.LongIdent
        | Type.AnonRecord node -> x.TransformAnonRecord node |> Type.AnonRecord
        | Type.Paren node -> x.TransformParen node |> Type.Paren
        | Type.SignatureParameter node -> x.TransformSignatureParameter node |> Type.SignatureParameter
        | Type.Or node -> x.TransformOr node |> Type.Or
        | Type.LongIdentApp node -> x.TransformLongIdentApp node |> Type.LongIdentApp

    abstract TransformFuns : TypeFunsNode -> TypeFunsNode

    default x.TransformFuns (node : TypeFunsNode) =
        let parameters =
            node.Parameters |> List.map (fun (t, arrow) -> x.TransformType t, arrow)

        TypeFunsNode (parameters, x.TransformType node.ReturnType, node.Range)

    abstract TransformTuple : TypeTupleNode -> TypeTupleNode

    default x.TransformTuple (node : TypeTupleNode) =
        let path =
            node.Path
            |> List.map (
                function
                | Choice1Of2 t -> Choice1Of2 (x.TransformType t)
                | token -> token
            )

        TypeTupleNode (path, node.Range)

    abstract TransformHashConstraint : TypeHashConstraintNode -> TypeHashConstraintNode
    default x.TransformHashConstraint (node : TypeHashConstraintNode) = node
    abstract TransformMeasurePower : TypeMeasurePowerNode -> TypeMeasurePowerNode
    default x.TransformMeasurePower (node : TypeMeasurePowerNode) = node
    abstract TransformStaticConstant : Constant -> Constant
    default x.TransformStaticConstant (node : Constant) = node
    abstract TransformStaticConstantExpr : TypeStaticConstantExprNode -> TypeStaticConstantExprNode
    default x.TransformStaticConstantExpr (node : TypeStaticConstantExprNode) = node
    abstract TransformStaticConstantNamed : TypeStaticConstantNamedNode -> TypeStaticConstantNamedNode
    default x.TransformStaticConstantNamed (node : TypeStaticConstantNamedNode) = node
    abstract TransformArray : TypeArrayNode -> TypeArrayNode
    default x.TransformArray (node : TypeArrayNode) = node
    abstract TransformAnon : SingleTextNode -> SingleTextNode
    default x.TransformAnon (node : SingleTextNode) = node
    abstract TransformVar : SingleTextNode -> SingleTextNode
    default x.TransformVar (node : SingleTextNode) = node
    abstract TransformAppPostfix : TypeAppPostFixNode -> TypeAppPostFixNode

    default x.TransformAppPostfix (node : TypeAppPostFixNode) =
        TypeAppPostFixNode (x.TransformType node.First, x.TransformType node.Last, node.Range)

    abstract TransformAppPrefix : TypeAppPrefixNode -> TypeAppPrefixNode
    default x.TransformAppPrefix (node : TypeAppPrefixNode) = node
    abstract TransformStructTuple : TypeStructTupleNode -> TypeStructTupleNode
    default x.TransformStructTuple (node : TypeStructTupleNode) = node
    abstract TransformWithSubTypeConstraint : TypeConstraint -> TypeConstraint
    default x.TransformWithSubTypeConstraint (node : TypeConstraint) = node
    abstract TransformWithGlobalConstraints : TypeWithGlobalConstraintsNode -> TypeWithGlobalConstraintsNode

    default x.TransformWithGlobalConstraints (node : TypeWithGlobalConstraintsNode) =
        TypeWithGlobalConstraintsNode (x.TransformType node.Type, node.TypeConstraints, node.Range)

    abstract TransformLongIdent : IdentListNode -> IdentListNode
    default x.TransformLongIdent (node : IdentListNode) = node
    abstract TransformAnonRecord : TypeAnonRecordNode -> TypeAnonRecordNode

    default x.TransformAnonRecord (node : TypeAnonRecordNode) =
        let fields = node.Fields |> List.map (fun (name, t) -> name, x.TransformType t)
        TypeAnonRecordNode (node.Struct, node.Opening, fields, node.Closing, node.Range)

    abstract TransformParen : TypeParenNode -> TypeParenNode

    default x.TransformParen (node : TypeParenNode) =
        TypeParenNode (node.OpeningParen, x.TransformType node.Type, node.ClosingParen, node.Range)

    abstract TransformSignatureParameter : TypeSignatureParameterNode -> TypeSignatureParameterNode

    default x.TransformSignatureParameter (node : TypeSignatureParameterNode) =
        TypeSignatureParameterNode (node.Attributes, node.Identifier, x.TransformType node.Type, node.Range)

    abstract TransformOr : TypeOrNode -> TypeOrNode
    default x.TransformOr (node : TypeOrNode) = node
    abstract TransformLongIdentApp : TypeLongIdentAppNode -> TypeLongIdentAppNode
    default x.TransformLongIdentApp (node : TypeLongIdentAppNode) = node

module TypeTransformer =
    let transform (t : Type) (transformer : TypeTransformerBase) = transformer.TransformType t
