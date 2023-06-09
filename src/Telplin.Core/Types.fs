namespace Telplin.Core

type TelplinError = | TelplinError of range : Fantomas.FCS.Text.range * message : string
