namespace Telplin.Core

/// Indicates something went wrong while accessing the Typed tree
type TelplinError = | TelplinError of range : Fantomas.FCS.Text.range * message : string
