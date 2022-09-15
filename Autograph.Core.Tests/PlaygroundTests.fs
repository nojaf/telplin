module Autograph.Core.Tests.PlaygroundTests

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open NUnit.Framework
open TestHelper

let targetFile =
    @"C:\Users\nojaf\Projects\autograph\Autograph.UntypedTree\UntypedTree.fs"

let compilerArgs =
    [|
        @"-o:C:\Users\nojaf\Projects\autograph\Autograph.UntypedTree\obj\Debug\net7.0\Autograph.UntypedTree.dll"
        @"-g"
        @"--debug:portable"
        @"--noframework"
        @"--define:TRACE"
        @"--define:DEBUG"
        @"--define:NET"
        @"--define:NET7_0"
        @"--define:NETCOREAPP"
        @"--define:NET5_0_OR_GREATER"
        @"--define:NET6_0_OR_GREATER"
        @"--define:NET7_0_OR_GREATER"
        @"--define:NETCOREAPP1_0_OR_GREATER"
        @"--define:NETCOREAPP1_1_OR_GREATER"
        @"--define:NETCOREAPP2_0_OR_GREATER"
        @"--define:NETCOREAPP2_1_OR_GREATER"
        @"--define:NETCOREAPP2_2_OR_GREATER"
        @"--define:NETCOREAPP3_0_OR_GREATER"
        @"--define:NETCOREAPP3_1_OR_GREATER"
        @"--doc:C:\Users\nojaf\Projects\autograph\Autograph.UntypedTree\obj\Debug\net7.0\Autograph.UntypedTree.xml"
        @"--optimize-"
        @"--tailcalls-"
        @"-r:C:\Users\nojaf\Projects\autograph\Autograph.Common\obj\Debug\net7.0\ref\Autograph.Common.dll"
        @"-r:C:\Users\nojaf\.nuget\packages\fantomas.core\5.0.0-beta-010\lib\netstandard2.0\Fantomas.Core.dll"
        @"-r:C:\Users\nojaf\.nuget\packages\fantomas.fcs\5.0.0-beta-010\lib\netstandard2.0\Fantomas.FCS.dll"
        @"-r:C:\Users\nojaf\.nuget\packages\fsharp.core\6.0.6\lib\netstandard2.1\FSharp.Core.dll"
        @"-r:C:\Users\nojaf\.nuget\packages\fslexyacc.runtime\10.2.0\lib\netstandard2.0\FsLexYacc.Runtime.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\Microsoft.CSharp.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\Microsoft.VisualBasic.Core.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\Microsoft.VisualBasic.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\Microsoft.Win32.Primitives.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\Microsoft.Win32.Registry.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\mscorlib.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\netstandard.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.AppContext.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Buffers.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Collections.Concurrent.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Collections.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Collections.Immutable.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Collections.NonGeneric.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Collections.Specialized.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.ComponentModel.Annotations.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.ComponentModel.DataAnnotations.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.ComponentModel.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.ComponentModel.EventBasedAsync.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.ComponentModel.Primitives.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.ComponentModel.TypeConverter.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Core.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Data.Common.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Data.DataSetExtensions.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Data.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Diagnostics.Contracts.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Diagnostics.Debug.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Diagnostics.DiagnosticSource.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Diagnostics.FileVersionInfo.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Diagnostics.Process.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Diagnostics.StackTrace.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Diagnostics.TextWriterTraceListener.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Diagnostics.Tools.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Diagnostics.TraceSource.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Diagnostics.Tracing.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Drawing.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Drawing.Primitives.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Dynamic.Runtime.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Formats.Asn1.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Formats.Tar.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Globalization.Calendars.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Globalization.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Globalization.Extensions.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.Compression.Brotli.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.Compression.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.Compression.FileSystem.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.Compression.ZipFile.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.FileSystem.AccessControl.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.FileSystem.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.FileSystem.DriveInfo.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.FileSystem.Primitives.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.FileSystem.Watcher.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.IsolatedStorage.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.MemoryMappedFiles.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.Pipes.AccessControl.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.Pipes.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.IO.UnmanagedMemoryStream.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Linq.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Linq.Expressions.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Linq.Parallel.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Linq.Queryable.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Memory.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.Http.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.Http.Json.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.HttpListener.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.Mail.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.NameResolution.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.NetworkInformation.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.Ping.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.Primitives.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.Quic.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.Requests.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.Security.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.ServicePoint.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.Sockets.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.WebClient.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.WebHeaderCollection.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.WebProxy.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.WebSockets.Client.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Net.WebSockets.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Numerics.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Numerics.Vectors.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.ObjectModel.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Reflection.DispatchProxy.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Reflection.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Reflection.Emit.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Reflection.Emit.ILGeneration.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Reflection.Emit.Lightweight.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Reflection.Extensions.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Reflection.Metadata.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Reflection.Primitives.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Reflection.TypeExtensions.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Resources.Reader.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Resources.ResourceManager.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Resources.Writer.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.CompilerServices.Unsafe.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.CompilerServices.VisualC.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.Extensions.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.Handles.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.InteropServices.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.InteropServices.JavaScript.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.InteropServices.RuntimeInformation.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.Intrinsics.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.Loader.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.Numerics.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.Serialization.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.Serialization.Formatters.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.Serialization.Json.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.Serialization.Primitives.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Runtime.Serialization.Xml.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.AccessControl.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Claims.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Cryptography.Algorithms.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Cryptography.Cng.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Cryptography.Csp.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Cryptography.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Cryptography.Encoding.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Cryptography.OpenSsl.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Cryptography.Primitives.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Cryptography.X509Certificates.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Principal.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.Principal.Windows.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Security.SecureString.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.ServiceModel.Web.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.ServiceProcess.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Text.Encoding.CodePages.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Text.Encoding.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Text.Encoding.Extensions.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Text.Encodings.Web.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Text.Json.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Text.RegularExpressions.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Threading.Channels.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Threading.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Threading.Overlapped.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Threading.Tasks.Dataflow.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Threading.Tasks.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Threading.Tasks.Extensions.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Threading.Tasks.Parallel.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Threading.Thread.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Threading.ThreadPool.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Threading.Timer.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Transactions.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Transactions.Local.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.ValueTuple.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Web.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Web.HttpUtility.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Windows.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Xml.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Xml.Linq.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Xml.ReaderWriter.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Xml.Serialization.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Xml.XDocument.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Xml.XmlDocument.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Xml.XmlSerializer.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Xml.XPath.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\System.Xml.XPath.XDocument.dll"
        @"-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.0-rc.1.22426.10\ref\net7.0\WindowsBase.dll"
        @"--target:library"
        @"--nowarn:IL2121"
        @"--warn:3"
        @"--warnaserror:3239,FS0025"
        @"--fullpaths"
        @"--flaterrors"
        @"--highentropyva+"
        @"--targetprofile:netcore"
        @"--nocopyfsharpcore"
        @"--deterministic+"
        @"--simpleresolution"
        @"--refout:obj\Debug\net7.0\refint\Autograph.UntypedTree.dll"
        @"C:\Users\nojaf\Projects\autograph\Autograph.UntypedTree\obj\Debug\net7.0\.NETCoreApp,Version=v7.0.AssemblyAttributes.fs"
        @"C:\Users\nojaf\Projects\autograph\Autograph.UntypedTree\obj\Debug\net7.0\Autograph.UntypedTree.AssemblyInfo.fs"
        targetFile
    |]

let projectOptions =
    let sourceFiles =
        compilerArgs
        |> Array.filter (fun line -> line.EndsWith (".fs") && File.Exists (line))

    let otherOptions =
        compilerArgs |> Array.filter (fun line -> not (line.EndsWith (".fs")))

    {
        ProjectFileName = "Autograph.Common"
        ProjectId = None
        SourceFiles = sourceFiles
        OtherOptions = otherOptions
        ReferencedProjects = [||]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.Now
        UnresolvedReferences = None
        OriginalLoadReferences = []
        Stamp = None
    }

#if DEBUG
[<Test>]
#endif
let ``real world`` () =
    let code = File.ReadAllText targetFile
    let sourceText = SourceText.ofString code

    let resolver =
        Autograph.TypedTree.Resolver.mkResolverFor targetFile sourceText projectOptions

    let signature = Autograph.UntypedTree.Writer.mkSignatureFile resolver code

    signature
    |> shouldEqualWithPrepend
        """
module Autograph.UntypedTree.Writer
open System.IO
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Autograph.Common
open FSharp.Compiler.Text

val zeroRange: range
val mkSynTypeFun: types: SynType list -> SynType
val mkSynTypeParen: t: SynType -> SynType
val mkSynTypeTuple: mkType: ('a -> SynType) -> ts: 'a list -> SynType
val mkSynTypeOfParameterTypeName: p: ParameterTypeName -> SynType

type Range with

    member ToProxy: unit -> RangeProxy

val (|NewConstructorPattern|_|):
    memberFlags: SynMemberFlags option * headPat: SynPat -> (SynLongIdent * SynArgPats * SynAccess option) option

val (|RemoveParensInPat|): pat: SynPat -> SynPat
val collectInfoFromSynArgPats: argPats: SynArgPats -> Map<string, SynType>
val mkSignatureFile: resolver: TypedTreeInfoResolver -> code: string -> string
val mkSynModuleOrNamespaceSig: resolver: TypedTreeInfoResolver -> SynModuleOrNamespace -> SynModuleOrNamespaceSig
val mkSynModuleSigDecl: resolver: TypedTreeInfoResolver -> decl: SynModuleDecl -> SynModuleSigDecl list
val mkSynValSig: resolver: TypedTreeInfoResolver -> SynBinding -> SynValSig

val mkSynTypeForArity:
    resolver: TypedTreeInfoResolver ->
    mBindingName: range ->
    arity: SynArgInfo list list ->
    existingTypedParameters: Map<string, SynType> ->
    existingReturnType: SynType option ->
        SynType

val mkSynTypeDefnSig: resolver: TypedTreeInfoResolver -> SynTypeDefn -> SynTypeDefnSig
val mkSynExceptionDefn: resolver: TypedTreeInfoResolver -> SynExceptionDefn -> SynExceptionSig

val mkSynModuleSigDeclNestedModule:
    resolver: TypedTreeInfoResolver ->
    synComponentInfo: SynComponentInfo ->
    isRecursive: bool ->
    synModuleDecls: SynModuleDecl list ->
    trivia: SynModuleDeclNestedModuleTrivia ->
        SynModuleSigDecl

val mkSynMemberSig: resolver: TypedTreeInfoResolver -> md: SynMemberDefn -> SynMemberSig option
"""
