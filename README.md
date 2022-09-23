# Autograph

It's a bit of secret for now.

## Scratch

dotnet build -v n /m:1 --no-incremental  | dotnet fsi .\script.fsx
dotnet build -v n /m:1 --no-incremental --no-dependencies | dotnet fsi ..\script.fsx
C:/Users/nojaf/Projects/autograph/src/Autograph/bin/Debug/net7.0/autograph.exe
dotnet build -bl --no-incremental ; C:/Users/nojaf/Projects/autograph/src/Autograph/bin/Debug/net7.0/autograph.exe .\msbuild.binlog --write


## Ideas

- Console runner
- Setting to skip the `private` nodes?
- Documentation limitations (conditional directives)

## Lamdbda

dotnet lambda package
dotnet publish --output "C:\Users\nojaf\Projects\autograph\src\Autograph.Lambda\bin\Release\net7.0\publish" --configuration "Release" --framework "net7.0" --self-contained true /p:GenerateRuntimeConfigurationFiles=true --runtime linux-x64

```http request
POST https://5n56jxbgr5.execute-api.eu-west-3.amazonaws.com/autograph-main-stage-3adc4b3/autograph/signature
Content-Type: text/plain
```

## Resolved options inside SDK container:

```
-o:obj/Debug/net7.0/SampleLibrary.dll
-g
--debug:portable
--noframework
--define:TRACE
--define:DEBUG
--define:NET
--define:NET7_0
--define:NETCOREAPP
--define:NET5_0_OR_GREATER
--define:NET6_0_OR_GREATER
--define:NET7_0_OR_GREATER
--define:NETCOREAPP1_0_OR_GREATER
--define:NETCOREAPP1_1_OR_GREATER
--define:NETCOREAPP2_0_OR_GREATER
--define:NETCOREAPP2_1_OR_GREATER
--define:NETCOREAPP2_2_OR_GREATER
--define:NETCOREAPP3_0_OR_GREATER
--define:NETCOREAPP3_1_OR_GREATER
--optimize-
--tailcalls-
-r:/root/.nuget/packages/fsharp.core/6.0.6/lib/netstandard2.1/FSharp.Core.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/Microsoft.CSharp.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/Microsoft.VisualBasic.Core.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/Microsoft.VisualBasic.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/Microsoft.Win32.Primitives.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/Microsoft.Win32.Registry.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/mscorlib.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/netstandard.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.AppContext.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Buffers.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Collections.Concurrent.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Collections.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Collections.Immutable.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Collections.NonGeneric.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Collections.Specialized.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.ComponentModel.Annotations.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.ComponentModel.DataAnnotations.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.ComponentModel.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.ComponentModel.EventBasedAsync.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.ComponentModel.Primitives.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.ComponentModel.TypeConverter.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Configuration.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Console.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Core.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Data.Common.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Data.DataSetExtensions.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Data.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Diagnostics.Contracts.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Diagnostics.Debug.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Diagnostics.DiagnosticSource.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Diagnostics.FileVersionInfo.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Diagnostics.Process.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Diagnostics.StackTrace.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Diagnostics.TextWriterTraceListener.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Diagnostics.Tools.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Diagnostics.TraceSource.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Diagnostics.Tracing.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Drawing.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Drawing.Primitives.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Dynamic.Runtime.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Formats.Asn1.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Formats.Tar.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Globalization.Calendars.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Globalization.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Globalization.Extensions.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.Compression.Brotli.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.Compression.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.Compression.FileSystem.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.Compression.ZipFile.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.FileSystem.AccessControl.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.FileSystem.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.FileSystem.DriveInfo.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.FileSystem.Primitives.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.FileSystem.Watcher.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.IsolatedStorage.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.MemoryMappedFiles.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.Pipes.AccessControl.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.Pipes.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.IO.UnmanagedMemoryStream.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Linq.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Linq.Expressions.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Linq.Parallel.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Linq.Queryable.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Memory.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.Http.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.Http.Json.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.HttpListener.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.Mail.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.NameResolution.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.NetworkInformation.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.Ping.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.Primitives.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.Quic.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.Requests.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.Security.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.ServicePoint.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.Sockets.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.WebClient.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.WebHeaderCollection.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.WebProxy.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.WebSockets.Client.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Net.WebSockets.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Numerics.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Numerics.Vectors.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.ObjectModel.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Reflection.DispatchProxy.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Reflection.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Reflection.Emit.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Reflection.Emit.ILGeneration.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Reflection.Emit.Lightweight.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Reflection.Extensions.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Reflection.Metadata.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Reflection.Primitives.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Reflection.TypeExtensions.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Resources.Reader.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Resources.ResourceManager.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Resources.Writer.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.CompilerServices.Unsafe.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.CompilerServices.VisualC.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.Extensions.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.Handles.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.InteropServices.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.InteropServices.JavaScript.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.InteropServices.RuntimeInformation.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.Intrinsics.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.Loader.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.Numerics.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.Serialization.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.Serialization.Formatters.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.Serialization.Json.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.Serialization.Primitives.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Runtime.Serialization.Xml.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.AccessControl.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Claims.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Cryptography.Algorithms.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Cryptography.Cng.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Cryptography.Csp.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Cryptography.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Cryptography.Encoding.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Cryptography.OpenSsl.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Cryptography.Primitives.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Cryptography.X509Certificates.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Principal.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.Principal.Windows.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Security.SecureString.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.ServiceModel.Web.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.ServiceProcess.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Text.Encoding.CodePages.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Text.Encoding.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Text.Encoding.Extensions.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Text.Encodings.Web.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Text.Json.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Text.RegularExpressions.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Threading.Channels.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Threading.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Threading.Overlapped.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Threading.Tasks.Dataflow.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Threading.Tasks.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Threading.Tasks.Extensions.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Threading.Tasks.Parallel.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Threading.Thread.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Threading.ThreadPool.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Threading.Timer.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Transactions.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Transactions.Local.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.ValueTuple.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Web.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Web.HttpUtility.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Windows.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Xml.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Xml.Linq.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Xml.ReaderWriter.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Xml.Serialization.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Xml.XDocument.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Xml.XmlDocument.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Xml.XmlSerializer.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Xml.XPath.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/System.Xml.XPath.XDocument.dll
-r:/usr/share/dotnet/packs/Microsoft.NETCore.App.Ref/7.0.0-rc.1.22426.10/ref/net7.0/WindowsBase.dll
--target:library
--nowarn:IL2121
--warn:3
--warnaserror:3239,FS0025
--fullpaths
--flaterrors
--highentropyva+
--targetprofile:netcore
--nocopyfsharpcore
--deterministic+
--simpleresolution
```

Laurelin & Telperion

Telplin?