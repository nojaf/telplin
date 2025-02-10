namespace Telplin.Lambda

open Amazon.Lambda.APIGatewayEvents
open Amazon.Lambda.Core
open Amazon.Lambda.RuntimeSupport
open Amazon.Lambda.Serialization.SystemTextJson

open System

module Function =

#if RELEASE
    [<EntryPoint>]
#endif
    let main _args =

        let handler =
            Func<APIGatewayProxyRequest, ILambdaContext, APIGatewayProxyResponse> Implementation.PostSignature

        use bootstrap =
            LambdaBootstrapBuilder.Create(handler, new DefaultLambdaJsonSerializer ()).Build ()

        bootstrap.RunAsync().GetAwaiter().GetResult ()
        0
