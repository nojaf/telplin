module Program

open System
open System.IO
open System.Security.Cryptography
open Pulumi
open Pulumi.Aws.S3
open Pulumi.FSharp
open Pulumi.Aws
open Pulumi.Aws.Lambda

let (</>) a b = Path.Combine (a, b)
let projectName = "Telplin"
let loweredProjectName = projectName.ToLower ()

let sha256 filePath =
    File.ReadAllBytes filePath
    |> SHA256.Create().ComputeHash
    |> System.Text.Encoding.UTF8.GetString

let zipPath =
    __SOURCE_DIRECTORY__
    </> ".."
    </> ".."
    </> "tool"
    </> "server"
    </> "bin"
    </> "Release"
    </> "net8.0"
    </> "server.zip"

let infra () =
    let bucket = BucketV2 $"{loweredProjectName}-bucket"

    let bucketAcl =
        BucketAclV2 (
            $"{loweredProjectName}-bucket-acl",
            args = BucketAclV2Args (Acl = input "private", Bucket = io bucket.Id)
        )

    let bucketObject =
        BucketObjectv2 (
            $"{loweredProjectName}-lambda-archive",
            BucketObjectv2Args (
                Key = input $"{loweredProjectName}-lambda-archive.zip",
                Bucket = io bucket.Id,
                Acl = bucketAcl.Acl,
                Source = input (FileArchive zipPath)
            )
        )

    let lambdaRole =
        Iam.Role (
            $"{projectName}LambdaRole",
            Iam.RoleArgs (
                AssumeRolePolicy =
                    input
                        """{
    "Version": "2012-10-17",
    "Statement": [{
        "Action": "sts:AssumeRole",
        "Principal": {
            "Service": "lambda.amazonaws.com"
        },
        "Effect": "Allow",
        "Sid": ""
    }]
}
"""
            )
        )

    let _policy =
        let args =
            Iam.RolePolicyArgs (
                Policy =
                    input
                        """{
	                                "Version": "2012-10-17",
	                                "Statement": [{
		                                "Effect": "Allow",
		                                "Action": [
			                                "logs:CreateLogGroup",
			                                "logs:CreateLogStream",
			                                "logs:PutLogEvents"
		                                ],
		                                "Resource": "arn:aws:logs:*:*:*"
	                                }]
                            }""",
                Role = io lambdaRole.Id
            )

        Iam.RolePolicy ($"{loweredProjectName}-log-policy", args)

    let gateway =
        let cors =
            ApiGatewayV2.Inputs.ApiCorsConfigurationArgs (
                AllowHeaders = inputList [ input "*" ],
                AllowMethods = inputList [ input "*" ],
                AllowOrigins =
                    inputList
                        [
                            input "https://nojaf.github.io"
                            input "https://nojaf.com"
                            input "https://localhost:8900"
                        ]
            )

        let args =
            ApiGatewayV2.ApiArgs (ProtocolType = input "HTTP", CorsConfiguration = input cors)

        ApiGatewayV2.Api ($"{loweredProjectName}-gateway", args)

    let _mainStage =
        let args = ApiGatewayV2.StageArgs (ApiId = io gateway.Id, AutoDeploy = input true)

        ApiGatewayV2.Stage ($"{loweredProjectName}-main-stage", args)

    let lambda =
        let args =
            FunctionArgs (
                Handler = input "bootstrap",
                S3Bucket = io bucket.Id,
                S3Key = io bucketObject.Key,
                SourceCodeHash = input (sha256 zipPath),
                Runtime = inputUnion2Of2 Runtime.CustomAL2,
                Role = io lambdaRole.Arn,
                Timeout = input 30,
                MemorySize = input 512
            )

        Function (loweredProjectName, args)

    let _log =
        CloudWatch.LogGroup (
            $"{loweredProjectName}-log-group",
            CloudWatch.LogGroupArgs (
                RetentionInDays = input 30,
                Name = io (lambda.Id.Apply (fun id -> $"/aws/lambda/{id}"))
            )
        )

    let _lambdaPermission =
        Permission (
            $"{loweredProjectName}-lambda-permissions",
            PermissionArgs (
                Function = io lambda.Name,
                Principal = input "apigateway.amazonaws.com",
                Action = input "lambda:InvokeFunction",
                SourceArn = io (Output.Format $"{gateway.ExecutionArn}/*")
            )
        )

    let lambdaIntegration =
        let args =
            ApiGatewayV2.IntegrationArgs (
                ApiId = io gateway.Id,
                IntegrationType = input "AWS_PROXY",
                IntegrationMethod = input "POST",
                IntegrationUri = io lambda.Arn
            )

        ApiGatewayV2.Integration ($"{loweredProjectName}-integration", args)

    let _apiRoute =
        let args =
            ApiGatewayV2.RouteArgs (
                ApiId = io gateway.Id,
                RouteKey = input $"POST /{loweredProjectName}/signature",
                Target = io (lambdaIntegration.Id.Apply (fun id -> $"integrations/{id}"))
            )

        ApiGatewayV2.Route ($"{loweredProjectName}-route", args)

    dict [ ("lambdaId", lambda.Id :> obj) ]

[<EntryPoint>]
let main _ = Deployment.run infra
