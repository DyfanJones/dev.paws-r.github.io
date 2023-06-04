<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>lambda_list_layers</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists Lambda layers and shows information about the latest version of each

### Description

Lists [Lambda
layers](https://docs.aws.amazon.com/lambda/latest/dg/invocation-layers.html)
and shows information about the latest version of each. Specify a
[runtime
identifier](https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html)
to list only layers that indicate that they're compatible with that
runtime. Specify a compatible architecture to include only layers that
are compatible with that [instruction set
architecture](https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html).

### Usage

    lambda_list_layers(CompatibleRuntime, Marker, MaxItems,
      CompatibleArchitecture)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="lambda_list_layers_:_CompatibleRuntime">CompatibleRuntime</code></td>
<td><p>A runtime identifier. For example, <code>go1.x</code>.</p></td>
</tr>
<tr class="even">
<td><code id="lambda_list_layers_:_Marker">Marker</code></td>
<td><p>A pagination token returned by a previous call.</p></td>
</tr>
<tr class="odd">
<td><code id="lambda_list_layers_:_MaxItems">MaxItems</code></td>
<td><p>The maximum number of layers to return.</p></td>
</tr>
<tr class="even">
<td><code
id="lambda_list_layers_:_CompatibleArchitecture">CompatibleArchitecture</code></td>
<td><p>The compatible <a
href="https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html">instruction
set architecture</a>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      NextMarker = "string",
      Layers = list(
        list(
          LayerName = "string",
          LayerArn = "string",
          LatestMatchingVersion = list(
            LayerVersionArn = "string",
            Version = 123,
            Description = "string",
            CreatedDate = "string",
            CompatibleRuntimes = list(
              "nodejs"|"nodejs4.3"|"nodejs6.10"|"nodejs8.10"|"nodejs10.x"|"nodejs12.x"|"nodejs14.x"|"nodejs16.x"|"java8"|"java8.al2"|"java11"|"python2.7"|"python3.6"|"python3.7"|"python3.8"|"python3.9"|"dotnetcore1.0"|"dotnetcore2.0"|"dotnetcore2.1"|"dotnetcore3.1"|"dotnet6"|"nodejs4.3-edge"|"go1.x"|"ruby2.5"|"ruby2.7"|"provided"|"provided.al2"|"nodejs18.x"|"python3.10"|"java17"
            ),
            LicenseInfo = "string",
            CompatibleArchitectures = list(
              "x86_64"|"arm64"
            )
          )
        )
      )
    )

### Request syntax

    svc$list_layers(
      CompatibleRuntime = "nodejs"|"nodejs4.3"|"nodejs6.10"|"nodejs8.10"|"nodejs10.x"|"nodejs12.x"|"nodejs14.x"|"nodejs16.x"|"java8"|"java8.al2"|"java11"|"python2.7"|"python3.6"|"python3.7"|"python3.8"|"python3.9"|"dotnetcore1.0"|"dotnetcore2.0"|"dotnetcore2.1"|"dotnetcore3.1"|"dotnet6"|"nodejs4.3-edge"|"go1.x"|"ruby2.5"|"ruby2.7"|"provided"|"provided.al2"|"nodejs18.x"|"python3.10"|"java17",
      Marker = "string",
      MaxItems = 123,
      CompatibleArchitecture = "x86_64"|"arm64"
    )