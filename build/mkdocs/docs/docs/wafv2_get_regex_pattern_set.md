<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>wafv2_get_regex_pattern_set</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves the specified RegexPatternSet

### Description

Retrieves the specified RegexPatternSet.

### Usage

    wafv2_get_regex_pattern_set(Name, Scope, Id)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="wafv2_get_regex_pattern_set_:_Name">Name</code></td>
<td><p>[required] The name of the set. You cannot change the name after
you create the set.</p></td>
</tr>
<tr class="even">
<td><code id="wafv2_get_regex_pattern_set_:_Scope">Scope</code></td>
<td><p>[required] Specifies whether this is for an Amazon CloudFront
distribution or for a regional application. A regional application can
be an Application Load Balancer (ALB), an Amazon API Gateway REST API,
an AppSync GraphQL API, an Amazon Cognito user pool, an App Runner
service, or an Amazon Web Services Verified Access instance.</p>
<p>To work with CloudFront, you must also specify the Region US East (N.
Virginia) as follows:</p>
<ul>
<li><p>CLI - Specify the Region when you use the CloudFront scope:
<code>--scope=CLOUDFRONT --region=us-east-1</code>.</p></li>
<li><p>API and SDKs - For all calls, use the Region endpoint
us-east-1.</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code id="wafv2_get_regex_pattern_set_:_Id">Id</code></td>
<td><p>[required] A unique identifier for the set. This ID is returned
in the responses to create and list commands. You provide it to
operations like update and delete.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      RegexPatternSet = list(
        Name = "string",
        Id = "string",
        ARN = "string",
        Description = "string",
        RegularExpressionList = list(
          list(
            RegexString = "string"
          )
        )
      ),
      LockToken = "string"
    )

### Request syntax

    svc$get_regex_pattern_set(
      Name = "string",
      Scope = "CLOUDFRONT"|"REGIONAL",
      Id = "string"
    )