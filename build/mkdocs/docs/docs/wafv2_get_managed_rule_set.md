<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>wafv2_get_managed_rule_set</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves the specified managed rule set

### Description

Retrieves the specified managed rule set.

This is intended for use only by vendors of managed rule sets. Vendors
are Amazon Web Services and Amazon Web Services Marketplace sellers.

Vendors, you can use the managed rule set APIs to provide controlled
rollout of your versioned managed rule group offerings for your
customers. The APIs are `list_managed_rule_sets`,
`get_managed_rule_set`, `put_managed_rule_set_versions`, and
`update_managed_rule_set_version_expiry_date`.

### Usage

    wafv2_get_managed_rule_set(Name, Scope, Id)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="wafv2_get_managed_rule_set_:_Name">Name</code></td>
<td><p>[required] The name of the managed rule set. You use this, along
with the rule set ID, to identify the rule set.</p>
<p>This name is assigned to the corresponding managed rule group, which
your customers can access and use.</p></td>
</tr>
<tr class="even">
<td><code id="wafv2_get_managed_rule_set_:_Scope">Scope</code></td>
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
<td><code id="wafv2_get_managed_rule_set_:_Id">Id</code></td>
<td><p>[required] A unique identifier for the managed rule set. The ID
is returned in the responses to commands like <code>list</code>. You
provide it to operations like <code>get</code> and
<code>update</code>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ManagedRuleSet = list(
        Name = "string",
        Id = "string",
        ARN = "string",
        Description = "string",
        PublishedVersions = list(
          list(
            AssociatedRuleGroupArn = "string",
            Capacity = 123,
            ForecastedLifetime = 123,
            PublishTimestamp = as.POSIXct(
              "2015-01-01"
            ),
            LastUpdateTimestamp = as.POSIXct(
              "2015-01-01"
            ),
            ExpiryTimestamp = as.POSIXct(
              "2015-01-01"
            )
          )
        ),
        RecommendedVersion = "string",
        LabelNamespace = "string"
      ),
      LockToken = "string"
    )

### Request syntax

    svc$get_managed_rule_set(
      Name = "string",
      Scope = "CLOUDFRONT"|"REGIONAL",
      Id = "string"
    )
