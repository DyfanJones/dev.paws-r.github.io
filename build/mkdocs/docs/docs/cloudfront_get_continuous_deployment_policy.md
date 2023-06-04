<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>cloudfront_get_continuous_deployment_policy</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets a continuous deployment policy, including metadata (the policy's identifier and the date and time when the policy was last modified)

### Description

Gets a continuous deployment policy, including metadata (the policy's
identifier and the date and time when the policy was last modified).

### Usage

    cloudfront_get_continuous_deployment_policy(Id)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="cloudfront_get_continuous_deployment_policy_:_Id">Id</code></td>
<td><p>[required] The identifier of the continuous deployment policy
that you are getting.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ContinuousDeploymentPolicy = list(
        Id = "string",
        LastModifiedTime = as.POSIXct(
          "2015-01-01"
        ),
        ContinuousDeploymentPolicyConfig = list(
          StagingDistributionDnsNames = list(
            Quantity = 123,
            Items = list(
              "string"
            )
          ),
          Enabled = TRUE|FALSE,
          TrafficConfig = list(
            SingleWeightConfig = list(
              Weight = 123.0,
              SessionStickinessConfig = list(
                IdleTTL = 123,
                MaximumTTL = 123
              )
            ),
            SingleHeaderConfig = list(
              Header = "string",
              Value = "string"
            ),
            Type = "SingleWeight"|"SingleHeader"
          )
        )
      ),
      ETag = "string"
    )

### Request syntax

    svc$get_continuous_deployment_policy(
      Id = "string"
    )