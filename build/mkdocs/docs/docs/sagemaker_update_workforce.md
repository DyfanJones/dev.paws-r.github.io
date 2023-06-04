<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>sagemaker_update_workforce</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Use this operation to update your workforce

### Description

Use this operation to update your workforce. You can use this operation
to require that workers use specific IP addresses to work on tasks and
to update your OpenID Connect (OIDC) Identity Provider (IdP) workforce
configuration.

The worker portal is now supported in VPC and public internet.

Use `SourceIpConfig` to restrict worker access to tasks to a specific
range of IP addresses. You specify allowed IP addresses by creating a
list of up to ten
[CIDRs](https://docs.aws.amazon.com/vpc/latest/userguide/how-it-works.html).
By default, a workforce isn't restricted to specific IP addresses. If
you specify a range of IP addresses, workers who attempt to access tasks
using any IP address outside the specified range are denied and get a
`⁠Not Found⁠` error message on the worker portal.

To restrict access to all the workers in public internet, add the
`SourceIpConfig` CIDR value as "10.0.0.0/16".

Amazon SageMaker does not support Source Ip restriction for worker
portals in VPC.

Use `OidcConfig` to update the configuration of a workforce created
using your own OIDC IdP.

You can only update your OIDC IdP configuration when there are no work
teams associated with your workforce. You can delete work teams using
the `delete_workteam` operation.

After restricting access to a range of IP addresses or updating your
OIDC IdP configuration with this operation, you can view details about
your update workforce using the `describe_workforce` operation.

This operation only applies to private workforces.

### Usage

    sagemaker_update_workforce(WorkforceName, SourceIpConfig, OidcConfig,
      WorkforceVpcConfig)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="sagemaker_update_workforce_:_WorkforceName">WorkforceName</code></td>
<td><p>[required] The name of the private workforce that you want to
update. You can find your workforce name by using the
<code>list_workforces</code> operation.</p></td>
</tr>
<tr class="even">
<td><code
id="sagemaker_update_workforce_:_SourceIpConfig">SourceIpConfig</code></td>
<td><p>A list of one to ten worker IP address ranges (<a
href="https://docs.aws.amazon.com/vpc/latest/userguide/how-it-works.html">CIDRs</a>)
that can be used to access tasks assigned to this workforce.</p>
<p>Maximum: Ten CIDR values</p></td>
</tr>
<tr class="odd">
<td><code
id="sagemaker_update_workforce_:_OidcConfig">OidcConfig</code></td>
<td><p>Use this parameter to update your OIDC Identity Provider (IdP)
configuration for a workforce made using your own IdP.</p></td>
</tr>
<tr class="even">
<td><code
id="sagemaker_update_workforce_:_WorkforceVpcConfig">WorkforceVpcConfig</code></td>
<td><p>Use this parameter to update your VPC configuration for a
workforce.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Workforce = list(
        WorkforceName = "string",
        WorkforceArn = "string",
        LastUpdatedDate = as.POSIXct(
          "2015-01-01"
        ),
        SourceIpConfig = list(
          Cidrs = list(
            "string"
          )
        ),
        SubDomain = "string",
        CognitoConfig = list(
          UserPool = "string",
          ClientId = "string"
        ),
        OidcConfig = list(
          ClientId = "string",
          Issuer = "string",
          AuthorizationEndpoint = "string",
          TokenEndpoint = "string",
          UserInfoEndpoint = "string",
          LogoutEndpoint = "string",
          JwksUri = "string"
        ),
        CreateDate = as.POSIXct(
          "2015-01-01"
        ),
        WorkforceVpcConfig = list(
          VpcId = "string",
          SecurityGroupIds = list(
            "string"
          ),
          Subnets = list(
            "string"
          ),
          VpcEndpointId = "string"
        ),
        Status = "Initializing"|"Updating"|"Deleting"|"Failed"|"Active",
        FailureReason = "string"
      )
    )

### Request syntax

    svc$update_workforce(
      WorkforceName = "string",
      SourceIpConfig = list(
        Cidrs = list(
          "string"
        )
      ),
      OidcConfig = list(
        ClientId = "string",
        ClientSecret = "string",
        Issuer = "string",
        AuthorizationEndpoint = "string",
        TokenEndpoint = "string",
        UserInfoEndpoint = "string",
        LogoutEndpoint = "string",
        JwksUri = "string"
      ),
      WorkforceVpcConfig = list(
        VpcId = "string",
        SecurityGroupIds = list(
          "string"
        ),
        Subnets = list(
          "string"
        )
      )
    )