<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>apprunner_delete_vpc_ingress_connection</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Delete an App Runner VPC Ingress Connection resource that's associated with an App Runner service

### Description

Delete an App Runner VPC Ingress Connection resource that's associated
with an App Runner service. The VPC Ingress Connection must be in one of
the following states to be deleted:

-   `AVAILABLE`

-   `FAILED_CREATION`

-   `FAILED_UPDATE`

-   `FAILED_DELETION`

### Usage

    apprunner_delete_vpc_ingress_connection(VpcIngressConnectionArn)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="apprunner_delete_vpc_ingress_connection_:_VpcIngressConnectionArn">VpcIngressConnectionArn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the App Runner VPC
Ingress Connection that you want to delete.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      VpcIngressConnection = list(
        VpcIngressConnectionArn = "string",
        VpcIngressConnectionName = "string",
        ServiceArn = "string",
        Status = "AVAILABLE"|"PENDING_CREATION"|"PENDING_UPDATE"|"PENDING_DELETION"|"FAILED_CREATION"|"FAILED_UPDATE"|"FAILED_DELETION"|"DELETED",
        AccountId = "string",
        DomainName = "string",
        IngressVpcConfiguration = list(
          VpcId = "string",
          VpcEndpointId = "string"
        ),
        CreatedAt = as.POSIXct(
          "2015-01-01"
        ),
        DeletedAt = as.POSIXct(
          "2015-01-01"
        )
      )
    )

### Request syntax

    svc$delete_vpc_ingress_connection(
      VpcIngressConnectionArn = "string"
    )