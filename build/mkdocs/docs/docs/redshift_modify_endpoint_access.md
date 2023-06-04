<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>redshift_modify_endpoint_access</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Modifies a Redshift-managed VPC endpoint

### Description

Modifies a Redshift-managed VPC endpoint.

### Usage

    redshift_modify_endpoint_access(EndpointName, VpcSecurityGroupIds)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="redshift_modify_endpoint_access_:_EndpointName">EndpointName</code></td>
<td><p>[required] The endpoint to be modified.</p></td>
</tr>
<tr class="even">
<td><code
id="redshift_modify_endpoint_access_:_VpcSecurityGroupIds">VpcSecurityGroupIds</code></td>
<td><p>The complete list of VPC security groups associated with the
endpoint after the endpoint is modified.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ClusterIdentifier = "string",
      ResourceOwner = "string",
      SubnetGroupName = "string",
      EndpointStatus = "string",
      EndpointName = "string",
      EndpointCreateTime = as.POSIXct(
        "2015-01-01"
      ),
      Port = 123,
      Address = "string",
      VpcSecurityGroups = list(
        list(
          VpcSecurityGroupId = "string",
          Status = "string"
        )
      ),
      VpcEndpoint = list(
        VpcEndpointId = "string",
        VpcId = "string",
        NetworkInterfaces = list(
          list(
            NetworkInterfaceId = "string",
            SubnetId = "string",
            PrivateIpAddress = "string",
            AvailabilityZone = "string"
          )
        )
      )
    )

### Request syntax

    svc$modify_endpoint_access(
      EndpointName = "string",
      VpcSecurityGroupIds = list(
        "string"
      )
    )