<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ec2_attach_internet_gateway</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Attaches an internet gateway or a virtual private gateway to a VPC, enabling connectivity between the internet and the VPC

### Description

Attaches an internet gateway or a virtual private gateway to a VPC,
enabling connectivity between the internet and the VPC. For more
information about your VPC and internet gateway, see the [Amazon Virtual
Private Cloud User
Guide](https://docs.aws.amazon.com/vpc/latest/userguide/).

### Usage

    ec2_attach_internet_gateway(DryRun, InternetGatewayId, VpcId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="ec2_attach_internet_gateway_:_DryRun">DryRun</code></td>
<td><p>Checks whether you have the required permissions for the action,
without actually making the request, and provides an error response. If
you have the required permissions, the error response is
<code>DryRunOperation</code>. Otherwise, it is
<code>UnauthorizedOperation</code>.</p></td>
</tr>
<tr class="even">
<td><code
id="ec2_attach_internet_gateway_:_InternetGatewayId">InternetGatewayId</code></td>
<td><p>[required] The ID of the internet gateway.</p></td>
</tr>
<tr class="odd">
<td><code id="ec2_attach_internet_gateway_:_VpcId">VpcId</code></td>
<td><p>[required] The ID of the VPC.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$attach_internet_gateway(
      DryRun = TRUE|FALSE,
      InternetGatewayId = "string",
      VpcId = "string"
    )

### Examples

    ## Not run: 
    # This example attaches the specified Internet gateway to the specified
    # VPC.
    svc$attach_internet_gateway(
      InternetGatewayId = "igw-c0a643a9",
      VpcId = "vpc-a01106c2"
    )

    ## End(Not run)