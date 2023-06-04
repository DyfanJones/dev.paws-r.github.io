<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ec2_disable_vgw_route_propagation</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Disables a virtual private gateway (VGW) from propagating routes to a specified route table of a VPC

### Description

Disables a virtual private gateway (VGW) from propagating routes to a
specified route table of a VPC.

### Usage

    ec2_disable_vgw_route_propagation(GatewayId, RouteTableId, DryRun)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ec2_disable_vgw_route_propagation_:_GatewayId">GatewayId</code></td>
<td><p>[required] The ID of the virtual private gateway.</p></td>
</tr>
<tr class="even">
<td><code
id="ec2_disable_vgw_route_propagation_:_RouteTableId">RouteTableId</code></td>
<td><p>[required] The ID of the route table.</p></td>
</tr>
<tr class="odd">
<td><code
id="ec2_disable_vgw_route_propagation_:_DryRun">DryRun</code></td>
<td><p>Checks whether you have the required permissions for the action,
without actually making the request, and provides an error response. If
you have the required permissions, the error response is
<code>DryRunOperation</code>. Otherwise, it is
<code>UnauthorizedOperation</code>.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$disable_vgw_route_propagation(
      GatewayId = "string",
      RouteTableId = "string",
      DryRun = TRUE|FALSE
    )

### Examples

    ## Not run: 
    # This example disables the specified virtual private gateway from
    # propagating static routes to the specified route table.
    svc$disable_vgw_route_propagation(
      GatewayId = "vgw-9a4cacf3",
      RouteTableId = "rtb-22574640"
    )

    ## End(Not run)