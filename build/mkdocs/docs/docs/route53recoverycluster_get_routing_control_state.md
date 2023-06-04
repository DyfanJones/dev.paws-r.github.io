<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>route53recoverycluster_get_routing_control_state</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Get the state for a routing control

### Description

Get the state for a routing control. A routing control is a simple
on/off switch that you can use to route traffic to cells. When a routing
control state is On, traffic flows to a cell. When the state is Off,
traffic does not flow.

Before you can create a routing control, you must first create a
cluster, and then host the control in a control panel on the cluster.
For more information, see [Create routing control
structures](https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.create.html)
in the Amazon Route 53 Application Recovery Controller Developer Guide.
You access one of the endpoints for the cluster to get or update the
routing control state to redirect traffic for your application.

*You must specify Regional endpoints when you work with API cluster
operations to get or update routing control states in Route 53 ARC.*

To see a code example for getting a routing control state, including
accessing Regional cluster endpoints in sequence, see [API
examples](https://docs.aws.amazon.com/r53recovery/latest/dg/service_code_examples_actions.html)
in the Amazon Route 53 Application Recovery Controller Developer Guide.

Learn more about working with routing controls in the following topics
in the Amazon Route 53 Application Recovery Controller Developer Guide:

-   [Viewing and updating routing control
    states](https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.update.html)

-   [Working with routing controls in Route 53
    ARC](https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.html)

### Usage

    route53recoverycluster_get_routing_control_state(RoutingControlArn)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="route53recoverycluster_get_routing_control_state_:_RoutingControlArn">RoutingControlArn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) for the routing control
that you want to get the state for.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      RoutingControlArn = "string",
      RoutingControlState = "On"|"Off",
      RoutingControlName = "string"
    )

### Request syntax

    svc$get_routing_control_state(
      RoutingControlArn = "string"
    )
