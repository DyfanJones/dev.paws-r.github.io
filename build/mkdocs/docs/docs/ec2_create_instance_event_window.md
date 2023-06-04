<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ec2_create_instance_event_window</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates an event window in which scheduled events for the associated Amazon EC2 instances can run

### Description

Creates an event window in which scheduled events for the associated
Amazon EC2 instances can run.

You can define either a set of time ranges or a cron expression when
creating the event window, but not both. All event window times are in
UTC.

You can create up to 200 event windows per Amazon Web Services Region.

When you create the event window, targets (instance IDs, Dedicated Host
IDs, or tags) are not yet associated with it. To ensure that the event
window can be used, you must associate one or more targets with it by
using the `associate_instance_event_window` API.

Event windows are applicable only for scheduled events that stop,
reboot, or terminate instances.

Event windows are *not* applicable for:

-   Expedited scheduled events and network maintenance events.

-   Unscheduled maintenance such as AutoRecovery and unplanned reboots.

For more information, see [Define event windows for scheduled
events](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/event-windows.html)
in the *Amazon EC2 User Guide*.

### Usage

    ec2_create_instance_event_window(DryRun, Name, TimeRanges,
      CronExpression, TagSpecifications)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ec2_create_instance_event_window_:_DryRun">DryRun</code></td>
<td><p>Checks whether you have the required permissions for the action,
without actually making the request, and provides an error response. If
you have the required permissions, the error response is
<code>DryRunOperation</code>. Otherwise, it is
<code>UnauthorizedOperation</code>.</p></td>
</tr>
<tr class="even">
<td><code id="ec2_create_instance_event_window_:_Name">Name</code></td>
<td><p>The name of the event window.</p></td>
</tr>
<tr class="odd">
<td><code
id="ec2_create_instance_event_window_:_TimeRanges">TimeRanges</code></td>
<td><p>The time range for the event window. If you specify a time range,
you can't specify a cron expression.</p></td>
</tr>
<tr class="even">
<td><code
id="ec2_create_instance_event_window_:_CronExpression">CronExpression</code></td>
<td><p>The cron expression for the event window, for example, <code
style="white-space: pre;">⁠* 0-4,20-23 * * 1,5⁠</code>. If you specify a
cron expression, you can't specify a time range.</p>
<p>Constraints:</p>
<ul>
<li><p>Only hour and day of the week values are supported.</p></li>
<li><p>For day of the week values, you can specify either integers
<code>0</code> through <code>6</code>, or alternative single values
<code>SUN</code> through <code>SAT</code>.</p></li>
<li><p>The minute, month, and year must be specified by
<code>*</code>.</p></li>
<li><p>The hour value must be one or a multiple range, for example,
<code>0-4</code> or <code
style="white-space: pre;">⁠0-4,20-23⁠</code>.</p></li>
<li><p>Each hour range must be \&gt;= 2 hours, for example,
<code>0-2</code> or <code>20-23</code>.</p></li>
<li><p>The event window must be \&gt;= 4 hours. The combined total time
ranges in the event window must be \&gt;= 4 hours.</p></li>
</ul>
<p>For more information about cron expressions, see <a
href="https://en.wikipedia.org/wiki/Cron">cron</a> on the <em>Wikipedia
website</em>.</p></td>
</tr>
<tr class="odd">
<td><code
id="ec2_create_instance_event_window_:_TagSpecifications">TagSpecifications</code></td>
<td><p>The tags to apply to the event window.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      InstanceEventWindow = list(
        InstanceEventWindowId = "string",
        TimeRanges = list(
          list(
            StartWeekDay = "sunday"|"monday"|"tuesday"|"wednesday"|"thursday"|"friday"|"saturday",
            StartHour = 123,
            EndWeekDay = "sunday"|"monday"|"tuesday"|"wednesday"|"thursday"|"friday"|"saturday",
            EndHour = 123
          )
        ),
        Name = "string",
        CronExpression = "string",
        AssociationTarget = list(
          InstanceIds = list(
            "string"
          ),
          Tags = list(
            list(
              Key = "string",
              Value = "string"
            )
          ),
          DedicatedHostIds = list(
            "string"
          )
        ),
        State = "creating"|"deleting"|"active"|"deleted",
        Tags = list(
          list(
            Key = "string",
            Value = "string"
          )
        )
      )
    )

### Request syntax

    svc$create_instance_event_window(
      DryRun = TRUE|FALSE,
      Name = "string",
      TimeRanges = list(
        list(
          StartWeekDay = "sunday"|"monday"|"tuesday"|"wednesday"|"thursday"|"friday"|"saturday",
          StartHour = 123,
          EndWeekDay = "sunday"|"monday"|"tuesday"|"wednesday"|"thursday"|"friday"|"saturday",
          EndHour = 123
        )
      ),
      CronExpression = "string",
      TagSpecifications = list(
        list(
          ResourceType = "capacity-reservation"|"client-vpn-endpoint"|"customer-gateway"|"carrier-gateway"|"coip-pool"|"dedicated-host"|"dhcp-options"|"egress-only-internet-gateway"|"elastic-ip"|"elastic-gpu"|"export-image-task"|"export-instance-task"|"fleet"|"fpga-image"|"host-reservation"|"image"|"import-image-task"|"import-snapshot-task"|"instance"|"instance-event-window"|"internet-gateway"|"ipam"|"ipam-pool"|"ipam-scope"|"ipv4pool-ec2"|"ipv6pool-ec2"|"key-pair"|"launch-template"|"local-gateway"|"local-gateway-route-table"|"local-gateway-virtual-interface"|"local-gateway-virtual-interface-group"|"local-gateway-route-table-vpc-association"|"local-gateway-route-table-virtual-interface-group-association"|"natgateway"|"network-acl"|"network-interface"|"network-insights-analysis"|"network-insights-path"|"network-insights-access-scope"|"network-insights-access-scope-analysis"|"placement-group"|"prefix-list"|"replace-root-volume-task"|"reserved-instances"|"route-table"|"security-group"|"security-group-rule"|"snapshot"|"spot-fleet-request"|"spot-instances-request"|"subnet"|"subnet-cidr-reservation"|"traffic-mirror-filter"|"traffic-mirror-session"|"traffic-mirror-target"|"transit-gateway"|"transit-gateway-attachment"|"transit-gateway-connect-peer"|"transit-gateway-multicast-domain"|"transit-gateway-policy-table"|"transit-gateway-route-table"|"transit-gateway-route-table-announcement"|"volume"|"vpc"|"vpc-endpoint"|"vpc-endpoint-connection"|"vpc-endpoint-service"|"vpc-endpoint-service-permission"|"vpc-peering-connection"|"vpn-connection"|"vpn-gateway"|"vpc-flow-log"|"capacity-reservation-fleet"|"traffic-mirror-filter-rule"|"vpc-endpoint-connection-device-type"|"verified-access-instance"|"verified-access-group"|"verified-access-endpoint"|"verified-access-policy"|"verified-access-trust-provider"|"vpn-connection-device-type"|"vpc-block-public-access-exclusion"|"ipam-resource-discovery"|"ipam-resource-discovery-association",
          Tags = list(
            list(
              Key = "string",
              Value = "string"
            )
          )
        )
      )
    )