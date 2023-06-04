<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ses_delete_configuration_set_event_destination</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Deletes a configuration set event destination

### Description

Deletes a configuration set event destination. Configuration set event
destinations are associated with configuration sets, which enable you to
publish email sending events. For information about using configuration
sets, see the [Amazon SES Developer
Guide](https://docs.aws.amazon.com/ses/latest/dg/monitor-sending-activity.html).

You can execute this operation no more than once per second.

### Usage

    ses_delete_configuration_set_event_destination(ConfigurationSetName,
      EventDestinationName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ses_delete_configuration_set_event_destination_:_ConfigurationSetName">ConfigurationSetName</code></td>
<td><p>[required] The name of the configuration set from which to delete
the event destination.</p></td>
</tr>
<tr class="even">
<td><code
id="ses_delete_configuration_set_event_destination_:_EventDestinationName">EventDestinationName</code></td>
<td><p>[required] The name of the event destination to delete.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$delete_configuration_set_event_destination(
      ConfigurationSetName = "string",
      EventDestinationName = "string"
    )