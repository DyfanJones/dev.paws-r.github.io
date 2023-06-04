<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>redshift_modify_event_subscription</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Modifies an existing Amazon Redshift event notification subscription

### Description

Modifies an existing Amazon Redshift event notification subscription.

### Usage

    redshift_modify_event_subscription(SubscriptionName, SnsTopicArn,
      SourceType, SourceIds, EventCategories, Severity, Enabled)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="redshift_modify_event_subscription_:_SubscriptionName">SubscriptionName</code></td>
<td><p>[required] The name of the modified Amazon Redshift event
notification subscription.</p></td>
</tr>
<tr class="even">
<td><code
id="redshift_modify_event_subscription_:_SnsTopicArn">SnsTopicArn</code></td>
<td><p>The Amazon Resource Name (ARN) of the SNS topic to be used by the
event notification subscription.</p></td>
</tr>
<tr class="odd">
<td><code
id="redshift_modify_event_subscription_:_SourceType">SourceType</code></td>
<td><p>The type of source that will be generating the events. For
example, if you want to be notified of events generated by a cluster,
you would set this parameter to cluster. If this value is not specified,
events are returned for all Amazon Redshift objects in your Amazon Web
Services account. You must specify a source type in order to specify
source IDs.</p>
<p>Valid values: cluster, cluster-parameter-group,
cluster-security-group, cluster-snapshot, and scheduled-action.</p></td>
</tr>
<tr class="even">
<td><code
id="redshift_modify_event_subscription_:_SourceIds">SourceIds</code></td>
<td><p>A list of one or more identifiers of Amazon Redshift source
objects. All of the objects must be of the same type as was specified in
the source type parameter. The event subscription will return only
events generated by the specified objects. If not specified, then events
are returned for all objects within the source type specified.</p>
<p>Example: my-cluster-1, my-cluster-2</p>
<p>Example: my-snapshot-20131010</p></td>
</tr>
<tr class="odd">
<td><code
id="redshift_modify_event_subscription_:_EventCategories">EventCategories</code></td>
<td><p>Specifies the Amazon Redshift event categories to be published by
the event notification subscription.</p>
<p>Values: configuration, management, monitoring, security,
pending</p></td>
</tr>
<tr class="even">
<td><code
id="redshift_modify_event_subscription_:_Severity">Severity</code></td>
<td><p>Specifies the Amazon Redshift event severity to be published by
the event notification subscription.</p>
<p>Values: ERROR, INFO</p></td>
</tr>
<tr class="odd">
<td><code
id="redshift_modify_event_subscription_:_Enabled">Enabled</code></td>
<td><p>A Boolean value indicating if the subscription is enabled.
<code>true</code> indicates the subscription is enabled</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      EventSubscription = list(
        CustomerAwsId = "string",
        CustSubscriptionId = "string",
        SnsTopicArn = "string",
        Status = "string",
        SubscriptionCreationTime = as.POSIXct(
          "2015-01-01"
        ),
        SourceType = "string",
        SourceIdsList = list(
          "string"
        ),
        EventCategoriesList = list(
          "string"
        ),
        Severity = "string",
        Enabled = TRUE|FALSE,
        Tags = list(
          list(
            Key = "string",
            Value = "string"
          )
        )
      )
    )

### Request syntax

    svc$modify_event_subscription(
      SubscriptionName = "string",
      SnsTopicArn = "string",
      SourceType = "string",
      SourceIds = list(
        "string"
      ),
      EventCategories = list(
        "string"
      ),
      Severity = "string",
      Enabled = TRUE|FALSE
    )