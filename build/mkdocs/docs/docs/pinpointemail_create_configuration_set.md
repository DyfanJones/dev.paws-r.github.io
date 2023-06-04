<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>pinpointemail_create_configuration_set</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Create a configuration set

### Description

Create a configuration set. *Configuration sets* are groups of rules
that you can apply to the emails you send using Amazon Pinpoint. You
apply a configuration set to an email by including a reference to the
configuration set in the headers of the email. When you apply a
configuration set to an email, all of the rules in that configuration
set are applied to the email.

### Usage

    pinpointemail_create_configuration_set(ConfigurationSetName,
      TrackingOptions, DeliveryOptions, ReputationOptions, SendingOptions,
      Tags)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="pinpointemail_create_configuration_set_:_ConfigurationSetName">ConfigurationSetName</code></td>
<td><p>[required] The name of the configuration set.</p></td>
</tr>
<tr class="even">
<td><code
id="pinpointemail_create_configuration_set_:_TrackingOptions">TrackingOptions</code></td>
<td><p>An object that defines the open and click tracking options for
emails that you send using the configuration set.</p></td>
</tr>
<tr class="odd">
<td><code
id="pinpointemail_create_configuration_set_:_DeliveryOptions">DeliveryOptions</code></td>
<td><p>An object that defines the dedicated IP pool that is used to send
emails that you send using the configuration set.</p></td>
</tr>
<tr class="even">
<td><code
id="pinpointemail_create_configuration_set_:_ReputationOptions">ReputationOptions</code></td>
<td><p>An object that defines whether or not Amazon Pinpoint collects
reputation metrics for the emails that you send that use the
configuration set.</p></td>
</tr>
<tr class="odd">
<td><code
id="pinpointemail_create_configuration_set_:_SendingOptions">SendingOptions</code></td>
<td><p>An object that defines whether or not Amazon Pinpoint can send
email that you send using the configuration set.</p></td>
</tr>
<tr class="even">
<td><code
id="pinpointemail_create_configuration_set_:_Tags">Tags</code></td>
<td><p>An array of objects that define the tags (keys and values) that
you want to associate with the configuration set.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$create_configuration_set(
      ConfigurationSetName = "string",
      TrackingOptions = list(
        CustomRedirectDomain = "string"
      ),
      DeliveryOptions = list(
        TlsPolicy = "REQUIRE"|"OPTIONAL",
        SendingPoolName = "string"
      ),
      ReputationOptions = list(
        ReputationMetricsEnabled = TRUE|FALSE,
        LastFreshStart = as.POSIXct(
          "2015-01-01"
        )
      ),
      SendingOptions = list(
        SendingEnabled = TRUE|FALSE
      ),
      Tags = list(
        list(
          Key = "string",
          Value = "string"
        )
      )
    )