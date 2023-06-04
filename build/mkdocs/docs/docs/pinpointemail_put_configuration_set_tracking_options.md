<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>pinpointemail_put_configuration_set_tracking_options</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Specify a custom domain to use for open and click tracking elements in email that you send using Amazon Pinpoint

### Description

Specify a custom domain to use for open and click tracking elements in
email that you send using Amazon Pinpoint.

### Usage

    pinpointemail_put_configuration_set_tracking_options(
      ConfigurationSetName, CustomRedirectDomain)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="pinpointemail_put_configuration_set_tracking_options_:_ConfigurationSetName">ConfigurationSetName</code></td>
<td><p>[required] The name of the configuration set that you want to add
a custom tracking domain to.</p></td>
</tr>
<tr class="even">
<td><code
id="pinpointemail_put_configuration_set_tracking_options_:_CustomRedirectDomain">CustomRedirectDomain</code></td>
<td><p>The domain that you want to use to track open and click
events.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$put_configuration_set_tracking_options(
      ConfigurationSetName = "string",
      CustomRedirectDomain = "string"
    )