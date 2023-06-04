<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>s3control_get_access_point_policy</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns the access point policy associated with the specified access point

### Description

Returns the access point policy associated with the specified access
point.

The following actions are related to `get_access_point_policy`:

-   `put_access_point_policy`

-   `delete_access_point_policy`

### Usage

    s3control_get_access_point_policy(AccountId, Name)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="s3control_get_access_point_policy_:_AccountId">AccountId</code></td>
<td><p>[required] The account ID for the account that owns the specified
access point.</p></td>
</tr>
<tr class="even">
<td><code id="s3control_get_access_point_policy_:_Name">Name</code></td>
<td><p>[required] The name of the access point whose policy you want to
retrieve.</p>
<p>For using this parameter with Amazon S3 on Outposts with the REST
API, you must specify the name and the x-amz-outpost-id as well.</p>
<p>For using this parameter with S3 on Outposts with the Amazon Web
Services SDK and CLI, you must specify the ARN of the access point
accessed in the format <code
style="white-space: pre;">⁠arn:aws:s3-outposts:&lt;Region&gt;:&lt;account-id&gt;:outpost/&lt;outpost-id&gt;/accesspoint/&lt;my-accesspoint-name&gt;⁠</code>.
For example, to access the access point <code>reports-ap</code> through
Outpost <code>my-outpost</code> owned by account
<code>123456789012</code> in Region <code>us-west-2</code>, use the URL
encoding of
<code>arn:aws:s3-outposts:us-west-2:123456789012:outpost/my-outpost/accesspoint/reports-ap</code>.
The value must be URL encoded.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Policy = "string"
    )

### Request syntax

    svc$get_access_point_policy(
      AccountId = "string",
      Name = "string"
    )