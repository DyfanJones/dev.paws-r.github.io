<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>s3_get_object_lock_configuration</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets the Object Lock configuration for a bucket

### Description

Gets the Object Lock configuration for a bucket. The rule specified in
the Object Lock configuration will be applied by default to every new
object placed in the specified bucket. For more information, see
[Locking
Objects](https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-lock.html).

The following action is related to `get_object_lock_configuration`:

-   `get_object_attributes`

### Usage

    s3_get_object_lock_configuration(Bucket, ExpectedBucketOwner)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="s3_get_object_lock_configuration_:_Bucket">Bucket</code></td>
<td><p>[required] The bucket whose Object Lock configuration you want to
retrieve.</p>
<p>When using this action with an access point, you must direct requests
to the access point hostname. The access point hostname takes the form
<em>AccessPointName</em>-<em>AccountId</em>.s3-accesspoint.<em>Region</em>.amazonaws.com.
When using this action with an access point through the Amazon Web
Services SDKs, you provide the access point ARN in place of the bucket
name. For more information about access point ARNs, see <a
href="https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html">Using
access points</a> in the <em>Amazon S3 User Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code
id="s3_get_object_lock_configuration_:_ExpectedBucketOwner">ExpectedBucketOwner</code></td>
<td><p>The account ID of the expected bucket owner. If the bucket is
owned by a different account, the request fails with the HTTP status
code <code style="white-space: pre;">⁠403 Forbidden⁠</code> (access
denied).</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ObjectLockConfiguration = list(
        ObjectLockEnabled = "Enabled",
        Rule = list(
          DefaultRetention = list(
            Mode = "GOVERNANCE"|"COMPLIANCE",
            Days = 123,
            Years = 123
          )
        )
      )
    )

### Request syntax

    svc$get_object_lock_configuration(
      Bucket = "string",
      ExpectedBucketOwner = "string"
    )