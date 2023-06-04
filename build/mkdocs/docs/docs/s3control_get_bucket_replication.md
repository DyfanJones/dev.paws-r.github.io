<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>s3control_get_bucket_replication</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## This operation gets an Amazon S3 on Outposts bucket's replication configuration

### Description

This operation gets an Amazon S3 on Outposts bucket's replication
configuration. To get an S3 bucket's replication configuration, see
`get_bucket_replication` in the *Amazon S3 API Reference*.

Returns the replication configuration of an S3 on Outposts bucket. For
more information about S3 on Outposts, see [Using Amazon S3 on
Outposts](https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html)
in the *Amazon S3 User Guide*. For information about S3 replication on
Outposts configuration, see [Replicating objects for S3 on
Outposts](https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3OutpostsReplication.html)
in the *Amazon S3 User Guide*.

It can take a while to propagate `PUT` or `DELETE` requests for a
replication configuration to all S3 on Outposts systems. Therefore, the
replication configuration that's returned by a `GET` request soon after
a `PUT` or `DELETE` request might return a more recent result than
what's on the Outpost. If an Outpost is offline, the delay in updating
the replication configuration on that Outpost can be significant.

This action requires permissions for the
`s3-outposts:GetReplicationConfiguration` action. The Outposts bucket
owner has this permission by default and can grant it to others. For
more information about permissions, see [Setting up IAM with S3 on
Outposts](https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3OutpostsIAM.html)
and [Managing access to S3 on Outposts
bucket](https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3OutpostsBucketPolicy.html)
in the *Amazon S3 User Guide*.

All Amazon S3 on Outposts REST API requests for this action require an
additional parameter of `x-amz-outpost-id` to be passed with the
request. In addition, you must use an S3 on Outposts endpoint hostname
prefix instead of `s3-control`. For an example of the request syntax for
Amazon S3 on Outposts that uses the S3 on Outposts endpoint hostname
prefix and the `x-amz-outpost-id` derived by using the access point ARN,
see the
[Examples](https://docs.aws.amazon.com/AmazonS3/latest/API/API_control_GetBucketReplication.html#API_control_GetBucketReplication_Examples)
section.

If you include the `Filter` element in a replication configuration, you
must also include the `DeleteMarkerReplication`, `Status`, and
`Priority` elements. The response also returns those elements.

For information about S3 on Outposts replication failure reasons, see
[Replication failure
reasons](https://docs.aws.amazon.com/AmazonS3/latest/userguide/outposts-replication-eventbridge.html#outposts-replication-failure-codes)
in the *Amazon S3 User Guide*.

The following operations are related to `get_bucket_replication`:

-   `put_bucket_replication`

-   `delete_bucket_replication`

### Usage

    s3control_get_bucket_replication(AccountId, Bucket)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="s3control_get_bucket_replication_:_AccountId">AccountId</code></td>
<td><p>[required] The Amazon Web Services account ID of the Outposts
bucket.</p></td>
</tr>
<tr class="even">
<td><code
id="s3control_get_bucket_replication_:_Bucket">Bucket</code></td>
<td><p>[required] Specifies the bucket to get the replication
information for.</p>
<p>For using this parameter with Amazon S3 on Outposts with the REST
API, you must specify the name and the x-amz-outpost-id as well.</p>
<p>For using this parameter with S3 on Outposts with the Amazon Web
Services SDK and CLI, you must specify the ARN of the bucket accessed in
the format <code
style="white-space: pre;">⁠arn:aws:s3-outposts:&lt;Region&gt;:&lt;account-id&gt;:outpost/&lt;outpost-id&gt;/bucket/&lt;my-bucket-name&gt;⁠</code>.
For example, to access the bucket <code>reports</code> through Outpost
<code>my-outpost</code> owned by account <code>123456789012</code> in
Region <code>us-west-2</code>, use the URL encoding of
<code>arn:aws:s3-outposts:us-west-2:123456789012:outpost/my-outpost/bucket/reports</code>.
The value must be URL encoded.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ReplicationConfiguration = list(
        Role = "string",
        Rules = list(
          list(
            ID = "string",
            Priority = 123,
            Prefix = "string",
            Filter = list(
              Prefix = "string",
              Tag = list(
                Key = "string",
                Value = "string"
              ),
              And = list(
                Prefix = "string",
                Tags = list(
                  list(
                    Key = "string",
                    Value = "string"
                  )
                )
              )
            ),
            Status = "Enabled"|"Disabled",
            SourceSelectionCriteria = list(
              SseKmsEncryptedObjects = list(
                Status = "Enabled"|"Disabled"
              ),
              ReplicaModifications = list(
                Status = "Enabled"|"Disabled"
              )
            ),
            ExistingObjectReplication = list(
              Status = "Enabled"|"Disabled"
            ),
            Destination = list(
              Account = "string",
              Bucket = "string",
              ReplicationTime = list(
                Status = "Enabled"|"Disabled",
                Time = list(
                  Minutes = 123
                )
              ),
              AccessControlTranslation = list(
                Owner = "Destination"
              ),
              EncryptionConfiguration = list(
                ReplicaKmsKeyID = "string"
              ),
              Metrics = list(
                Status = "Enabled"|"Disabled",
                EventThreshold = list(
                  Minutes = 123
                )
              ),
              StorageClass = "STANDARD"|"REDUCED_REDUNDANCY"|"STANDARD_IA"|"ONEZONE_IA"|"INTELLIGENT_TIERING"|"GLACIER"|"DEEP_ARCHIVE"|"OUTPOSTS"|"GLACIER_IR"
            ),
            DeleteMarkerReplication = list(
              Status = "Enabled"|"Disabled"
            ),
            Bucket = "string"
          )
        )
      )
    )

### Request syntax

    svc$get_bucket_replication(
      AccountId = "string",
      Bucket = "string"
    )