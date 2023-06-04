<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>rds_copy_db_snapshot</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Copies the specified DB snapshot

### Description

Copies the specified DB snapshot. The source DB snapshot must be in the
`available` state.

You can copy a snapshot from one Amazon Web Services Region to another.
In that case, the Amazon Web Services Region where you call the
`copy_db_snapshot` operation is the destination Amazon Web Services
Region for the DB snapshot copy.

This command doesn't apply to RDS Custom.

For more information about copying snapshots, see [Copying a DB
Snapshot](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopyDBSnapshot)
in the *Amazon RDS User Guide*.

### Usage

    rds_copy_db_snapshot(SourceDBSnapshotIdentifier,
      TargetDBSnapshotIdentifier, KmsKeyId, Tags, CopyTags, PreSignedUrl,
      OptionGroupName, TargetCustomAvailabilityZone, CopyOptionGroup,
      SourceRegion)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="rds_copy_db_snapshot_:_SourceDBSnapshotIdentifier">SourceDBSnapshotIdentifier</code></td>
<td><p>[required] The identifier for the source DB snapshot.</p>
<p>If the source snapshot is in the same Amazon Web Services Region as
the copy, specify a valid DB snapshot identifier. For example, you might
specify <code>rds:mysql-instance1-snapshot-20130805</code>.</p>
<p>If the source snapshot is in a different Amazon Web Services Region
than the copy, specify a valid DB snapshot ARN. For example, you might
specify
<code>arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805</code>.</p>
<p>If you are copying from a shared manual DB snapshot, this parameter
must be the Amazon Resource Name (ARN) of the shared DB snapshot.</p>
<p>If you are copying an encrypted snapshot this parameter must be in
the ARN format for the source Amazon Web Services Region.</p>
<p>Constraints:</p>
<ul>
<li><p>Must specify a valid system snapshot in the "available"
state.</p></li>
</ul>
<p>Example: <code>rds:mydb-2012-04-02-00-01</code></p>
<p>Example:
<code>arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805</code></p></td>
</tr>
<tr class="even">
<td><code
id="rds_copy_db_snapshot_:_TargetDBSnapshotIdentifier">TargetDBSnapshotIdentifier</code></td>
<td><p>[required] The identifier for the copy of the snapshot.</p>
<p>Constraints:</p>
<ul>
<li><p>Can't be null, empty, or blank</p></li>
<li><p>Must contain from 1 to 255 letters, numbers, or hyphens</p></li>
<li><p>First character must be a letter</p></li>
<li><p>Can't end with a hyphen or contain two consecutive
hyphens</p></li>
</ul>
<p>Example: <code>my-db-snapshot</code></p></td>
</tr>
<tr class="odd">
<td><code id="rds_copy_db_snapshot_:_KmsKeyId">KmsKeyId</code></td>
<td><p>The Amazon Web Services KMS key identifier for an encrypted DB
snapshot. The Amazon Web Services KMS key identifier is the key ARN, key
ID, alias ARN, or alias name for the KMS key.</p>
<p>If you copy an encrypted DB snapshot from your Amazon Web Services
account, you can specify a value for this parameter to encrypt the copy
with a new KMS key. If you don't specify a value for this parameter,
then the copy of the DB snapshot is encrypted with the same Amazon Web
Services KMS key as the source DB snapshot.</p>
<p>If you copy an encrypted DB snapshot that is shared from another
Amazon Web Services account, then you must specify a value for this
parameter.</p>
<p>If you specify this parameter when you copy an unencrypted snapshot,
the copy is encrypted.</p>
<p>If you copy an encrypted snapshot to a different Amazon Web Services
Region, then you must specify an Amazon Web Services KMS key identifier
for the destination Amazon Web Services Region. KMS keys are specific to
the Amazon Web Services Region that they are created in, and you can't
use KMS keys from one Amazon Web Services Region in another Amazon Web
Services Region.</p></td>
</tr>
<tr class="even">
<td><code id="rds_copy_db_snapshot_:_Tags">Tags</code></td>
<td></td>
</tr>
<tr class="odd">
<td><code id="rds_copy_db_snapshot_:_CopyTags">CopyTags</code></td>
<td><p>A value that indicates whether to copy all tags from the source
DB snapshot to the target DB snapshot. By default, tags aren't
copied.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_copy_db_snapshot_:_PreSignedUrl">PreSignedUrl</code></td>
<td><p>When you are copying a snapshot from one Amazon Web Services
GovCloud (US) Region to another, the URL that contains a Signature
Version 4 signed request for the <code>copy_db_snapshot</code> API
operation in the source Amazon Web Services Region that contains the
source DB snapshot to copy.</p>
<p>This setting applies only to Amazon Web Services GovCloud (US)
Regions. It's ignored in other Amazon Web Services Regions.</p>
<p>You must specify this parameter when you copy an encrypted DB
snapshot from another Amazon Web Services Region by using the Amazon RDS
API. Don't specify <code>PreSignedUrl</code> when you are copying an
encrypted DB snapshot in the same Amazon Web Services Region.</p>
<p>The presigned URL must be a valid request for the
<code>copy_db_cluster_snapshot</code> API operation that can run in the
source Amazon Web Services Region that contains the encrypted DB cluster
snapshot to copy. The presigned URL request must contain the following
parameter values:</p>
<ul>
<li><p><code>DestinationRegion</code> - The Amazon Web Services Region
that the encrypted DB snapshot is copied to. This Amazon Web Services
Region is the same one where the <code>copy_db_snapshot</code> operation
is called that contains this presigned URL.</p>
<p>For example, if you copy an encrypted DB snapshot from the us-west-2
Amazon Web Services Region to the us-east-1 Amazon Web Services Region,
then you call the <code>copy_db_snapshot</code> operation in the
us-east-1 Amazon Web Services Region and provide a presigned URL that
contains a call to the <code>copy_db_snapshot</code> operation in the
us-west-2 Amazon Web Services Region. For this example, the
<code>DestinationRegion</code> in the presigned URL must be set to the
us-east-1 Amazon Web Services Region.</p></li>
<li><p><code>KmsKeyId</code> - The KMS key identifier for the KMS key to
use to encrypt the copy of the DB snapshot in the destination Amazon Web
Services Region. This is the same identifier for both the
<code>copy_db_snapshot</code> operation that is called in the
destination Amazon Web Services Region, and the operation contained in
the presigned URL.</p></li>
<li><p><code>SourceDBSnapshotIdentifier</code> - The DB snapshot
identifier for the encrypted snapshot to be copied. This identifier must
be in the Amazon Resource Name (ARN) format for the source Amazon Web
Services Region. For example, if you are copying an encrypted DB
snapshot from the us-west-2 Amazon Web Services Region, then your
<code>SourceDBSnapshotIdentifier</code> looks like the following
example:
<code>arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115</code>.</p></li>
</ul>
<p>To learn how to generate a Signature Version 4 signed request, see <a
href="https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html">Authenticating
Requests: Using Query Parameters (Amazon Web Services Signature Version
4)</a> and <a
href="https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-signing.html">Signature
Version 4 Signing Process</a>.</p>
<p>If you are using an Amazon Web Services SDK tool or the CLI, you can
specify <code>SourceRegion</code> (or <code>--source-region</code> for
the CLI) instead of specifying <code>PreSignedUrl</code> manually.
Specifying <code>SourceRegion</code> autogenerates a presigned URL that
is a valid request for the operation that can run in the source Amazon
Web Services Region.</p>
<p>If you supply a value for this operation's <code>SourceRegion</code>
parameter, a pre-signed URL will be calculated on your behalf.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_copy_db_snapshot_:_OptionGroupName">OptionGroupName</code></td>
<td><p>The name of an option group to associate with the copy of the
snapshot.</p>
<p>Specify this option if you are copying a snapshot from one Amazon Web
Services Region to another, and your DB instance uses a nondefault
option group. If your source DB instance uses Transparent Data
Encryption for Oracle or Microsoft SQL Server, you must specify this
option when copying across Amazon Web Services Regions. For more
information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options">Option
group considerations</a> in the <em>Amazon RDS User Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_copy_db_snapshot_:_TargetCustomAvailabilityZone">TargetCustomAvailabilityZone</code></td>
<td><p>The external custom Availability Zone (CAZ) identifier for the
target CAZ.</p>
<p>Example: <code>rds-caz-aiqhTgQv</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_copy_db_snapshot_:_CopyOptionGroup">CopyOptionGroup</code></td>
<td><p>A value that indicates whether to copy the DB option group
associated with the source DB snapshot to the target Amazon Web Services
account and associate with the target DB snapshot. The associated option
group can be copied only with cross-account snapshot copy
calls.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_copy_db_snapshot_:_SourceRegion">SourceRegion</code></td>
<td><p>The ID of the region that contains the snapshot to be
copied.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      DBSnapshot = list(
        DBSnapshotIdentifier = "string",
        DBInstanceIdentifier = "string",
        SnapshotCreateTime = as.POSIXct(
          "2015-01-01"
        ),
        Engine = "string",
        AllocatedStorage = 123,
        Status = "string",
        Port = 123,
        AvailabilityZone = "string",
        VpcId = "string",
        InstanceCreateTime = as.POSIXct(
          "2015-01-01"
        ),
        MasterUsername = "string",
        EngineVersion = "string",
        LicenseModel = "string",
        SnapshotType = "string",
        Iops = 123,
        OptionGroupName = "string",
        PercentProgress = 123,
        SourceRegion = "string",
        SourceDBSnapshotIdentifier = "string",
        StorageType = "string",
        TdeCredentialArn = "string",
        Encrypted = TRUE|FALSE,
        KmsKeyId = "string",
        DBSnapshotArn = "string",
        Timezone = "string",
        IAMDatabaseAuthenticationEnabled = TRUE|FALSE,
        ProcessorFeatures = list(
          list(
            Name = "string",
            Value = "string"
          )
        ),
        DbiResourceId = "string",
        TagList = list(
          list(
            Key = "string",
            Value = "string"
          )
        ),
        OriginalSnapshotCreateTime = as.POSIXct(
          "2015-01-01"
        ),
        SnapshotDatabaseTime = as.POSIXct(
          "2015-01-01"
        ),
        SnapshotTarget = "string",
        StorageThroughput = 123
      )
    )

### Request syntax

    svc$copy_db_snapshot(
      SourceDBSnapshotIdentifier = "string",
      TargetDBSnapshotIdentifier = "string",
      KmsKeyId = "string",
      Tags = list(
        list(
          Key = "string",
          Value = "string"
        )
      ),
      CopyTags = TRUE|FALSE,
      PreSignedUrl = "string",
      OptionGroupName = "string",
      TargetCustomAvailabilityZone = "string",
      CopyOptionGroup = TRUE|FALSE,
      SourceRegion = "string"
    )