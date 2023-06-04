<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>glacier_initiate_job</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## This operation initiates a job of the specified type, which can be a select, an archival retrieval, or a vault retrieval

### Description

This operation initiates a job of the specified type, which can be a
select, an archival retrieval, or a vault retrieval. For more
information about using this operation, see the documentation for the
underlying REST API [Initiate a
Job](https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html).

### Usage

    glacier_initiate_job(accountId, vaultName, jobParameters)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="glacier_initiate_job_:_accountId">accountId</code></td>
<td><p>[required] The <code>AccountId</code> value is the AWS account ID
of the account that owns the vault. You can either specify an AWS
account ID or optionally a single '<code>-</code>' (hyphen), in which
case Amazon S3 Glacier uses the AWS account ID associated with the
credentials used to sign the request. If you use an account ID, do not
include any hyphens ('-') in the ID.</p></td>
</tr>
<tr class="even">
<td><code id="glacier_initiate_job_:_vaultName">vaultName</code></td>
<td><p>[required] The name of the vault.</p></td>
</tr>
<tr class="odd">
<td><code
id="glacier_initiate_job_:_jobParameters">jobParameters</code></td>
<td><p>Provides options for specifying job information.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      location = "string",
      jobId = "string",
      jobOutputPath = "string"
    )

### Request syntax

    svc$initiate_job(
      accountId = "string",
      vaultName = "string",
      jobParameters = list(
        Format = "string",
        Type = "string",
        ArchiveId = "string",
        Description = "string",
        SNSTopic = "string",
        RetrievalByteRange = "string",
        Tier = "string",
        InventoryRetrievalParameters = list(
          StartDate = "string",
          EndDate = "string",
          Limit = "string",
          Marker = "string"
        ),
        SelectParameters = list(
          InputSerialization = list(
            csv = list(
              FileHeaderInfo = "USE"|"IGNORE"|"NONE",
              Comments = "string",
              QuoteEscapeCharacter = "string",
              RecordDelimiter = "string",
              FieldDelimiter = "string",
              QuoteCharacter = "string"
            )
          ),
          ExpressionType = "SQL",
          Expression = "string",
          OutputSerialization = list(
            csv = list(
              QuoteFields = "ALWAYS"|"ASNEEDED",
              QuoteEscapeCharacter = "string",
              RecordDelimiter = "string",
              FieldDelimiter = "string",
              QuoteCharacter = "string"
            )
          )
        ),
        OutputLocation = list(
          S3 = list(
            BucketName = "string",
            Prefix = "string",
            Encryption = list(
              EncryptionType = "aws:kms"|"AES256",
              KMSKeyId = "string",
              KMSContext = "string"
            ),
            CannedACL = "private"|"public-read"|"public-read-write"|"aws-exec-read"|"authenticated-read"|"bucket-owner-read"|"bucket-owner-full-control",
            AccessControlList = list(
              list(
                Grantee = list(
                  Type = "AmazonCustomerByEmail"|"CanonicalUser"|"Group",
                  DisplayName = "string",
                  URI = "string",
                  ID = "string",
                  EmailAddress = "string"
                ),
                Permission = "FULL_CONTROL"|"WRITE"|"WRITE_ACP"|"READ"|"READ_ACP"
              )
            ),
            Tagging = list(
              "string"
            ),
            UserMetadata = list(
              "string"
            ),
            StorageClass = "STANDARD"|"REDUCED_REDUNDANCY"|"STANDARD_IA"
          )
        )
      )
    )

### Examples

    ## Not run: 
    # The example initiates an inventory-retrieval job for the vault named
    # examplevault.
    svc$initiate_job(
      accountId = "-",
      jobParameters = list(
        Description = "My inventory job",
        Format = "CSV",
        SNSTopic = "arn:aws:sns:us-west-2:111111111111:Glacier-InventoryRetrieval-topic-Example",
        Type = "inventory-retrieval"
      ),
      vaultName = "examplevault"
    )

    ## End(Not run)