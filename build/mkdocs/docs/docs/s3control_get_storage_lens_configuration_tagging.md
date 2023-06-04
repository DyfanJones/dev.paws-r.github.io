<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>s3control_get_storage_lens_configuration_tagging</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets the tags of Amazon S3 Storage Lens configuration

### Description

Gets the tags of Amazon S3 Storage Lens configuration. For more
information about S3 Storage Lens, see [Assessing your storage activity
and usage with Amazon S3 Storage
Lens](https://docs.aws.amazon.com/AmazonS3/latest/userguide/storage_lens.html)
in the *Amazon S3 User Guide*.

To use this action, you must have permission to perform the
`s3:GetStorageLensConfigurationTagging` action. For more information,
see [Setting permissions to use Amazon S3 Storage
Lens](https://docs.aws.amazon.com/AmazonS3/latest/userguide/storage_lens_iam_permissions.html)
in the *Amazon S3 User Guide*.

### Usage

    s3control_get_storage_lens_configuration_tagging(ConfigId, AccountId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="s3control_get_storage_lens_configuration_tagging_:_ConfigId">ConfigId</code></td>
<td><p>[required] The ID of the Amazon S3 Storage Lens
configuration.</p></td>
</tr>
<tr class="even">
<td><code
id="s3control_get_storage_lens_configuration_tagging_:_AccountId">AccountId</code></td>
<td><p>[required] The account ID of the requester.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Tags = list(
        list(
          Key = "string",
          Value = "string"
        )
      )
    )

### Request syntax

    svc$get_storage_lens_configuration_tagging(
      ConfigId = "string",
      AccountId = "string"
    )