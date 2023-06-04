<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>appstream_create_updated_image</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates a new image with the latest Windows operating system updates, driver updates, and AppStream 2

### Description

Creates a new image with the latest Windows operating system updates,
driver updates, and AppStream 2.0 agent software.

For more information, see the "Update an Image by Using Managed
AppStream 2.0 Image Updates" section in [Administer Your AppStream 2.0
Images](https://docs.aws.amazon.com/appstream2/latest/developerguide/administer-images.html),
in the *Amazon AppStream 2.0 Administration Guide*.

### Usage

    appstream_create_updated_image(existingImageName, newImageName,
      newImageDescription, newImageDisplayName, newImageTags, dryRun)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="appstream_create_updated_image_:_existingImageName">existingImageName</code></td>
<td><p>[required] The name of the image to update.</p></td>
</tr>
<tr class="even">
<td><code
id="appstream_create_updated_image_:_newImageName">newImageName</code></td>
<td><p>[required] The name of the new image. The name must be unique
within the AWS account and Region.</p></td>
</tr>
<tr class="odd">
<td><code
id="appstream_create_updated_image_:_newImageDescription">newImageDescription</code></td>
<td><p>The description to display for the new image.</p></td>
</tr>
<tr class="even">
<td><code
id="appstream_create_updated_image_:_newImageDisplayName">newImageDisplayName</code></td>
<td><p>The name to display for the new image.</p></td>
</tr>
<tr class="odd">
<td><code
id="appstream_create_updated_image_:_newImageTags">newImageTags</code></td>
<td><p>The tags to associate with the new image. A tag is a key-value
pair, and the value is optional. For example, Environment=Test. If you
do not specify a value, Environment=.</p>
<p>Generally allowed characters are: letters, numbers, and spaces
representable in UTF-8, and the following special characters:</p>
<p>_ . : / = + \ - @</p>
<p>If you do not specify a value, the value is set to an empty
string.</p>
<p>For more information about tags, see <a
href="https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html">Tagging
Your Resources</a> in the <em>Amazon AppStream 2.0 Administration
Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code
id="appstream_create_updated_image_:_dryRun">dryRun</code></td>
<td><p>Indicates whether to display the status of image update
availability before AppStream 2.0 initiates the process of creating a
new updated image. If this value is set to <code>true</code>, AppStream
2.0 displays whether image updates are available. If this value is set
to <code>false</code>, AppStream 2.0 initiates the process of creating a
new updated image without displaying whether image updates are
available.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      image = list(
        Name = "string",
        Arn = "string",
        BaseImageArn = "string",
        DisplayName = "string",
        State = "PENDING"|"AVAILABLE"|"FAILED"|"COPYING"|"DELETING"|"CREATING"|"IMPORTING",
        Visibility = "PUBLIC"|"PRIVATE"|"SHARED",
        ImageBuilderSupported = TRUE|FALSE,
        ImageBuilderName = "string",
        Platform = "WINDOWS"|"WINDOWS_SERVER_2016"|"WINDOWS_SERVER_2019"|"AMAZON_LINUX2",
        Description = "string",
        StateChangeReason = list(
          Code = "INTERNAL_ERROR"|"IMAGE_BUILDER_NOT_AVAILABLE"|"IMAGE_COPY_FAILURE",
          Message = "string"
        ),
        Applications = list(
          list(
            Name = "string",
            DisplayName = "string",
            IconURL = "string",
            LaunchPath = "string",
            LaunchParameters = "string",
            Enabled = TRUE|FALSE,
            Metadata = list(
              "string"
            ),
            WorkingDirectory = "string",
            Description = "string",
            Arn = "string",
            AppBlockArn = "string",
            IconS3Location = list(
              S3Bucket = "string",
              S3Key = "string"
            ),
            Platforms = list(
              "WINDOWS"|"WINDOWS_SERVER_2016"|"WINDOWS_SERVER_2019"|"AMAZON_LINUX2"
            ),
            InstanceFamilies = list(
              "string"
            ),
            CreatedTime = as.POSIXct(
              "2015-01-01"
            )
          )
        ),
        CreatedTime = as.POSIXct(
          "2015-01-01"
        ),
        PublicBaseImageReleasedDate = as.POSIXct(
          "2015-01-01"
        ),
        AppstreamAgentVersion = "string",
        ImagePermissions = list(
          allowFleet = TRUE|FALSE,
          allowImageBuilder = TRUE|FALSE
        ),
        ImageErrors = list(
          list(
            ErrorCode = "IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION"|"IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION"|"IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION"|"NETWORK_INTERFACE_LIMIT_EXCEEDED"|"INTERNAL_SERVICE_ERROR"|"IAM_SERVICE_ROLE_IS_MISSING"|"MACHINE_ROLE_IS_MISSING"|"STS_DISABLED_IN_REGION"|"SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES"|"IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION"|"SUBNET_NOT_FOUND"|"IMAGE_NOT_FOUND"|"INVALID_SUBNET_CONFIGURATION"|"SECURITY_GROUPS_NOT_FOUND"|"IGW_NOT_ATTACHED"|"IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION"|"FLEET_STOPPED"|"FLEET_INSTANCE_PROVISIONING_FAILURE"|"DOMAIN_JOIN_ERROR_FILE_NOT_FOUND"|"DOMAIN_JOIN_ERROR_ACCESS_DENIED"|"DOMAIN_JOIN_ERROR_LOGON_FAILURE"|"DOMAIN_JOIN_ERROR_INVALID_PARAMETER"|"DOMAIN_JOIN_ERROR_MORE_DATA"|"DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN"|"DOMAIN_JOIN_ERROR_NOT_SUPPORTED"|"DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME"|"DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED"|"DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED"|"DOMAIN_JOIN_NERR_PASSWORD_EXPIRED"|"DOMAIN_JOIN_INTERNAL_SERVICE_ERROR",
            ErrorMessage = "string",
            ErrorTimestamp = as.POSIXct(
              "2015-01-01"
            )
          )
        )
      ),
      canUpdateImage = TRUE|FALSE
    )

### Request syntax

    svc$create_updated_image(
      existingImageName = "string",
      newImageName = "string",
      newImageDescription = "string",
      newImageDisplayName = "string",
      newImageTags = list(
        "string"
      ),
      dryRun = TRUE|FALSE
    )