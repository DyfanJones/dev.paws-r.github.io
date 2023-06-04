<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>appstream_describe_images</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves a list that describes one or more specified images, if the image names or image ARNs are provided

### Description

Retrieves a list that describes one or more specified images, if the
image names or image ARNs are provided. Otherwise, all images in the
account are described.

### Usage

    appstream_describe_images(Names, Arns, Type, NextToken, MaxResults)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="appstream_describe_images_:_Names">Names</code></td>
<td><p>The names of the public or private images to describe.</p></td>
</tr>
<tr class="even">
<td><code id="appstream_describe_images_:_Arns">Arns</code></td>
<td><p>The ARNs of the public, private, and shared images to
describe.</p></td>
</tr>
<tr class="odd">
<td><code id="appstream_describe_images_:_Type">Type</code></td>
<td><p>The type of image (public, private, or shared) to
describe.</p></td>
</tr>
<tr class="even">
<td><code
id="appstream_describe_images_:_NextToken">NextToken</code></td>
<td><p>The pagination token to use to retrieve the next page of results
for this operation. If this value is null, it retrieves the first
page.</p></td>
</tr>
<tr class="odd">
<td><code
id="appstream_describe_images_:_MaxResults">MaxResults</code></td>
<td><p>The maximum size of each page of results.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Images = list(
        list(
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
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$describe_images(
      Names = list(
        "string"
      ),
      Arns = list(
        "string"
      ),
      Type = "PUBLIC"|"PRIVATE"|"SHARED",
      NextToken = "string",
      MaxResults = 123
    )