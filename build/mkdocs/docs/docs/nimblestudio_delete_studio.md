<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>nimblestudio_delete_studio</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Delete a studio resource

### Description

Delete a studio resource.

### Usage

    nimblestudio_delete_studio(clientToken, studioId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="nimblestudio_delete_studio_:_clientToken">clientToken</code></td>
<td><p>Unique, case-sensitive identifier that you provide to ensure the
idempotency of the request. If you don’t specify a client token, the
Amazon Web Services SDK automatically generates a client token and uses
it for the request to ensure idempotency.</p></td>
</tr>
<tr class="even">
<td><code
id="nimblestudio_delete_studio_:_studioId">studioId</code></td>
<td><p>[required] The studio ID.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      studio = list(
        adminRoleArn = "string",
        arn = "string",
        createdAt = as.POSIXct(
          "2015-01-01"
        ),
        displayName = "string",
        homeRegion = "string",
        ssoClientId = "string",
        state = "CREATE_IN_PROGRESS"|"READY"|"UPDATE_IN_PROGRESS"|"DELETE_IN_PROGRESS"|"DELETED"|"DELETE_FAILED"|"CREATE_FAILED"|"UPDATE_FAILED",
        statusCode = "STUDIO_CREATED"|"STUDIO_DELETED"|"STUDIO_UPDATED"|"STUDIO_CREATE_IN_PROGRESS"|"STUDIO_UPDATE_IN_PROGRESS"|"STUDIO_DELETE_IN_PROGRESS"|"STUDIO_WITH_LAUNCH_PROFILES_NOT_DELETED"|"STUDIO_WITH_STUDIO_COMPONENTS_NOT_DELETED"|"STUDIO_WITH_STREAMING_IMAGES_NOT_DELETED"|"AWS_SSO_NOT_ENABLED"|"AWS_SSO_ACCESS_DENIED"|"ROLE_NOT_OWNED_BY_STUDIO_OWNER"|"ROLE_COULD_NOT_BE_ASSUMED"|"INTERNAL_ERROR"|"ENCRYPTION_KEY_NOT_FOUND"|"ENCRYPTION_KEY_ACCESS_DENIED"|"AWS_SSO_CONFIGURATION_REPAIRED"|"AWS_SSO_CONFIGURATION_REPAIR_IN_PROGRESS"|"AWS_STS_REGION_DISABLED",
        statusMessage = "string",
        studioEncryptionConfiguration = list(
          keyArn = "string",
          keyType = "AWS_OWNED_KEY"|"CUSTOMER_MANAGED_KEY"
        ),
        studioId = "string",
        studioName = "string",
        studioUrl = "string",
        tags = list(
          "string"
        ),
        updatedAt = as.POSIXct(
          "2015-01-01"
        ),
        userRoleArn = "string"
      )
    )

### Request syntax

    svc$delete_studio(
      clientToken = "string",
      studioId = "string"
    )