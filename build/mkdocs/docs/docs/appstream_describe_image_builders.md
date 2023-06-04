<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>appstream_describe_image_builders</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves a list that describes one or more specified image builders, if the image builder names are provided

### Description

Retrieves a list that describes one or more specified image builders, if
the image builder names are provided. Otherwise, all image builders in
the account are described.

### Usage

    appstream_describe_image_builders(Names, MaxResults, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="appstream_describe_image_builders_:_Names">Names</code></td>
<td><p>The names of the image builders to describe.</p></td>
</tr>
<tr class="even">
<td><code
id="appstream_describe_image_builders_:_MaxResults">MaxResults</code></td>
<td><p>The maximum size of each page of results.</p></td>
</tr>
<tr class="odd">
<td><code
id="appstream_describe_image_builders_:_NextToken">NextToken</code></td>
<td><p>The pagination token to use to retrieve the next page of results
for this operation. If this value is null, it retrieves the first
page.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ImageBuilders = list(
        list(
          Name = "string",
          Arn = "string",
          ImageArn = "string",
          Description = "string",
          DisplayName = "string",
          VpcConfig = list(
            SubnetIds = list(
              "string"
            ),
            SecurityGroupIds = list(
              "string"
            )
          ),
          InstanceType = "string",
          Platform = "WINDOWS"|"WINDOWS_SERVER_2016"|"WINDOWS_SERVER_2019"|"AMAZON_LINUX2",
          IamRoleArn = "string",
          State = "PENDING"|"UPDATING_AGENT"|"RUNNING"|"STOPPING"|"STOPPED"|"REBOOTING"|"SNAPSHOTTING"|"DELETING"|"FAILED"|"UPDATING"|"PENDING_QUALIFICATION",
          StateChangeReason = list(
            Code = "INTERNAL_ERROR"|"IMAGE_UNAVAILABLE",
            Message = "string"
          ),
          CreatedTime = as.POSIXct(
            "2015-01-01"
          ),
          EnableDefaultInternetAccess = TRUE|FALSE,
          DomainJoinInfo = list(
            DirectoryName = "string",
            OrganizationalUnitDistinguishedName = "string"
          ),
          NetworkAccessConfiguration = list(
            EniPrivateIpAddress = "string",
            EniId = "string"
          ),
          ImageBuilderErrors = list(
            list(
              ErrorCode = "IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION"|"IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION"|"IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION"|"NETWORK_INTERFACE_LIMIT_EXCEEDED"|"INTERNAL_SERVICE_ERROR"|"IAM_SERVICE_ROLE_IS_MISSING"|"MACHINE_ROLE_IS_MISSING"|"STS_DISABLED_IN_REGION"|"SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES"|"IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION"|"SUBNET_NOT_FOUND"|"IMAGE_NOT_FOUND"|"INVALID_SUBNET_CONFIGURATION"|"SECURITY_GROUPS_NOT_FOUND"|"IGW_NOT_ATTACHED"|"IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION"|"FLEET_STOPPED"|"FLEET_INSTANCE_PROVISIONING_FAILURE"|"DOMAIN_JOIN_ERROR_FILE_NOT_FOUND"|"DOMAIN_JOIN_ERROR_ACCESS_DENIED"|"DOMAIN_JOIN_ERROR_LOGON_FAILURE"|"DOMAIN_JOIN_ERROR_INVALID_PARAMETER"|"DOMAIN_JOIN_ERROR_MORE_DATA"|"DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN"|"DOMAIN_JOIN_ERROR_NOT_SUPPORTED"|"DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME"|"DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED"|"DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED"|"DOMAIN_JOIN_NERR_PASSWORD_EXPIRED"|"DOMAIN_JOIN_INTERNAL_SERVICE_ERROR",
              ErrorMessage = "string",
              ErrorTimestamp = as.POSIXct(
                "2015-01-01"
              )
            )
          ),
          AppstreamAgentVersion = "string",
          AccessEndpoints = list(
            list(
              EndpointType = "STREAMING",
              VpceId = "string"
            )
          )
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$describe_image_builders(
      Names = list(
        "string"
      ),
      MaxResults = 123,
      NextToken = "string"
    )