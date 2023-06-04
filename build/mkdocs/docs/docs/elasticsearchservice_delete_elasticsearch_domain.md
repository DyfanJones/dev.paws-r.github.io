<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>elasticsearchservice_delete_elasticsearch_domain</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Permanently deletes the specified Elasticsearch domain and all of its data

### Description

Permanently deletes the specified Elasticsearch domain and all of its
data. Once a domain is deleted, it cannot be recovered.

### Usage

    elasticsearchservice_delete_elasticsearch_domain(DomainName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="elasticsearchservice_delete_elasticsearch_domain_:_DomainName">DomainName</code></td>
<td><p>[required] The name of the Elasticsearch domain that you want to
permanently delete.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      DomainStatus = list(
        DomainId = "string",
        DomainName = "string",
        ARN = "string",
        Created = TRUE|FALSE,
        Deleted = TRUE|FALSE,
        Endpoint = "string",
        Endpoints = list(
          "string"
        ),
        Processing = TRUE|FALSE,
        UpgradeProcessing = TRUE|FALSE,
        ElasticsearchVersion = "string",
        ElasticsearchClusterConfig = list(
          InstanceType = "m3.medium.elasticsearch"|"m3.large.elasticsearch"|"m3.xlarge.elasticsearch"|"m3.2xlarge.elasticsearch"|"m4.large.elasticsearch"|"m4.xlarge.elasticsearch"|"m4.2xlarge.elasticsearch"|"m4.4xlarge.elasticsearch"|"m4.10xlarge.elasticsearch"|"m5.large.elasticsearch"|"m5.xlarge.elasticsearch"|"m5.2xlarge.elasticsearch"|"m5.4xlarge.elasticsearch"|"m5.12xlarge.elasticsearch"|"r5.large.elasticsearch"|"r5.xlarge.elasticsearch"|"r5.2xlarge.elasticsearch"|"r5.4xlarge.elasticsearch"|"r5.12xlarge.elasticsearch"|"c5.large.elasticsearch"|"c5.xlarge.elasticsearch"|"c5.2xlarge.elasticsearch"|"c5.4xlarge.elasticsearch"|"c5.9xlarge.elasticsearch"|"c5.18xlarge.elasticsearch"|"ultrawarm1.medium.elasticsearch"|"ultrawarm1.large.elasticsearch"|"t2.micro.elasticsearch"|"t2.small.elasticsearch"|"t2.medium.elasticsearch"|"r3.large.elasticsearch"|"r3.xlarge.elasticsearch"|"r3.2xlarge.elasticsearch"|"r3.4xlarge.elasticsearch"|"r3.8xlarge.elasticsearch"|"i2.xlarge.elasticsearch"|"i2.2xlarge.elasticsearch"|"d2.xlarge.elasticsearch"|"d2.2xlarge.elasticsearch"|"d2.4xlarge.elasticsearch"|"d2.8xlarge.elasticsearch"|"c4.large.elasticsearch"|"c4.xlarge.elasticsearch"|"c4.2xlarge.elasticsearch"|"c4.4xlarge.elasticsearch"|"c4.8xlarge.elasticsearch"|"r4.large.elasticsearch"|"r4.xlarge.elasticsearch"|"r4.2xlarge.elasticsearch"|"r4.4xlarge.elasticsearch"|"r4.8xlarge.elasticsearch"|"r4.16xlarge.elasticsearch"|"i3.large.elasticsearch"|"i3.xlarge.elasticsearch"|"i3.2xlarge.elasticsearch"|"i3.4xlarge.elasticsearch"|"i3.8xlarge.elasticsearch"|"i3.16xlarge.elasticsearch",
          InstanceCount = 123,
          DedicatedMasterEnabled = TRUE|FALSE,
          ZoneAwarenessEnabled = TRUE|FALSE,
          ZoneAwarenessConfig = list(
            AvailabilityZoneCount = 123
          ),
          DedicatedMasterType = "m3.medium.elasticsearch"|"m3.large.elasticsearch"|"m3.xlarge.elasticsearch"|"m3.2xlarge.elasticsearch"|"m4.large.elasticsearch"|"m4.xlarge.elasticsearch"|"m4.2xlarge.elasticsearch"|"m4.4xlarge.elasticsearch"|"m4.10xlarge.elasticsearch"|"m5.large.elasticsearch"|"m5.xlarge.elasticsearch"|"m5.2xlarge.elasticsearch"|"m5.4xlarge.elasticsearch"|"m5.12xlarge.elasticsearch"|"r5.large.elasticsearch"|"r5.xlarge.elasticsearch"|"r5.2xlarge.elasticsearch"|"r5.4xlarge.elasticsearch"|"r5.12xlarge.elasticsearch"|"c5.large.elasticsearch"|"c5.xlarge.elasticsearch"|"c5.2xlarge.elasticsearch"|"c5.4xlarge.elasticsearch"|"c5.9xlarge.elasticsearch"|"c5.18xlarge.elasticsearch"|"ultrawarm1.medium.elasticsearch"|"ultrawarm1.large.elasticsearch"|"t2.micro.elasticsearch"|"t2.small.elasticsearch"|"t2.medium.elasticsearch"|"r3.large.elasticsearch"|"r3.xlarge.elasticsearch"|"r3.2xlarge.elasticsearch"|"r3.4xlarge.elasticsearch"|"r3.8xlarge.elasticsearch"|"i2.xlarge.elasticsearch"|"i2.2xlarge.elasticsearch"|"d2.xlarge.elasticsearch"|"d2.2xlarge.elasticsearch"|"d2.4xlarge.elasticsearch"|"d2.8xlarge.elasticsearch"|"c4.large.elasticsearch"|"c4.xlarge.elasticsearch"|"c4.2xlarge.elasticsearch"|"c4.4xlarge.elasticsearch"|"c4.8xlarge.elasticsearch"|"r4.large.elasticsearch"|"r4.xlarge.elasticsearch"|"r4.2xlarge.elasticsearch"|"r4.4xlarge.elasticsearch"|"r4.8xlarge.elasticsearch"|"r4.16xlarge.elasticsearch"|"i3.large.elasticsearch"|"i3.xlarge.elasticsearch"|"i3.2xlarge.elasticsearch"|"i3.4xlarge.elasticsearch"|"i3.8xlarge.elasticsearch"|"i3.16xlarge.elasticsearch",
          DedicatedMasterCount = 123,
          WarmEnabled = TRUE|FALSE,
          WarmType = "ultrawarm1.medium.elasticsearch"|"ultrawarm1.large.elasticsearch",
          WarmCount = 123,
          ColdStorageOptions = list(
            Enabled = TRUE|FALSE
          )
        ),
        EBSOptions = list(
          EBSEnabled = TRUE|FALSE,
          VolumeType = "standard"|"gp2"|"io1"|"gp3",
          VolumeSize = 123,
          Iops = 123,
          Throughput = 123
        ),
        AccessPolicies = "string",
        SnapshotOptions = list(
          AutomatedSnapshotStartHour = 123
        ),
        VPCOptions = list(
          VPCId = "string",
          SubnetIds = list(
            "string"
          ),
          AvailabilityZones = list(
            "string"
          ),
          SecurityGroupIds = list(
            "string"
          )
        ),
        CognitoOptions = list(
          Enabled = TRUE|FALSE,
          UserPoolId = "string",
          IdentityPoolId = "string",
          RoleArn = "string"
        ),
        EncryptionAtRestOptions = list(
          Enabled = TRUE|FALSE,
          KmsKeyId = "string"
        ),
        NodeToNodeEncryptionOptions = list(
          Enabled = TRUE|FALSE
        ),
        AdvancedOptions = list(
          "string"
        ),
        LogPublishingOptions = list(
          list(
            CloudWatchLogsLogGroupArn = "string",
            Enabled = TRUE|FALSE
          )
        ),
        ServiceSoftwareOptions = list(
          CurrentVersion = "string",
          NewVersion = "string",
          UpdateAvailable = TRUE|FALSE,
          Cancellable = TRUE|FALSE,
          UpdateStatus = "PENDING_UPDATE"|"IN_PROGRESS"|"COMPLETED"|"NOT_ELIGIBLE"|"ELIGIBLE",
          Description = "string",
          AutomatedUpdateDate = as.POSIXct(
            "2015-01-01"
          ),
          OptionalDeployment = TRUE|FALSE
        ),
        DomainEndpointOptions = list(
          EnforceHTTPS = TRUE|FALSE,
          TLSSecurityPolicy = "Policy-Min-TLS-1-0-2019-07"|"Policy-Min-TLS-1-2-2019-07",
          CustomEndpointEnabled = TRUE|FALSE,
          CustomEndpoint = "string",
          CustomEndpointCertificateArn = "string"
        ),
        AdvancedSecurityOptions = list(
          Enabled = TRUE|FALSE,
          InternalUserDatabaseEnabled = TRUE|FALSE,
          SAMLOptions = list(
            Enabled = TRUE|FALSE,
            Idp = list(
              MetadataContent = "string",
              EntityId = "string"
            ),
            SubjectKey = "string",
            RolesKey = "string",
            SessionTimeoutMinutes = 123
          ),
          AnonymousAuthDisableDate = as.POSIXct(
            "2015-01-01"
          ),
          AnonymousAuthEnabled = TRUE|FALSE
        ),
        AutoTuneOptions = list(
          State = "ENABLED"|"DISABLED"|"ENABLE_IN_PROGRESS"|"DISABLE_IN_PROGRESS"|"DISABLED_AND_ROLLBACK_SCHEDULED"|"DISABLED_AND_ROLLBACK_IN_PROGRESS"|"DISABLED_AND_ROLLBACK_COMPLETE"|"DISABLED_AND_ROLLBACK_ERROR"|"ERROR",
          ErrorMessage = "string"
        ),
        ChangeProgressDetails = list(
          ChangeId = "string",
          Message = "string"
        )
      )
    )

### Request syntax

    svc$delete_elasticsearch_domain(
      DomainName = "string"
    )