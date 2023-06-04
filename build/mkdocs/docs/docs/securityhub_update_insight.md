<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>securityhub_update_insight</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Updates the Security Hub insight identified by the specified insight ARN

### Description

Updates the Security Hub insight identified by the specified insight
ARN.

### Usage

    securityhub_update_insight(InsightArn, Name, Filters, GroupByAttribute)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="securityhub_update_insight_:_InsightArn">InsightArn</code></td>
<td><p>[required] The ARN of the insight that you want to
update.</p></td>
</tr>
<tr class="even">
<td><code id="securityhub_update_insight_:_Name">Name</code></td>
<td><p>The updated name for the insight.</p></td>
</tr>
<tr class="odd">
<td><code id="securityhub_update_insight_:_Filters">Filters</code></td>
<td><p>The updated filters that define this insight.</p></td>
</tr>
<tr class="even">
<td><code
id="securityhub_update_insight_:_GroupByAttribute">GroupByAttribute</code></td>
<td><p>The updated <code>GroupBy</code> attribute that defines this
insight.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$update_insight(
      InsightArn = "string",
      Name = "string",
      Filters = list(
        ProductArn = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        AwsAccountId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Id = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        GeneratorId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Region = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Type = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        FirstObservedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        LastObservedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        CreatedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        UpdatedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        SeverityProduct = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        SeverityNormalized = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        SeverityLabel = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Confidence = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        Criticality = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        Title = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Description = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        RecommendationText = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        SourceUrl = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ProductFields = list(
          list(
            Key = "string",
            Value = "string",
            Comparison = "EQUALS"|"NOT_EQUALS"
          )
        ),
        ProductName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        CompanyName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        UserDefinedFields = list(
          list(
            Key = "string",
            Value = "string",
            Comparison = "EQUALS"|"NOT_EQUALS"
          )
        ),
        MalwareName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        MalwareType = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        MalwarePath = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        MalwareState = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NetworkDirection = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NetworkProtocol = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NetworkSourceIpV4 = list(
          list(
            Cidr = "string"
          )
        ),
        NetworkSourceIpV6 = list(
          list(
            Cidr = "string"
          )
        ),
        NetworkSourcePort = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        NetworkSourceDomain = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NetworkSourceMac = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NetworkDestinationIpV4 = list(
          list(
            Cidr = "string"
          )
        ),
        NetworkDestinationIpV6 = list(
          list(
            Cidr = "string"
          )
        ),
        NetworkDestinationPort = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        NetworkDestinationDomain = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ProcessName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ProcessPath = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ProcessPid = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        ProcessParentPid = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        ProcessLaunchedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ProcessTerminatedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ThreatIntelIndicatorType = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ThreatIntelIndicatorValue = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ThreatIntelIndicatorCategory = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ThreatIntelIndicatorLastObservedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ThreatIntelIndicatorSource = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ThreatIntelIndicatorSourceUrl = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceType = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourcePartition = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceRegion = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceTags = list(
          list(
            Key = "string",
            Value = "string",
            Comparison = "EQUALS"|"NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceType = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceImageId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceIpV4Addresses = list(
          list(
            Cidr = "string"
          )
        ),
        ResourceAwsEc2InstanceIpV6Addresses = list(
          list(
            Cidr = "string"
          )
        ),
        ResourceAwsEc2InstanceKeyName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceIamInstanceProfileArn = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceVpcId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceSubnetId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceLaunchedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ResourceAwsS3BucketOwnerId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsS3BucketOwnerName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsIamAccessKeyUserName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsIamAccessKeyPrincipalName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsIamAccessKeyStatus = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsIamAccessKeyCreatedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ResourceAwsIamUserUserName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceContainerName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceContainerImageId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceContainerImageName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceContainerLaunchedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ResourceDetailsOther = list(
          list(
            Key = "string",
            Value = "string",
            Comparison = "EQUALS"|"NOT_EQUALS"
          )
        ),
        ComplianceStatus = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        VerificationState = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        WorkflowState = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        WorkflowStatus = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        RecordState = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        RelatedFindingsProductArn = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        RelatedFindingsId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NoteText = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NoteUpdatedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        NoteUpdatedBy = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Keyword = list(
          list(
            Value = "string"
          )
        ),
        FindingProviderFieldsConfidence = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        FindingProviderFieldsCriticality = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        FindingProviderFieldsRelatedFindingsId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        FindingProviderFieldsRelatedFindingsProductArn = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        FindingProviderFieldsSeverityLabel = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        FindingProviderFieldsSeverityOriginal = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        FindingProviderFieldsTypes = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Sample = list(
          list(
            Value = TRUE|FALSE
          )
        ),
        ComplianceSecurityControlId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ComplianceAssociatedStandardsId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        )
      ),
      GroupByAttribute = "string"
    )

### Examples

    ## Not run: 
    # The following example updates the specified Security Hub insight.
    svc$update_insight(
      Filters = list(
        ResourceType = list(
          list(
            Comparison = "EQUALS",
            Value = "AwsIamRole"
          )
        ),
        SeverityLabel = list(
          list(
            Comparison = "EQUALS",
            Value = "HIGH"
          )
        )
      ),
      InsightArn = "arn:aws:securityhub:us-west-1:123456789012:insight/12345678...",
      Name = "High severity role findings"
    )

    ## End(Not run)