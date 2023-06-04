<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>wellarchitected_get_workload</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Get an existing workload

### Description

Get an existing workload.

### Usage

    wellarchitected_get_workload(WorkloadId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="wellarchitected_get_workload_:_WorkloadId">WorkloadId</code></td>
<td><p>[required]</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Workload = list(
        WorkloadId = "string",
        WorkloadArn = "string",
        WorkloadName = "string",
        Description = "string",
        Environment = "PRODUCTION"|"PREPRODUCTION",
        UpdatedAt = as.POSIXct(
          "2015-01-01"
        ),
        AccountIds = list(
          "string"
        ),
        AwsRegions = list(
          "string"
        ),
        NonAwsRegions = list(
          "string"
        ),
        ArchitecturalDesign = "string",
        ReviewOwner = "string",
        ReviewRestrictionDate = as.POSIXct(
          "2015-01-01"
        ),
        IsReviewOwnerUpdateAcknowledged = TRUE|FALSE,
        IndustryType = "string",
        Industry = "string",
        Notes = "string",
        ImprovementStatus = "NOT_APPLICABLE"|"NOT_STARTED"|"IN_PROGRESS"|"COMPLETE"|"RISK_ACKNOWLEDGED",
        RiskCounts = list(
          123
        ),
        PillarPriorities = list(
          "string"
        ),
        Lenses = list(
          "string"
        ),
        Owner = "string",
        ShareInvitationId = "string",
        Tags = list(
          "string"
        ),
        DiscoveryConfig = list(
          TrustedAdvisorIntegrationStatus = "ENABLED"|"DISABLED",
          WorkloadResourceDefinition = list(
            "WORKLOAD_METADATA"|"APP_REGISTRY"
          )
        ),
        Applications = list(
          "string"
        )
      )
    )

### Request syntax

    svc$get_workload(
      WorkloadId = "string"
    )