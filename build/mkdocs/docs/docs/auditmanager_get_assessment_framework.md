<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>auditmanager_get_assessment_framework</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns a framework from Audit Manager

### Description

Returns a framework from Audit Manager.

### Usage

    auditmanager_get_assessment_framework(frameworkId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="auditmanager_get_assessment_framework_:_frameworkId">frameworkId</code></td>
<td><p>[required] The identifier for the framework.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      framework = list(
        arn = "string",
        id = "string",
        name = "string",
        type = "Standard"|"Custom",
        complianceType = "string",
        description = "string",
        logo = "string",
        controlSources = "string",
        controlSets = list(
          list(
            id = "string",
            name = "string",
            controls = list(
              list(
                arn = "string",
                id = "string",
                type = "Standard"|"Custom",
                name = "string",
                description = "string",
                testingInformation = "string",
                actionPlanTitle = "string",
                actionPlanInstructions = "string",
                controlSources = "string",
                controlMappingSources = list(
                  list(
                    sourceId = "string",
                    sourceName = "string",
                    sourceDescription = "string",
                    sourceSetUpOption = "System_Controls_Mapping"|"Procedural_Controls_Mapping",
                    sourceType = "AWS_Cloudtrail"|"AWS_Config"|"AWS_Security_Hub"|"AWS_API_Call"|"MANUAL",
                    sourceKeyword = list(
                      keywordInputType = "SELECT_FROM_LIST",
                      keywordValue = "string"
                    ),
                    sourceFrequency = "DAILY"|"WEEKLY"|"MONTHLY",
                    troubleshootingText = "string"
                  )
                ),
                createdAt = as.POSIXct(
                  "2015-01-01"
                ),
                lastUpdatedAt = as.POSIXct(
                  "2015-01-01"
                ),
                createdBy = "string",
                lastUpdatedBy = "string",
                tags = list(
                  "string"
                )
              )
            )
          )
        ),
        createdAt = as.POSIXct(
          "2015-01-01"
        ),
        lastUpdatedAt = as.POSIXct(
          "2015-01-01"
        ),
        createdBy = "string",
        lastUpdatedBy = "string",
        tags = list(
          "string"
        )
      )
    )

### Request syntax

    svc$get_assessment_framework(
      frameworkId = "string"
    )