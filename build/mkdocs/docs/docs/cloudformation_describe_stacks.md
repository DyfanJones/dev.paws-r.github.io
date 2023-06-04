<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>cloudformation_describe_stacks</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns the description for the specified stack; if no stack name was specified, then it returns the description for all the stacks created

### Description

Returns the description for the specified stack; if no stack name was
specified, then it returns the description for all the stacks created.

If the stack doesn't exist, an `ValidationError` is returned.

### Usage

    cloudformation_describe_stacks(StackName, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="cloudformation_describe_stacks_:_StackName">StackName</code></td>
<td><p>The name or the unique stack ID that's associated with the stack,
which aren't always interchangeable:</p>
<ul>
<li><p>Running stacks: You can specify either the stack's name or its
unique stack ID.</p></li>
<li><p>Deleted stacks: You must specify the unique stack ID.</p></li>
</ul>
<p>Default: There is no default value.</p></td>
</tr>
<tr class="even">
<td><code
id="cloudformation_describe_stacks_:_NextToken">NextToken</code></td>
<td><p>A string that identifies the next page of stacks that you want to
retrieve.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Stacks = list(
        list(
          StackId = "string",
          StackName = "string",
          ChangeSetId = "string",
          Description = "string",
          Parameters = list(
            list(
              ParameterKey = "string",
              ParameterValue = "string",
              UsePreviousValue = TRUE|FALSE,
              ResolvedValue = "string"
            )
          ),
          CreationTime = as.POSIXct(
            "2015-01-01"
          ),
          DeletionTime = as.POSIXct(
            "2015-01-01"
          ),
          LastUpdatedTime = as.POSIXct(
            "2015-01-01"
          ),
          RollbackConfiguration = list(
            RollbackTriggers = list(
              list(
                Arn = "string",
                Type = "string"
              )
            ),
            MonitoringTimeInMinutes = 123
          ),
          StackStatus = "CREATE_IN_PROGRESS"|"CREATE_FAILED"|"CREATE_COMPLETE"|"ROLLBACK_IN_PROGRESS"|"ROLLBACK_FAILED"|"ROLLBACK_COMPLETE"|"DELETE_IN_PROGRESS"|"DELETE_FAILED"|"DELETE_COMPLETE"|"UPDATE_IN_PROGRESS"|"UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"|"UPDATE_COMPLETE"|"UPDATE_FAILED"|"UPDATE_ROLLBACK_IN_PROGRESS"|"UPDATE_ROLLBACK_FAILED"|"UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"|"UPDATE_ROLLBACK_COMPLETE"|"REVIEW_IN_PROGRESS"|"IMPORT_IN_PROGRESS"|"IMPORT_COMPLETE"|"IMPORT_ROLLBACK_IN_PROGRESS"|"IMPORT_ROLLBACK_FAILED"|"IMPORT_ROLLBACK_COMPLETE",
          StackStatusReason = "string",
          DisableRollback = TRUE|FALSE,
          NotificationARNs = list(
            "string"
          ),
          TimeoutInMinutes = 123,
          Capabilities = list(
            "CAPABILITY_IAM"|"CAPABILITY_NAMED_IAM"|"CAPABILITY_AUTO_EXPAND"
          ),
          Outputs = list(
            list(
              OutputKey = "string",
              OutputValue = "string",
              Description = "string",
              ExportName = "string"
            )
          ),
          RoleARN = "string",
          Tags = list(
            list(
              Key = "string",
              Value = "string"
            )
          ),
          EnableTerminationProtection = TRUE|FALSE,
          ParentId = "string",
          RootId = "string",
          DriftInformation = list(
            StackDriftStatus = "DRIFTED"|"IN_SYNC"|"UNKNOWN"|"NOT_CHECKED",
            LastCheckTimestamp = as.POSIXct(
              "2015-01-01"
            )
          )
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$describe_stacks(
      StackName = "string",
      NextToken = "string"
    )