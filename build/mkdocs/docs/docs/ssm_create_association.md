<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ssm_create_association</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## A State Manager association defines the state that you want to maintain on your managed nodes

### Description

A State Manager association defines the state that you want to maintain
on your managed nodes. For example, an association can specify that
anti-virus software must be installed and running on your managed nodes,
or that certain ports must be closed. For static targets, the
association specifies a schedule for when the configuration is
reapplied. For dynamic targets, such as an Amazon Web Services resource
group or an Amazon Web Services autoscaling group, State Manager, a
capability of Amazon Web Services Systems Manager applies the
configuration when new managed nodes are added to the group. The
association also specifies actions to take when applying the
configuration. For example, an association for anti-virus software might
run once a day. If the software isn't installed, then State Manager
installs it. If the software is installed, but the service isn't
running, then the association might instruct State Manager to start the
service.

### Usage

    ssm_create_association(Name, DocumentVersion, InstanceId, Parameters,
      Targets, ScheduleExpression, OutputLocation, AssociationName,
      AutomationTargetParameterName, MaxErrors, MaxConcurrency,
      ComplianceSeverity, SyncCompliance, ApplyOnlyAtCronInterval,
      CalendarNames, TargetLocations, ScheduleOffset, TargetMaps, Tags,
      AlarmConfiguration)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="ssm_create_association_:_Name">Name</code></td>
<td><p>[required] The name of the SSM Command document or Automation
runbook that contains the configuration information for the managed
node.</p>
<p>You can specify Amazon Web Services-predefined documents, documents
you created, or a document that is shared with you from another Amazon
Web Services account.</p>
<p>For Systems Manager documents (SSM documents) that are shared with
you from other Amazon Web Services accounts, you must specify the
complete SSM document ARN, in the following format:</p>
<p><code>arn:partition:ssm:region:account-id:document/document-name </code></p>
<p>For example:</p>
<p><code>arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document</code></p>
<p>For Amazon Web Services-predefined documents and SSM documents you
created in your account, you only need to specify the document name. For
example, <code>AWS-ApplyPatchBaseline</code> or
<code>My-Document</code>.</p></td>
</tr>
<tr class="even">
<td><code
id="ssm_create_association_:_DocumentVersion">DocumentVersion</code></td>
<td><p>The document version you want to associate with the target(s).
Can be a specific version or the default version.</p>
<p>State Manager doesn't support running associations that use a new
version of a document if that document is shared from another account.
State Manager always runs the <code>default</code> version of a document
if shared from another account, even though the Systems Manager console
shows that a new version was processed. If you want to run an
association using a new version of a document shared form another
account, you must set the document version to
<code>default</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssm_create_association_:_InstanceId">InstanceId</code></td>
<td><p>The managed node ID.</p>
<p><code>InstanceId</code> has been deprecated. To specify a managed
node ID for an association, use the <code>Targets</code> parameter.
Requests that include the parameter <code>InstanceID</code> with Systems
Manager documents (SSM documents) that use schema version 2.0 or later
will fail. In addition, if you use the parameter
<code>InstanceId</code>, you can't use the parameters
<code>AssociationName</code>, <code>DocumentVersion</code>,
<code>MaxErrors</code>, <code>MaxConcurrency</code>,
<code>OutputLocation</code>, or <code>ScheduleExpression</code>. To use
these parameters, you must use the <code>Targets</code>
parameter.</p></td>
</tr>
<tr class="even">
<td><code
id="ssm_create_association_:_Parameters">Parameters</code></td>
<td><p>The parameters for the runtime configuration of the
document.</p></td>
</tr>
<tr class="odd">
<td><code id="ssm_create_association_:_Targets">Targets</code></td>
<td><p>The targets for the association. You can target managed nodes by
using tags, Amazon Web Services resource groups, all managed nodes in an
Amazon Web Services account, or individual managed node IDs. You can
target all managed nodes in an Amazon Web Services account by specifying
the <code>InstanceIds</code> key with a value of <code>*</code>. For
more information about choosing targets for an association, see <a
href="https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-state-manager-targets-and-rate-controls.html">Using
targets and rate controls with State Manager associations</a> in the
<em>Amazon Web Services Systems Manager User Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code
id="ssm_create_association_:_ScheduleExpression">ScheduleExpression</code></td>
<td><p>A cron expression when the association will be applied to the
target(s).</p></td>
</tr>
<tr class="odd">
<td><code
id="ssm_create_association_:_OutputLocation">OutputLocation</code></td>
<td><p>An Amazon Simple Storage Service (Amazon S3) bucket where you
want to store the output details of the request.</p></td>
</tr>
<tr class="even">
<td><code
id="ssm_create_association_:_AssociationName">AssociationName</code></td>
<td><p>Specify a descriptive name for the association.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssm_create_association_:_AutomationTargetParameterName">AutomationTargetParameterName</code></td>
<td><p>Choose the parameter that will define how your automation will
branch out. This target is required for associations that use an
Automation runbook and target resources by using rate controls.
Automation is a capability of Amazon Web Services Systems
Manager.</p></td>
</tr>
<tr class="even">
<td><code id="ssm_create_association_:_MaxErrors">MaxErrors</code></td>
<td><p>The number of errors that are allowed before the system stops
sending requests to run the association on additional targets. You can
specify either an absolute number of errors, for example 10, or a
percentage of the target set, for example 10%. If you specify 3, for
example, the system stops sending requests when the fourth error is
received. If you specify 0, then the system stops sending requests after
the first error is returned. If you run an association on 50 managed
nodes and set <code>MaxError</code> to 10%, then the system stops
sending the request when the sixth error is received.</p>
<p>Executions that are already running an association when
<code>MaxErrors</code> is reached are allowed to complete, but some of
these executions may fail as well. If you need to ensure that there
won't be more than max-errors failed executions, set
<code>MaxConcurrency</code> to 1 so that executions proceed one at a
time.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssm_create_association_:_MaxConcurrency">MaxConcurrency</code></td>
<td><p>The maximum number of targets allowed to run the association at
the same time. You can specify a number, for example 10, or a percentage
of the target set, for example 10%. The default value is 100%, which
means all targets run the association at the same time.</p>
<p>If a new managed node starts and attempts to run an association while
Systems Manager is running <code>MaxConcurrency</code> associations, the
association is allowed to run. During the next association interval, the
new managed node will process its association within the limit specified
for <code>MaxConcurrency</code>.</p></td>
</tr>
<tr class="even">
<td><code
id="ssm_create_association_:_ComplianceSeverity">ComplianceSeverity</code></td>
<td><p>The severity level to assign to the association.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssm_create_association_:_SyncCompliance">SyncCompliance</code></td>
<td><p>The mode for generating association compliance. You can specify
<code>AUTO</code> or <code>MANUAL</code>. In <code>AUTO</code> mode, the
system uses the status of the association execution to determine the
compliance status. If the association execution runs successfully, then
the association is <code>COMPLIANT</code>. If the association execution
doesn't run successfully, the association is
<code>NON-COMPLIANT</code>.</p>
<p>In <code>MANUAL</code> mode, you must specify the
<code>AssociationId</code> as a parameter for the
<code>put_compliance_items</code> API operation. In this case,
compliance data isn't managed by State Manager. It is managed by your
direct call to the <code>put_compliance_items</code> API operation.</p>
<p>By default, all associations use <code>AUTO</code> mode.</p></td>
</tr>
<tr class="even">
<td><code
id="ssm_create_association_:_ApplyOnlyAtCronInterval">ApplyOnlyAtCronInterval</code></td>
<td><p>By default, when you create a new association, the system runs it
immediately after it is created and then according to the schedule you
specified. Specify this option if you don't want an association to run
immediately after you create it. This parameter isn't supported for rate
expressions.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssm_create_association_:_CalendarNames">CalendarNames</code></td>
<td><p>The names or Amazon Resource Names (ARNs) of the Change Calendar
type documents you want to gate your associations under. The
associations only run when that change calendar is open. For more
information, see <a
href="https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar.html">Amazon
Web Services Systems Manager Change Calendar</a>.</p></td>
</tr>
<tr class="even">
<td><code
id="ssm_create_association_:_TargetLocations">TargetLocations</code></td>
<td><p>A location is a combination of Amazon Web Services Regions and
Amazon Web Services accounts where you want to run the association. Use
this action to create an association in multiple Regions and multiple
accounts.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssm_create_association_:_ScheduleOffset">ScheduleOffset</code></td>
<td><p>Number of days to wait after the scheduled day to run an
association. For example, if you specified a cron schedule of <code
style="white-space: pre;">⁠cron(0 0 ? * THU#2 *)⁠</code>, you could
specify an offset of 3 to run the association each Sunday after the
second Thursday of the month. For more information about cron schedules
for associations, see <a
href="https://docs.aws.amazon.com/systems-manager/latest/userguide/reference-cron-and-rate-expressions.html">Reference:
Cron and rate expressions for Systems Manager</a> in the <em>Amazon Web
Services Systems Manager User Guide</em>.</p>
<p>To use offsets, you must specify the
<code>ApplyOnlyAtCronInterval</code> parameter. This option tells the
system not to run an association immediately after you create
it.</p></td>
</tr>
<tr class="even">
<td><code
id="ssm_create_association_:_TargetMaps">TargetMaps</code></td>
<td><p>A key-value mapping of document parameters to target resources.
Both Targets and TargetMaps can't be specified together.</p></td>
</tr>
<tr class="odd">
<td><code id="ssm_create_association_:_Tags">Tags</code></td>
<td><p>Adds or overwrites one or more tags for a State Manager
association. <em>Tags</em> are metadata that you can assign to your
Amazon Web Services resources. Tags enable you to categorize your
resources in different ways, for example, by purpose, owner, or
environment. Each tag consists of a key and an optional value, both of
which you define.</p></td>
</tr>
<tr class="even">
<td><code
id="ssm_create_association_:_AlarmConfiguration">AlarmConfiguration</code></td>
<td></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      AssociationDescription = list(
        Name = "string",
        InstanceId = "string",
        AssociationVersion = "string",
        Date = as.POSIXct(
          "2015-01-01"
        ),
        LastUpdateAssociationDate = as.POSIXct(
          "2015-01-01"
        ),
        Status = list(
          Date = as.POSIXct(
            "2015-01-01"
          ),
          Name = "Pending"|"Success"|"Failed",
          Message = "string",
          AdditionalInfo = "string"
        ),
        Overview = list(
          Status = "string",
          DetailedStatus = "string",
          AssociationStatusAggregatedCount = list(
            123
          )
        ),
        DocumentVersion = "string",
        AutomationTargetParameterName = "string",
        Parameters = list(
          list(
            "string"
          )
        ),
        AssociationId = "string",
        Targets = list(
          list(
            Key = "string",
            Values = list(
              "string"
            )
          )
        ),
        ScheduleExpression = "string",
        OutputLocation = list(
          S3Location = list(
            OutputS3Region = "string",
            OutputS3BucketName = "string",
            OutputS3KeyPrefix = "string"
          )
        ),
        LastExecutionDate = as.POSIXct(
          "2015-01-01"
        ),
        LastSuccessfulExecutionDate = as.POSIXct(
          "2015-01-01"
        ),
        AssociationName = "string",
        MaxErrors = "string",
        MaxConcurrency = "string",
        ComplianceSeverity = "CRITICAL"|"HIGH"|"MEDIUM"|"LOW"|"UNSPECIFIED",
        SyncCompliance = "AUTO"|"MANUAL",
        ApplyOnlyAtCronInterval = TRUE|FALSE,
        CalendarNames = list(
          "string"
        ),
        TargetLocations = list(
          list(
            Accounts = list(
              "string"
            ),
            Regions = list(
              "string"
            ),
            TargetLocationMaxConcurrency = "string",
            TargetLocationMaxErrors = "string",
            ExecutionRoleName = "string",
            TargetLocationAlarmConfiguration = list(
              IgnorePollAlarmFailure = TRUE|FALSE,
              Alarms = list(
                list(
                  Name = "string"
                )
              )
            )
          )
        ),
        ScheduleOffset = 123,
        TargetMaps = list(
          list(
            list(
              "string"
            )
          )
        ),
        AlarmConfiguration = list(
          IgnorePollAlarmFailure = TRUE|FALSE,
          Alarms = list(
            list(
              Name = "string"
            )
          )
        ),
        TriggeredAlarms = list(
          list(
            Name = "string",
            State = "UNKNOWN"|"ALARM"
          )
        )
      )
    )

### Request syntax

    svc$create_association(
      Name = "string",
      DocumentVersion = "string",
      InstanceId = "string",
      Parameters = list(
        list(
          "string"
        )
      ),
      Targets = list(
        list(
          Key = "string",
          Values = list(
            "string"
          )
        )
      ),
      ScheduleExpression = "string",
      OutputLocation = list(
        S3Location = list(
          OutputS3Region = "string",
          OutputS3BucketName = "string",
          OutputS3KeyPrefix = "string"
        )
      ),
      AssociationName = "string",
      AutomationTargetParameterName = "string",
      MaxErrors = "string",
      MaxConcurrency = "string",
      ComplianceSeverity = "CRITICAL"|"HIGH"|"MEDIUM"|"LOW"|"UNSPECIFIED",
      SyncCompliance = "AUTO"|"MANUAL",
      ApplyOnlyAtCronInterval = TRUE|FALSE,
      CalendarNames = list(
        "string"
      ),
      TargetLocations = list(
        list(
          Accounts = list(
            "string"
          ),
          Regions = list(
            "string"
          ),
          TargetLocationMaxConcurrency = "string",
          TargetLocationMaxErrors = "string",
          ExecutionRoleName = "string",
          TargetLocationAlarmConfiguration = list(
            IgnorePollAlarmFailure = TRUE|FALSE,
            Alarms = list(
              list(
                Name = "string"
              )
            )
          )
        )
      ),
      ScheduleOffset = 123,
      TargetMaps = list(
        list(
          list(
            "string"
          )
        )
      ),
      Tags = list(
        list(
          Key = "string",
          Value = "string"
        )
      ),
      AlarmConfiguration = list(
        IgnorePollAlarmFailure = TRUE|FALSE,
        Alarms = list(
          list(
            Name = "string"
          )
        )
      )
    )