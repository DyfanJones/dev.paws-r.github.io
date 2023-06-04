<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>costexplorer_create_anomaly_monitor</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates a new cost anomaly detection monitor with the requested type and monitor specification

### Description

Creates a new cost anomaly detection monitor with the requested type and
monitor specification.

### Usage

    costexplorer_create_anomaly_monitor(AnomalyMonitor, ResourceTags)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="costexplorer_create_anomaly_monitor_:_AnomalyMonitor">AnomalyMonitor</code></td>
<td><p>[required] The cost anomaly detection monitor object that you
want to create.</p></td>
</tr>
<tr class="even">
<td><code
id="costexplorer_create_anomaly_monitor_:_ResourceTags">ResourceTags</code></td>
<td><p>An optional list of tags to associate with the specified <a
href="https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_AnomalyMonitor.html"><code>AnomalyMonitor</code></a>
. You can use resource tags to control access to your
<code>monitor</code> using IAM policies.</p>
<p>Each tag consists of a key and a value, and each key must be unique
for the resource. The following restrictions apply to resource tags:</p>
<ul>
<li><p>Although the maximum number of array members is 200, you can
assign a maximum of 50 user-tags to one resource. The remaining are
reserved for Amazon Web Services use</p></li>
<li><p>The maximum length of a key is 128 characters</p></li>
<li><p>The maximum length of a value is 256 characters</p></li>
<li><p>Keys and values can only contain alphanumeric characters, spaces,
and any of the following: <code
style="white-space: pre;">⁠_.:/=+@-⁠</code></p></li>
<li><p>Keys and values are case sensitive</p></li>
<li><p>Keys and values are trimmed for any leading or trailing
whitespaces</p></li>
<li><p>Don’t use <code style="white-space: pre;">⁠aws:⁠</code> as a prefix
for your keys. This prefix is reserved for Amazon Web Services
use</p></li>
</ul></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      MonitorArn = "string"
    )

### Request syntax

    svc$create_anomaly_monitor(
      AnomalyMonitor = list(
        MonitorArn = "string",
        MonitorName = "string",
        CreationDate = "string",
        LastUpdatedDate = "string",
        LastEvaluatedDate = "string",
        MonitorType = "DIMENSIONAL"|"CUSTOM",
        MonitorDimension = "SERVICE",
        MonitorSpecification = list(
          Or = list(
            list()
          ),
          And = list(
            list()
          ),
          Not = list(),
          Dimensions = list(
            Key = "AZ"|"INSTANCE_TYPE"|"LINKED_ACCOUNT"|"LINKED_ACCOUNT_NAME"|"OPERATION"|"PURCHASE_TYPE"|"REGION"|"SERVICE"|"SERVICE_CODE"|"USAGE_TYPE"|"USAGE_TYPE_GROUP"|"RECORD_TYPE"|"OPERATING_SYSTEM"|"TENANCY"|"SCOPE"|"PLATFORM"|"SUBSCRIPTION_ID"|"LEGAL_ENTITY_NAME"|"DEPLOYMENT_OPTION"|"DATABASE_ENGINE"|"CACHE_ENGINE"|"INSTANCE_TYPE_FAMILY"|"BILLING_ENTITY"|"RESERVATION_ID"|"RESOURCE_ID"|"RIGHTSIZING_TYPE"|"SAVINGS_PLANS_TYPE"|"SAVINGS_PLAN_ARN"|"PAYMENT_OPTION"|"AGREEMENT_END_DATE_TIME_AFTER"|"AGREEMENT_END_DATE_TIME_BEFORE"|"INVOICING_ENTITY"|"ANOMALY_TOTAL_IMPACT_ABSOLUTE"|"ANOMALY_TOTAL_IMPACT_PERCENTAGE",
            Values = list(
              "string"
            ),
            MatchOptions = list(
              "EQUALS"|"ABSENT"|"STARTS_WITH"|"ENDS_WITH"|"CONTAINS"|"CASE_SENSITIVE"|"CASE_INSENSITIVE"|"GREATER_THAN_OR_EQUAL"
            )
          ),
          Tags = list(
            Key = "string",
            Values = list(
              "string"
            ),
            MatchOptions = list(
              "EQUALS"|"ABSENT"|"STARTS_WITH"|"ENDS_WITH"|"CONTAINS"|"CASE_SENSITIVE"|"CASE_INSENSITIVE"|"GREATER_THAN_OR_EQUAL"
            )
          ),
          CostCategories = list(
            Key = "string",
            Values = list(
              "string"
            ),
            MatchOptions = list(
              "EQUALS"|"ABSENT"|"STARTS_WITH"|"ENDS_WITH"|"CONTAINS"|"CASE_SENSITIVE"|"CASE_INSENSITIVE"|"GREATER_THAN_OR_EQUAL"
            )
          )
        ),
        DimensionalValueCount = 123
      ),
      ResourceTags = list(
        list(
          Key = "string",
          Value = "string"
        )
      )
    )