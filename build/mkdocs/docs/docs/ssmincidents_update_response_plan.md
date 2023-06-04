<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ssmincidents_update_response_plan</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Updates the specified response plan

### Description

Updates the specified response plan.

### Usage

    ssmincidents_update_response_plan(actions, arn, chatChannel,
      clientToken, displayName, engagements, incidentTemplateDedupeString,
      incidentTemplateImpact, incidentTemplateNotificationTargets,
      incidentTemplateSummary, incidentTemplateTags, incidentTemplateTitle,
      integrations)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ssmincidents_update_response_plan_:_actions">actions</code></td>
<td><p>The actions that this response plan takes at the beginning of an
incident.</p></td>
</tr>
<tr class="even">
<td><code id="ssmincidents_update_response_plan_:_arn">arn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the response
plan.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssmincidents_update_response_plan_:_chatChannel">chatChannel</code></td>
<td><p>The Chatbot chat channel used for collaboration during an
incident.</p>
<p>Use the empty structure to remove the chat channel from the response
plan.</p></td>
</tr>
<tr class="even">
<td><code
id="ssmincidents_update_response_plan_:_clientToken">clientToken</code></td>
<td><p>A token ensuring that the operation is called only once with the
specified details.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssmincidents_update_response_plan_:_displayName">displayName</code></td>
<td><p>The long format name of the response plan. The display name can't
contain spaces.</p></td>
</tr>
<tr class="even">
<td><code
id="ssmincidents_update_response_plan_:_engagements">engagements</code></td>
<td><p>The Amazon Resource Name (ARN) for the contacts and escalation
plans that the response plan engages during an incident.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssmincidents_update_response_plan_:_incidentTemplateDedupeString">incidentTemplateDedupeString</code></td>
<td><p>The string Incident Manager uses to prevent duplicate incidents
from being created by the same incident in the same account.</p></td>
</tr>
<tr class="even">
<td><code
id="ssmincidents_update_response_plan_:_incidentTemplateImpact">incidentTemplateImpact</code></td>
<td><p>Defines the impact to the customers. Providing an impact
overwrites the impact provided by a response plan.</p>
<p><strong>Possible impacts:</strong></p>
<ul>
<li><p><code>5</code> - Severe impact</p></li>
<li><p><code>4</code> - High impact</p></li>
<li><p><code>3</code> - Medium impact</p></li>
<li><p><code>2</code> - Low impact</p></li>
<li><p><code>1</code> - No impact</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code
id="ssmincidents_update_response_plan_:_incidentTemplateNotificationTargets">incidentTemplateNotificationTargets</code></td>
<td><p>The Amazon SNS targets that are notified when updates are made to
an incident.</p></td>
</tr>
<tr class="even">
<td><code
id="ssmincidents_update_response_plan_:_incidentTemplateSummary">incidentTemplateSummary</code></td>
<td><p>A brief summary of the incident. This typically contains what has
happened, what's currently happening, and next steps.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssmincidents_update_response_plan_:_incidentTemplateTags">incidentTemplateTags</code></td>
<td><p>Tags to assign to the template. When the
<code>start_incident</code> API action is called, Incident Manager
assigns the tags specified in the template to the incident. To call this
action, you must also have permission to call the
<code>tag_resource</code> API action for the incident record
resource.</p></td>
</tr>
<tr class="even">
<td><code
id="ssmincidents_update_response_plan_:_incidentTemplateTitle">incidentTemplateTitle</code></td>
<td><p>The short format name of the incident. The title can't contain
spaces.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssmincidents_update_response_plan_:_integrations">integrations</code></td>
<td><p>Information about third-party services integrated into the
response plan.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$update_response_plan(
      actions = list(
        list(
          ssmAutomation = list(
            documentName = "string",
            documentVersion = "string",
            dynamicParameters = list(
              list(
                variable = "INCIDENT_RECORD_ARN"|"INVOLVED_RESOURCES"
              )
            ),
            parameters = list(
              list(
                "string"
              )
            ),
            roleArn = "string",
            targetAccount = "RESPONSE_PLAN_OWNER_ACCOUNT"|"IMPACTED_ACCOUNT"
          )
        )
      ),
      arn = "string",
      chatChannel = list(
        chatbotSns = list(
          "string"
        ),
        empty = list()
      ),
      clientToken = "string",
      displayName = "string",
      engagements = list(
        "string"
      ),
      incidentTemplateDedupeString = "string",
      incidentTemplateImpact = 123,
      incidentTemplateNotificationTargets = list(
        list(
          snsTopicArn = "string"
        )
      ),
      incidentTemplateSummary = "string",
      incidentTemplateTags = list(
        "string"
      ),
      incidentTemplateTitle = "string",
      integrations = list(
        list(
          pagerDutyConfiguration = list(
            name = "string",
            pagerDutyIncidentConfiguration = list(
              serviceId = "string"
            ),
            secretId = "string"
          )
        )
      )
    )