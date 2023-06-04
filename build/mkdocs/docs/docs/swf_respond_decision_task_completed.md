<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>swf_respond_decision_task_completed</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Used by deciders to tell the service that the DecisionTask identified by the taskToken has successfully completed

### Description

Used by deciders to tell the service that the DecisionTask identified by
the `taskToken` has successfully completed. The `decisions` argument
specifies the list of decisions made while processing the task.

A `DecisionTaskCompleted` event is added to the workflow history. The
`executionContext` specified is attached to the event in the workflow
execution history.

**Access Control**

If an IAM policy grants permission to use
`respond_decision_task_completed`, it can express permissions for the
list of decisions in the `decisions` parameter. Each of the decisions
has one or more parameters, much like a regular API call. To allow for
policies to be as readable as possible, you can express permissions on
decisions as if they were actual API calls, including applying
conditions to some parameters. For more information, see [Using IAM to
Manage Access to Amazon SWF
Workflows](https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html)
in the *Amazon SWF Developer Guide*.

### Usage

    swf_respond_decision_task_completed(taskToken, decisions,
      executionContext)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="swf_respond_decision_task_completed_:_taskToken">taskToken</code></td>
<td><p>[required] The <code>taskToken</code> from the DecisionTask.</p>
<p><code>taskToken</code> is generated by the service and should be
treated as an opaque value. If the task is passed to another process,
its <code>taskToken</code> must also be passed. This enables it to
provide its progress and respond with results.</p></td>
</tr>
<tr class="even">
<td><code
id="swf_respond_decision_task_completed_:_decisions">decisions</code></td>
<td><p>The list of decisions (possibly empty) made by the decider while
processing this decision task. See the docs for the Decision structure
for details.</p></td>
</tr>
<tr class="odd">
<td><code
id="swf_respond_decision_task_completed_:_executionContext">executionContext</code></td>
<td><p>User defined context to add to workflow execution.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$respond_decision_task_completed(
      taskToken = "string",
      decisions = list(
        list(
          decisionType = "ScheduleActivityTask"|"RequestCancelActivityTask"|"CompleteWorkflowExecution"|"FailWorkflowExecution"|"CancelWorkflowExecution"|"ContinueAsNewWorkflowExecution"|"RecordMarker"|"StartTimer"|"CancelTimer"|"SignalExternalWorkflowExecution"|"RequestCancelExternalWorkflowExecution"|"StartChildWorkflowExecution"|"ScheduleLambdaFunction",
          scheduleActivityTaskDecisionAttributes = list(
            activityType = list(
              name = "string",
              version = "string"
            ),
            activityId = "string",
            control = "string",
            input = "string",
            scheduleToCloseTimeout = "string",
            taskList = list(
              name = "string"
            ),
            taskPriority = "string",
            scheduleToStartTimeout = "string",
            startToCloseTimeout = "string",
            heartbeatTimeout = "string"
          ),
          requestCancelActivityTaskDecisionAttributes = list(
            activityId = "string"
          ),
          completeWorkflowExecutionDecisionAttributes = list(
            result = "string"
          ),
          failWorkflowExecutionDecisionAttributes = list(
            reason = "string",
            details = "string"
          ),
          cancelWorkflowExecutionDecisionAttributes = list(
            details = "string"
          ),
          continueAsNewWorkflowExecutionDecisionAttributes = list(
            input = "string",
            executionStartToCloseTimeout = "string",
            taskList = list(
              name = "string"
            ),
            taskPriority = "string",
            taskStartToCloseTimeout = "string",
            childPolicy = "TERMINATE"|"REQUEST_CANCEL"|"ABANDON",
            tagList = list(
              "string"
            ),
            workflowTypeVersion = "string",
            lambdaRole = "string"
          ),
          recordMarkerDecisionAttributes = list(
            markerName = "string",
            details = "string"
          ),
          startTimerDecisionAttributes = list(
            timerId = "string",
            control = "string",
            startToFireTimeout = "string"
          ),
          cancelTimerDecisionAttributes = list(
            timerId = "string"
          ),
          signalExternalWorkflowExecutionDecisionAttributes = list(
            workflowId = "string",
            runId = "string",
            signalName = "string",
            input = "string",
            control = "string"
          ),
          requestCancelExternalWorkflowExecutionDecisionAttributes = list(
            workflowId = "string",
            runId = "string",
            control = "string"
          ),
          startChildWorkflowExecutionDecisionAttributes = list(
            workflowType = list(
              name = "string",
              version = "string"
            ),
            workflowId = "string",
            control = "string",
            input = "string",
            executionStartToCloseTimeout = "string",
            taskList = list(
              name = "string"
            ),
            taskPriority = "string",
            taskStartToCloseTimeout = "string",
            childPolicy = "TERMINATE"|"REQUEST_CANCEL"|"ABANDON",
            tagList = list(
              "string"
            ),
            lambdaRole = "string"
          ),
          scheduleLambdaFunctionDecisionAttributes = list(
            id = "string",
            name = "string",
            control = "string",
            input = "string",
            startToCloseTimeout = "string"
          )
        )
      ),
      executionContext = "string"
    )