<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>swf_get_workflow_execution_history</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns the history of the specified workflow execution

### Description

Returns the history of the specified workflow execution. The results may
be split into multiple pages. To retrieve subsequent pages, make the
call again using the `nextPageToken` returned by the initial call.

This operation is eventually consistent. The results are best effort and
may not exactly reflect recent updates and changes.

**Access Control**

You can use IAM policies to control this action's access to Amazon SWF
resources as follows:

-   Use a `Resource` element with the domain name to limit the action to
    only specified domains.

-   Use an `Action` element to allow or deny permission to call this
    action.

-   You cannot use an IAM policy to constrain this action's parameters.

If the caller doesn't have sufficient permissions to invoke the action,
or the parameter values fall outside the specified constraints, the
action fails. The associated event attribute's `cause` parameter is set
to `OPERATION_NOT_PERMITTED`. For details and example IAM policies, see
[Using IAM to Manage Access to Amazon SWF
Workflows](https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html)
in the *Amazon SWF Developer Guide*.

### Usage

    swf_get_workflow_execution_history(domain, execution, nextPageToken,
      maximumPageSize, reverseOrder)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="swf_get_workflow_execution_history_:_domain">domain</code></td>
<td><p>[required] The name of the domain containing the workflow
execution.</p></td>
</tr>
<tr class="even">
<td><code
id="swf_get_workflow_execution_history_:_execution">execution</code></td>
<td><p>[required] Specifies the workflow execution for which to return
the history.</p></td>
</tr>
<tr class="odd">
<td><code
id="swf_get_workflow_execution_history_:_nextPageToken">nextPageToken</code></td>
<td><p>If <code>NextPageToken</code> is returned there are more results
available. The value of <code>NextPageToken</code> is a unique
pagination token for each page. Make the call again using the returned
token to retrieve the next page. Keep all other arguments unchanged.
Each pagination token expires after 24 hours. Using an expired
pagination token will return a <code>400</code> error: "<code
style="white-space: pre;">⁠Specified token has exceeded its maximum lifetime⁠</code>".</p>
<p>The configured <code>maximumPageSize</code> determines how many
results can be returned in a single call.</p></td>
</tr>
<tr class="even">
<td><code
id="swf_get_workflow_execution_history_:_maximumPageSize">maximumPageSize</code></td>
<td><p>The maximum number of results that are returned per call. Use
<code>nextPageToken</code> to obtain further pages of results.</p></td>
</tr>
<tr class="odd">
<td><code
id="swf_get_workflow_execution_history_:_reverseOrder">reverseOrder</code></td>
<td><p>When set to <code>true</code>, returns the events in reverse
order. By default the results are returned in ascending order of the
<code>eventTimeStamp</code> of the events.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      events = list(
        list(
          eventTimestamp = as.POSIXct(
            "2015-01-01"
          ),
          eventType = "WorkflowExecutionStarted"|"WorkflowExecutionCancelRequested"|"WorkflowExecutionCompleted"|"CompleteWorkflowExecutionFailed"|"WorkflowExecutionFailed"|"FailWorkflowExecutionFailed"|"WorkflowExecutionTimedOut"|"WorkflowExecutionCanceled"|"CancelWorkflowExecutionFailed"|"WorkflowExecutionContinuedAsNew"|"ContinueAsNewWorkflowExecutionFailed"|"WorkflowExecutionTerminated"|"DecisionTaskScheduled"|"DecisionTaskStarted"|"DecisionTaskCompleted"|"DecisionTaskTimedOut"|"ActivityTaskScheduled"|"ScheduleActivityTaskFailed"|"ActivityTaskStarted"|"ActivityTaskCompleted"|"ActivityTaskFailed"|"ActivityTaskTimedOut"|"ActivityTaskCanceled"|"ActivityTaskCancelRequested"|"RequestCancelActivityTaskFailed"|"WorkflowExecutionSignaled"|"MarkerRecorded"|"RecordMarkerFailed"|"TimerStarted"|"StartTimerFailed"|"TimerFired"|"TimerCanceled"|"CancelTimerFailed"|"StartChildWorkflowExecutionInitiated"|"StartChildWorkflowExecutionFailed"|"ChildWorkflowExecutionStarted"|"ChildWorkflowExecutionCompleted"|"ChildWorkflowExecutionFailed"|"ChildWorkflowExecutionTimedOut"|"ChildWorkflowExecutionCanceled"|"ChildWorkflowExecutionTerminated"|"SignalExternalWorkflowExecutionInitiated"|"SignalExternalWorkflowExecutionFailed"|"ExternalWorkflowExecutionSignaled"|"RequestCancelExternalWorkflowExecutionInitiated"|"RequestCancelExternalWorkflowExecutionFailed"|"ExternalWorkflowExecutionCancelRequested"|"LambdaFunctionScheduled"|"LambdaFunctionStarted"|"LambdaFunctionCompleted"|"LambdaFunctionFailed"|"LambdaFunctionTimedOut"|"ScheduleLambdaFunctionFailed"|"StartLambdaFunctionFailed",
          eventId = 123,
          workflowExecutionStartedEventAttributes = list(
            input = "string",
            executionStartToCloseTimeout = "string",
            taskStartToCloseTimeout = "string",
            childPolicy = "TERMINATE"|"REQUEST_CANCEL"|"ABANDON",
            taskList = list(
              name = "string"
            ),
            taskPriority = "string",
            workflowType = list(
              name = "string",
              version = "string"
            ),
            tagList = list(
              "string"
            ),
            continuedExecutionRunId = "string",
            parentWorkflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            parentInitiatedEventId = 123,
            lambdaRole = "string"
          ),
          workflowExecutionCompletedEventAttributes = list(
            result = "string",
            decisionTaskCompletedEventId = 123
          ),
          completeWorkflowExecutionFailedEventAttributes = list(
            cause = "UNHANDLED_DECISION"|"OPERATION_NOT_PERMITTED",
            decisionTaskCompletedEventId = 123
          ),
          workflowExecutionFailedEventAttributes = list(
            reason = "string",
            details = "string",
            decisionTaskCompletedEventId = 123
          ),
          failWorkflowExecutionFailedEventAttributes = list(
            cause = "UNHANDLED_DECISION"|"OPERATION_NOT_PERMITTED",
            decisionTaskCompletedEventId = 123
          ),
          workflowExecutionTimedOutEventAttributes = list(
            timeoutType = "START_TO_CLOSE",
            childPolicy = "TERMINATE"|"REQUEST_CANCEL"|"ABANDON"
          ),
          workflowExecutionCanceledEventAttributes = list(
            details = "string",
            decisionTaskCompletedEventId = 123
          ),
          cancelWorkflowExecutionFailedEventAttributes = list(
            cause = "UNHANDLED_DECISION"|"OPERATION_NOT_PERMITTED",
            decisionTaskCompletedEventId = 123
          ),
          workflowExecutionContinuedAsNewEventAttributes = list(
            input = "string",
            decisionTaskCompletedEventId = 123,
            newExecutionRunId = "string",
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
            workflowType = list(
              name = "string",
              version = "string"
            ),
            lambdaRole = "string"
          ),
          continueAsNewWorkflowExecutionFailedEventAttributes = list(
            cause = "UNHANDLED_DECISION"|"WORKFLOW_TYPE_DEPRECATED"|"WORKFLOW_TYPE_DOES_NOT_EXIST"|"DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"|"DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"|"DEFAULT_TASK_LIST_UNDEFINED"|"DEFAULT_CHILD_POLICY_UNDEFINED"|"CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED"|"OPERATION_NOT_PERMITTED",
            decisionTaskCompletedEventId = 123
          ),
          workflowExecutionTerminatedEventAttributes = list(
            reason = "string",
            details = "string",
            childPolicy = "TERMINATE"|"REQUEST_CANCEL"|"ABANDON",
            cause = "CHILD_POLICY_APPLIED"|"EVENT_LIMIT_EXCEEDED"|"OPERATOR_INITIATED"
          ),
          workflowExecutionCancelRequestedEventAttributes = list(
            externalWorkflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            externalInitiatedEventId = 123,
            cause = "CHILD_POLICY_APPLIED"
          ),
          decisionTaskScheduledEventAttributes = list(
            taskList = list(
              name = "string"
            ),
            taskPriority = "string",
            startToCloseTimeout = "string"
          ),
          decisionTaskStartedEventAttributes = list(
            identity = "string",
            scheduledEventId = 123
          ),
          decisionTaskCompletedEventAttributes = list(
            executionContext = "string",
            scheduledEventId = 123,
            startedEventId = 123
          ),
          decisionTaskTimedOutEventAttributes = list(
            timeoutType = "START_TO_CLOSE",
            scheduledEventId = 123,
            startedEventId = 123
          ),
          activityTaskScheduledEventAttributes = list(
            activityType = list(
              name = "string",
              version = "string"
            ),
            activityId = "string",
            input = "string",
            control = "string",
            scheduleToStartTimeout = "string",
            scheduleToCloseTimeout = "string",
            startToCloseTimeout = "string",
            taskList = list(
              name = "string"
            ),
            taskPriority = "string",
            decisionTaskCompletedEventId = 123,
            heartbeatTimeout = "string"
          ),
          activityTaskStartedEventAttributes = list(
            identity = "string",
            scheduledEventId = 123
          ),
          activityTaskCompletedEventAttributes = list(
            result = "string",
            scheduledEventId = 123,
            startedEventId = 123
          ),
          activityTaskFailedEventAttributes = list(
            reason = "string",
            details = "string",
            scheduledEventId = 123,
            startedEventId = 123
          ),
          activityTaskTimedOutEventAttributes = list(
            timeoutType = "START_TO_CLOSE"|"SCHEDULE_TO_START"|"SCHEDULE_TO_CLOSE"|"HEARTBEAT",
            scheduledEventId = 123,
            startedEventId = 123,
            details = "string"
          ),
          activityTaskCanceledEventAttributes = list(
            details = "string",
            scheduledEventId = 123,
            startedEventId = 123,
            latestCancelRequestedEventId = 123
          ),
          activityTaskCancelRequestedEventAttributes = list(
            decisionTaskCompletedEventId = 123,
            activityId = "string"
          ),
          workflowExecutionSignaledEventAttributes = list(
            signalName = "string",
            input = "string",
            externalWorkflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            externalInitiatedEventId = 123
          ),
          markerRecordedEventAttributes = list(
            markerName = "string",
            details = "string",
            decisionTaskCompletedEventId = 123
          ),
          recordMarkerFailedEventAttributes = list(
            markerName = "string",
            cause = "OPERATION_NOT_PERMITTED",
            decisionTaskCompletedEventId = 123
          ),
          timerStartedEventAttributes = list(
            timerId = "string",
            control = "string",
            startToFireTimeout = "string",
            decisionTaskCompletedEventId = 123
          ),
          timerFiredEventAttributes = list(
            timerId = "string",
            startedEventId = 123
          ),
          timerCanceledEventAttributes = list(
            timerId = "string",
            startedEventId = 123,
            decisionTaskCompletedEventId = 123
          ),
          startChildWorkflowExecutionInitiatedEventAttributes = list(
            workflowId = "string",
            workflowType = list(
              name = "string",
              version = "string"
            ),
            control = "string",
            input = "string",
            executionStartToCloseTimeout = "string",
            taskList = list(
              name = "string"
            ),
            taskPriority = "string",
            decisionTaskCompletedEventId = 123,
            childPolicy = "TERMINATE"|"REQUEST_CANCEL"|"ABANDON",
            taskStartToCloseTimeout = "string",
            tagList = list(
              "string"
            ),
            lambdaRole = "string"
          ),
          childWorkflowExecutionStartedEventAttributes = list(
            workflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            workflowType = list(
              name = "string",
              version = "string"
            ),
            initiatedEventId = 123
          ),
          childWorkflowExecutionCompletedEventAttributes = list(
            workflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            workflowType = list(
              name = "string",
              version = "string"
            ),
            result = "string",
            initiatedEventId = 123,
            startedEventId = 123
          ),
          childWorkflowExecutionFailedEventAttributes = list(
            workflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            workflowType = list(
              name = "string",
              version = "string"
            ),
            reason = "string",
            details = "string",
            initiatedEventId = 123,
            startedEventId = 123
          ),
          childWorkflowExecutionTimedOutEventAttributes = list(
            workflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            workflowType = list(
              name = "string",
              version = "string"
            ),
            timeoutType = "START_TO_CLOSE",
            initiatedEventId = 123,
            startedEventId = 123
          ),
          childWorkflowExecutionCanceledEventAttributes = list(
            workflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            workflowType = list(
              name = "string",
              version = "string"
            ),
            details = "string",
            initiatedEventId = 123,
            startedEventId = 123
          ),
          childWorkflowExecutionTerminatedEventAttributes = list(
            workflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            workflowType = list(
              name = "string",
              version = "string"
            ),
            initiatedEventId = 123,
            startedEventId = 123
          ),
          signalExternalWorkflowExecutionInitiatedEventAttributes = list(
            workflowId = "string",
            runId = "string",
            signalName = "string",
            input = "string",
            decisionTaskCompletedEventId = 123,
            control = "string"
          ),
          externalWorkflowExecutionSignaledEventAttributes = list(
            workflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            initiatedEventId = 123
          ),
          signalExternalWorkflowExecutionFailedEventAttributes = list(
            workflowId = "string",
            runId = "string",
            cause = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"|"SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"|"OPERATION_NOT_PERMITTED",
            initiatedEventId = 123,
            decisionTaskCompletedEventId = 123,
            control = "string"
          ),
          externalWorkflowExecutionCancelRequestedEventAttributes = list(
            workflowExecution = list(
              workflowId = "string",
              runId = "string"
            ),
            initiatedEventId = 123
          ),
          requestCancelExternalWorkflowExecutionInitiatedEventAttributes = list(
            workflowId = "string",
            runId = "string",
            decisionTaskCompletedEventId = 123,
            control = "string"
          ),
          requestCancelExternalWorkflowExecutionFailedEventAttributes = list(
            workflowId = "string",
            runId = "string",
            cause = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"|"REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"|"OPERATION_NOT_PERMITTED",
            initiatedEventId = 123,
            decisionTaskCompletedEventId = 123,
            control = "string"
          ),
          scheduleActivityTaskFailedEventAttributes = list(
            activityType = list(
              name = "string",
              version = "string"
            ),
            activityId = "string",
            cause = "ACTIVITY_TYPE_DEPRECATED"|"ACTIVITY_TYPE_DOES_NOT_EXIST"|"ACTIVITY_ID_ALREADY_IN_USE"|"OPEN_ACTIVITIES_LIMIT_EXCEEDED"|"ACTIVITY_CREATION_RATE_EXCEEDED"|"DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"|"DEFAULT_TASK_LIST_UNDEFINED"|"DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"|"DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"|"DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"|"OPERATION_NOT_PERMITTED",
            decisionTaskCompletedEventId = 123
          ),
          requestCancelActivityTaskFailedEventAttributes = list(
            activityId = "string",
            cause = "ACTIVITY_ID_UNKNOWN"|"OPERATION_NOT_PERMITTED",
            decisionTaskCompletedEventId = 123
          ),
          startTimerFailedEventAttributes = list(
            timerId = "string",
            cause = "TIMER_ID_ALREADY_IN_USE"|"OPEN_TIMERS_LIMIT_EXCEEDED"|"TIMER_CREATION_RATE_EXCEEDED"|"OPERATION_NOT_PERMITTED",
            decisionTaskCompletedEventId = 123
          ),
          cancelTimerFailedEventAttributes = list(
            timerId = "string",
            cause = "TIMER_ID_UNKNOWN"|"OPERATION_NOT_PERMITTED",
            decisionTaskCompletedEventId = 123
          ),
          startChildWorkflowExecutionFailedEventAttributes = list(
            workflowType = list(
              name = "string",
              version = "string"
            ),
            cause = "WORKFLOW_TYPE_DOES_NOT_EXIST"|"WORKFLOW_TYPE_DEPRECATED"|"OPEN_CHILDREN_LIMIT_EXCEEDED"|"OPEN_WORKFLOWS_LIMIT_EXCEEDED"|"CHILD_CREATION_RATE_EXCEEDED"|"WORKFLOW_ALREADY_RUNNING"|"DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"|"DEFAULT_TASK_LIST_UNDEFINED"|"DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"|"DEFAULT_CHILD_POLICY_UNDEFINED"|"OPERATION_NOT_PERMITTED",
            workflowId = "string",
            initiatedEventId = 123,
            decisionTaskCompletedEventId = 123,
            control = "string"
          ),
          lambdaFunctionScheduledEventAttributes = list(
            id = "string",
            name = "string",
            control = "string",
            input = "string",
            startToCloseTimeout = "string",
            decisionTaskCompletedEventId = 123
          ),
          lambdaFunctionStartedEventAttributes = list(
            scheduledEventId = 123
          ),
          lambdaFunctionCompletedEventAttributes = list(
            scheduledEventId = 123,
            startedEventId = 123,
            result = "string"
          ),
          lambdaFunctionFailedEventAttributes = list(
            scheduledEventId = 123,
            startedEventId = 123,
            reason = "string",
            details = "string"
          ),
          lambdaFunctionTimedOutEventAttributes = list(
            scheduledEventId = 123,
            startedEventId = 123,
            timeoutType = "START_TO_CLOSE"
          ),
          scheduleLambdaFunctionFailedEventAttributes = list(
            id = "string",
            name = "string",
            cause = "ID_ALREADY_IN_USE"|"OPEN_LAMBDA_FUNCTIONS_LIMIT_EXCEEDED"|"LAMBDA_FUNCTION_CREATION_RATE_EXCEEDED"|"LAMBDA_SERVICE_NOT_AVAILABLE_IN_REGION",
            decisionTaskCompletedEventId = 123
          ),
          startLambdaFunctionFailedEventAttributes = list(
            scheduledEventId = 123,
            cause = "ASSUME_ROLE_FAILED",
            message = "string"
          )
        )
      ),
      nextPageToken = "string"
    )

### Request syntax

    svc$get_workflow_execution_history(
      domain = "string",
      execution = list(
        workflowId = "string",
        runId = "string"
      ),
      nextPageToken = "string",
      maximumPageSize = 123,
      reverseOrder = TRUE|FALSE
    )