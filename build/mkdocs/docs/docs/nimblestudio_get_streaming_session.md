<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>nimblestudio_get_streaming_session</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets StreamingSession resource

### Description

Gets StreamingSession resource.

Invoke this operation to poll for a streaming session state while
creating or deleting a session.

### Usage

    nimblestudio_get_streaming_session(sessionId, studioId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="nimblestudio_get_streaming_session_:_sessionId">sessionId</code></td>
<td><p>[required] The streaming session ID.</p></td>
</tr>
<tr class="even">
<td><code
id="nimblestudio_get_streaming_session_:_studioId">studioId</code></td>
<td><p>[required] The studio ID.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      session = list(
        arn = "string",
        automaticTerminationMode = "DEACTIVATED"|"ACTIVATED",
        backupMode = "AUTOMATIC"|"DEACTIVATED",
        createdAt = as.POSIXct(
          "2015-01-01"
        ),
        createdBy = "string",
        ec2InstanceType = "string",
        launchProfileId = "string",
        maxBackupsToRetain = 123,
        ownedBy = "string",
        sessionId = "string",
        sessionPersistenceMode = "DEACTIVATED"|"ACTIVATED",
        startedAt = as.POSIXct(
          "2015-01-01"
        ),
        startedBy = "string",
        startedFromBackupId = "string",
        state = "CREATE_IN_PROGRESS"|"DELETE_IN_PROGRESS"|"READY"|"DELETED"|"CREATE_FAILED"|"DELETE_FAILED"|"STOP_IN_PROGRESS"|"START_IN_PROGRESS"|"STOPPED"|"STOP_FAILED"|"START_FAILED",
        statusCode = "STREAMING_SESSION_READY"|"STREAMING_SESSION_DELETED"|"STREAMING_SESSION_CREATE_IN_PROGRESS"|"STREAMING_SESSION_DELETE_IN_PROGRESS"|"INTERNAL_ERROR"|"INSUFFICIENT_CAPACITY"|"ACTIVE_DIRECTORY_DOMAIN_JOIN_ERROR"|"NETWORK_CONNECTION_ERROR"|"INITIALIZATION_SCRIPT_ERROR"|"DECRYPT_STREAMING_IMAGE_ERROR"|"NETWORK_INTERFACE_ERROR"|"STREAMING_SESSION_STOPPED"|"STREAMING_SESSION_STARTED"|"STREAMING_SESSION_STOP_IN_PROGRESS"|"STREAMING_SESSION_START_IN_PROGRESS"|"AMI_VALIDATION_ERROR",
        statusMessage = "string",
        stopAt = as.POSIXct(
          "2015-01-01"
        ),
        stoppedAt = as.POSIXct(
          "2015-01-01"
        ),
        stoppedBy = "string",
        streamingImageId = "string",
        tags = list(
          "string"
        ),
        terminateAt = as.POSIXct(
          "2015-01-01"
        ),
        updatedAt = as.POSIXct(
          "2015-01-01"
        ),
        updatedBy = "string",
        volumeConfiguration = list(
          iops = 123,
          size = 123,
          throughput = 123
        ),
        volumeRetentionMode = "RETAIN"|"DELETE"
      )
    )

### Request syntax

    svc$get_streaming_session(
      sessionId = "string",
      studioId = "string"
    )