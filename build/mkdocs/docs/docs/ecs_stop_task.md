<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ecs_stop_task</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Stops a running task

### Description

Stops a running task. Any tags associated with the task will be deleted.

When `stop_task` is called on a task, the equivalent of `⁠docker stop⁠` is
issued to the containers running in the task. This results in a
`SIGTERM` value and a default 30-second timeout, after which the
`SIGKILL` value is sent and the containers are forcibly stopped. If the
container handles the `SIGTERM` value gracefully and exits within 30
seconds from receiving it, no `SIGKILL` value is sent.

The default 30-second timeout can be configured on the Amazon ECS
container agent with the `ECS_CONTAINER_STOP_TIMEOUT` variable. For more
information, see [Amazon ECS Container Agent
Configuration](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html)
in the *Amazon Elastic Container Service Developer Guide*.

### Usage

    ecs_stop_task(cluster, task, reason)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="ecs_stop_task_:_cluster">cluster</code></td>
<td><p>The short name or full Amazon Resource Name (ARN) of the cluster
that hosts the task to stop. If you do not specify a cluster, the
default cluster is assumed.</p></td>
</tr>
<tr class="even">
<td><code id="ecs_stop_task_:_task">task</code></td>
<td><p>[required] The task ID of the task to stop.</p></td>
</tr>
<tr class="odd">
<td><code id="ecs_stop_task_:_reason">reason</code></td>
<td><p>An optional message specified when a task is stopped. For
example, if you're using a custom scheduler, you can use this parameter
to specify the reason for stopping the task here, and the message
appears in subsequent <code>describe_tasks</code> API operations on this
task. Up to 255 characters are allowed in this message.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      task = list(
        attachments = list(
          list(
            id = "string",
            type = "string",
            status = "string",
            details = list(
              list(
                name = "string",
                value = "string"
              )
            )
          )
        ),
        attributes = list(
          list(
            name = "string",
            value = "string",
            targetType = "container-instance",
            targetId = "string"
          )
        ),
        availabilityZone = "string",
        capacityProviderName = "string",
        clusterArn = "string",
        connectivity = "CONNECTED"|"DISCONNECTED",
        connectivityAt = as.POSIXct(
          "2015-01-01"
        ),
        containerInstanceArn = "string",
        containers = list(
          list(
            containerArn = "string",
            taskArn = "string",
            name = "string",
            image = "string",
            imageDigest = "string",
            runtimeId = "string",
            lastStatus = "string",
            exitCode = 123,
            reason = "string",
            networkBindings = list(
              list(
                bindIP = "string",
                containerPort = 123,
                hostPort = 123,
                protocol = "tcp"|"udp",
                containerPortRange = "string",
                hostPortRange = "string"
              )
            ),
            networkInterfaces = list(
              list(
                attachmentId = "string",
                privateIpv4Address = "string",
                ipv6Address = "string"
              )
            ),
            healthStatus = "HEALTHY"|"UNHEALTHY"|"UNKNOWN",
            managedAgents = list(
              list(
                lastStartedAt = as.POSIXct(
                  "2015-01-01"
                ),
                name = "ExecuteCommandAgent",
                reason = "string",
                lastStatus = "string"
              )
            ),
            cpu = "string",
            memory = "string",
            memoryReservation = "string",
            gpuIds = list(
              "string"
            )
          )
        ),
        cpu = "string",
        createdAt = as.POSIXct(
          "2015-01-01"
        ),
        desiredStatus = "string",
        enableExecuteCommand = TRUE|FALSE,
        executionStoppedAt = as.POSIXct(
          "2015-01-01"
        ),
        group = "string",
        healthStatus = "HEALTHY"|"UNHEALTHY"|"UNKNOWN",
        inferenceAccelerators = list(
          list(
            deviceName = "string",
            deviceType = "string"
          )
        ),
        lastStatus = "string",
        launchType = "EC2"|"FARGATE"|"EXTERNAL",
        memory = "string",
        overrides = list(
          containerOverrides = list(
            list(
              name = "string",
              command = list(
                "string"
              ),
              environment = list(
                list(
                  name = "string",
                  value = "string"
                )
              ),
              environmentFiles = list(
                list(
                  value = "string",
                  type = "s3"
                )
              ),
              cpu = 123,
              memory = 123,
              memoryReservation = 123,
              resourceRequirements = list(
                list(
                  value = "string",
                  type = "GPU"|"InferenceAccelerator"
                )
              )
            )
          ),
          cpu = "string",
          inferenceAcceleratorOverrides = list(
            list(
              deviceName = "string",
              deviceType = "string"
            )
          ),
          executionRoleArn = "string",
          memory = "string",
          taskRoleArn = "string",
          ephemeralStorage = list(
            sizeInGiB = 123
          )
        ),
        platformVersion = "string",
        platformFamily = "string",
        pullStartedAt = as.POSIXct(
          "2015-01-01"
        ),
        pullStoppedAt = as.POSIXct(
          "2015-01-01"
        ),
        startedAt = as.POSIXct(
          "2015-01-01"
        ),
        startedBy = "string",
        stopCode = "TaskFailedToStart"|"EssentialContainerExited"|"UserInitiated"|"ServiceSchedulerInitiated"|"SpotInterruption"|"TerminationNotice",
        stoppedAt = as.POSIXct(
          "2015-01-01"
        ),
        stoppedReason = "string",
        stoppingAt = as.POSIXct(
          "2015-01-01"
        ),
        tags = list(
          list(
            key = "string",
            value = "string"
          )
        ),
        taskArn = "string",
        taskDefinitionArn = "string",
        version = 123,
        ephemeralStorage = list(
          sizeInGiB = 123
        )
      )
    )

### Request syntax

    svc$stop_task(
      cluster = "string",
      task = "string",
      reason = "string"
    )