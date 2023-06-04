<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>codedeploy_batch_get_deployment_groups</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets information about one or more deployment groups

### Description

Gets information about one or more deployment groups.

### Usage

    codedeploy_batch_get_deployment_groups(applicationName,
      deploymentGroupNames)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="codedeploy_batch_get_deployment_groups_:_applicationName">applicationName</code></td>
<td><p>[required] The name of an CodeDeploy application associated with
the applicable IAM or Amazon Web Services account.</p></td>
</tr>
<tr class="even">
<td><code
id="codedeploy_batch_get_deployment_groups_:_deploymentGroupNames">deploymentGroupNames</code></td>
<td><p>[required] The names of the deployment groups.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      deploymentGroupsInfo = list(
        list(
          applicationName = "string",
          deploymentGroupId = "string",
          deploymentGroupName = "string",
          deploymentConfigName = "string",
          ec2TagFilters = list(
            list(
              Key = "string",
              Value = "string",
              Type = "KEY_ONLY"|"VALUE_ONLY"|"KEY_AND_VALUE"
            )
          ),
          onPremisesInstanceTagFilters = list(
            list(
              Key = "string",
              Value = "string",
              Type = "KEY_ONLY"|"VALUE_ONLY"|"KEY_AND_VALUE"
            )
          ),
          autoScalingGroups = list(
            list(
              name = "string",
              hook = "string"
            )
          ),
          serviceRoleArn = "string",
          targetRevision = list(
            revisionType = "S3"|"GitHub"|"String"|"AppSpecContent",
            s3Location = list(
              bucket = "string",
              key = "string",
              bundleType = "tar"|"tgz"|"zip"|"YAML"|"JSON",
              version = "string",
              eTag = "string"
            ),
            gitHubLocation = list(
              repository = "string",
              commitId = "string"
            ),
            string = list(
              content = "string",
              sha256 = "string"
            ),
            appSpecContent = list(
              content = "string",
              sha256 = "string"
            )
          ),
          triggerConfigurations = list(
            list(
              triggerName = "string",
              triggerTargetArn = "string",
              triggerEvents = list(
                "DeploymentStart"|"DeploymentSuccess"|"DeploymentFailure"|"DeploymentStop"|"DeploymentRollback"|"DeploymentReady"|"InstanceStart"|"InstanceSuccess"|"InstanceFailure"|"InstanceReady"
              )
            )
          ),
          alarmConfiguration = list(
            enabled = TRUE|FALSE,
            ignorePollAlarmFailure = TRUE|FALSE,
            alarms = list(
              list(
                name = "string"
              )
            )
          ),
          autoRollbackConfiguration = list(
            enabled = TRUE|FALSE,
            events = list(
              "DEPLOYMENT_FAILURE"|"DEPLOYMENT_STOP_ON_ALARM"|"DEPLOYMENT_STOP_ON_REQUEST"
            )
          ),
          deploymentStyle = list(
            deploymentType = "IN_PLACE"|"BLUE_GREEN",
            deploymentOption = "WITH_TRAFFIC_CONTROL"|"WITHOUT_TRAFFIC_CONTROL"
          ),
          outdatedInstancesStrategy = "UPDATE"|"IGNORE",
          blueGreenDeploymentConfiguration = list(
            terminateBlueInstancesOnDeploymentSuccess = list(
              action = "TERMINATE"|"KEEP_ALIVE",
              terminationWaitTimeInMinutes = 123
            ),
            deploymentReadyOption = list(
              actionOnTimeout = "CONTINUE_DEPLOYMENT"|"STOP_DEPLOYMENT",
              waitTimeInMinutes = 123
            ),
            greenFleetProvisioningOption = list(
              action = "DISCOVER_EXISTING"|"COPY_AUTO_SCALING_GROUP"
            )
          ),
          loadBalancerInfo = list(
            elbInfoList = list(
              list(
                name = "string"
              )
            ),
            targetGroupInfoList = list(
              list(
                name = "string"
              )
            ),
            targetGroupPairInfoList = list(
              list(
                targetGroups = list(
                  list(
                    name = "string"
                  )
                ),
                prodTrafficRoute = list(
                  listenerArns = list(
                    "string"
                  )
                ),
                testTrafficRoute = list(
                  listenerArns = list(
                    "string"
                  )
                )
              )
            )
          ),
          lastSuccessfulDeployment = list(
            deploymentId = "string",
            status = "Created"|"Queued"|"InProgress"|"Baking"|"Succeeded"|"Failed"|"Stopped"|"Ready",
            endTime = as.POSIXct(
              "2015-01-01"
            ),
            createTime = as.POSIXct(
              "2015-01-01"
            )
          ),
          lastAttemptedDeployment = list(
            deploymentId = "string",
            status = "Created"|"Queued"|"InProgress"|"Baking"|"Succeeded"|"Failed"|"Stopped"|"Ready",
            endTime = as.POSIXct(
              "2015-01-01"
            ),
            createTime = as.POSIXct(
              "2015-01-01"
            )
          ),
          ec2TagSet = list(
            ec2TagSetList = list(
              list(
                list(
                  Key = "string",
                  Value = "string",
                  Type = "KEY_ONLY"|"VALUE_ONLY"|"KEY_AND_VALUE"
                )
              )
            )
          ),
          onPremisesTagSet = list(
            onPremisesTagSetList = list(
              list(
                list(
                  Key = "string",
                  Value = "string",
                  Type = "KEY_ONLY"|"VALUE_ONLY"|"KEY_AND_VALUE"
                )
              )
            )
          ),
          computePlatform = "Server"|"Lambda"|"ECS",
          ecsServices = list(
            list(
              serviceName = "string",
              clusterName = "string"
            )
          )
        )
      ),
      errorMessage = "string"
    )

### Request syntax

    svc$batch_get_deployment_groups(
      applicationName = "string",
      deploymentGroupNames = list(
        "string"
      )
    )