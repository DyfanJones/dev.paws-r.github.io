<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>codebuild_batch_get_builds</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets information about one or more builds

### Description

Gets information about one or more builds.

### Usage

    codebuild_batch_get_builds(ids)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="codebuild_batch_get_builds_:_ids">ids</code></td>
<td><p>[required] The IDs of the builds.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      builds = list(
        list(
          id = "string",
          arn = "string",
          buildNumber = 123,
          startTime = as.POSIXct(
            "2015-01-01"
          ),
          endTime = as.POSIXct(
            "2015-01-01"
          ),
          currentPhase = "string",
          buildStatus = "SUCCEEDED"|"FAILED"|"FAULT"|"TIMED_OUT"|"IN_PROGRESS"|"STOPPED",
          sourceVersion = "string",
          resolvedSourceVersion = "string",
          projectName = "string",
          phases = list(
            list(
              phaseType = "SUBMITTED"|"QUEUED"|"PROVISIONING"|"DOWNLOAD_SOURCE"|"INSTALL"|"PRE_BUILD"|"BUILD"|"POST_BUILD"|"UPLOAD_ARTIFACTS"|"FINALIZING"|"COMPLETED",
              phaseStatus = "SUCCEEDED"|"FAILED"|"FAULT"|"TIMED_OUT"|"IN_PROGRESS"|"STOPPED",
              startTime = as.POSIXct(
                "2015-01-01"
              ),
              endTime = as.POSIXct(
                "2015-01-01"
              ),
              durationInSeconds = 123,
              contexts = list(
                list(
                  statusCode = "string",
                  message = "string"
                )
              )
            )
          ),
          source = list(
            type = "CODECOMMIT"|"CODEPIPELINE"|"GITHUB"|"S3"|"BITBUCKET"|"GITHUB_ENTERPRISE"|"NO_SOURCE",
            location = "string",
            gitCloneDepth = 123,
            gitSubmodulesConfig = list(
              fetchSubmodules = TRUE|FALSE
            ),
            buildspec = "string",
            auth = list(
              type = "OAUTH",
              resource = "string"
            ),
            reportBuildStatus = TRUE|FALSE,
            buildStatusConfig = list(
              context = "string",
              targetUrl = "string"
            ),
            insecureSsl = TRUE|FALSE,
            sourceIdentifier = "string"
          ),
          secondarySources = list(
            list(
              type = "CODECOMMIT"|"CODEPIPELINE"|"GITHUB"|"S3"|"BITBUCKET"|"GITHUB_ENTERPRISE"|"NO_SOURCE",
              location = "string",
              gitCloneDepth = 123,
              gitSubmodulesConfig = list(
                fetchSubmodules = TRUE|FALSE
              ),
              buildspec = "string",
              auth = list(
                type = "OAUTH",
                resource = "string"
              ),
              reportBuildStatus = TRUE|FALSE,
              buildStatusConfig = list(
                context = "string",
                targetUrl = "string"
              ),
              insecureSsl = TRUE|FALSE,
              sourceIdentifier = "string"
            )
          ),
          secondarySourceVersions = list(
            list(
              sourceIdentifier = "string",
              sourceVersion = "string"
            )
          ),
          artifacts = list(
            location = "string",
            sha256sum = "string",
            md5sum = "string",
            overrideArtifactName = TRUE|FALSE,
            encryptionDisabled = TRUE|FALSE,
            artifactIdentifier = "string",
            bucketOwnerAccess = "NONE"|"READ_ONLY"|"FULL"
          ),
          secondaryArtifacts = list(
            list(
              location = "string",
              sha256sum = "string",
              md5sum = "string",
              overrideArtifactName = TRUE|FALSE,
              encryptionDisabled = TRUE|FALSE,
              artifactIdentifier = "string",
              bucketOwnerAccess = "NONE"|"READ_ONLY"|"FULL"
            )
          ),
          cache = list(
            type = "NO_CACHE"|"S3"|"LOCAL",
            location = "string",
            modes = list(
              "LOCAL_DOCKER_LAYER_CACHE"|"LOCAL_SOURCE_CACHE"|"LOCAL_CUSTOM_CACHE"
            )
          ),
          environment = list(
            type = "WINDOWS_CONTAINER"|"LINUX_CONTAINER"|"LINUX_GPU_CONTAINER"|"ARM_CONTAINER"|"WINDOWS_SERVER_2019_CONTAINER",
            image = "string",
            computeType = "BUILD_GENERAL1_SMALL"|"BUILD_GENERAL1_MEDIUM"|"BUILD_GENERAL1_LARGE"|"BUILD_GENERAL1_2XLARGE",
            environmentVariables = list(
              list(
                name = "string",
                value = "string",
                type = "PLAINTEXT"|"PARAMETER_STORE"|"SECRETS_MANAGER"
              )
            ),
            privilegedMode = TRUE|FALSE,
            certificate = "string",
            registryCredential = list(
              credential = "string",
              credentialProvider = "SECRETS_MANAGER"
            ),
            imagePullCredentialsType = "CODEBUILD"|"SERVICE_ROLE"
          ),
          serviceRole = "string",
          logs = list(
            groupName = "string",
            streamName = "string",
            deepLink = "string",
            s3DeepLink = "string",
            cloudWatchLogsArn = "string",
            s3LogsArn = "string",
            cloudWatchLogs = list(
              status = "ENABLED"|"DISABLED",
              groupName = "string",
              streamName = "string"
            ),
            s3Logs = list(
              status = "ENABLED"|"DISABLED",
              location = "string",
              encryptionDisabled = TRUE|FALSE,
              bucketOwnerAccess = "NONE"|"READ_ONLY"|"FULL"
            )
          ),
          timeoutInMinutes = 123,
          queuedTimeoutInMinutes = 123,
          buildComplete = TRUE|FALSE,
          initiator = "string",
          vpcConfig = list(
            vpcId = "string",
            subnets = list(
              "string"
            ),
            securityGroupIds = list(
              "string"
            )
          ),
          networkInterface = list(
            subnetId = "string",
            networkInterfaceId = "string"
          ),
          encryptionKey = "string",
          exportedEnvironmentVariables = list(
            list(
              name = "string",
              value = "string"
            )
          ),
          reportArns = list(
            "string"
          ),
          fileSystemLocations = list(
            list(
              type = "EFS",
              location = "string",
              mountPoint = "string",
              identifier = "string",
              mountOptions = "string"
            )
          ),
          debugSession = list(
            sessionEnabled = TRUE|FALSE,
            sessionTarget = "string"
          ),
          buildBatchArn = "string"
        )
      ),
      buildsNotFound = list(
        "string"
      )
    )

### Request syntax

    svc$batch_get_builds(
      ids = list(
        "string"
      )
    )