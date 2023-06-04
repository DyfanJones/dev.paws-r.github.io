<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>codebuild_batch_get_projects</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets information about one or more build projects

### Description

Gets information about one or more build projects.

### Usage

    codebuild_batch_get_projects(names)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="codebuild_batch_get_projects_:_names">names</code></td>
<td><p>[required] The names or ARNs of the build projects. To get
information about a project shared with your Amazon Web Services
account, its ARN must be specified. You cannot specify a shared project
using its name.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      projects = list(
        list(
          name = "string",
          arn = "string",
          description = "string",
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
          sourceVersion = "string",
          secondarySourceVersions = list(
            list(
              sourceIdentifier = "string",
              sourceVersion = "string"
            )
          ),
          artifacts = list(
            type = "CODEPIPELINE"|"S3"|"NO_ARTIFACTS",
            location = "string",
            path = "string",
            namespaceType = "NONE"|"BUILD_ID",
            name = "string",
            packaging = "NONE"|"ZIP",
            overrideArtifactName = TRUE|FALSE,
            encryptionDisabled = TRUE|FALSE,
            artifactIdentifier = "string",
            bucketOwnerAccess = "NONE"|"READ_ONLY"|"FULL"
          ),
          secondaryArtifacts = list(
            list(
              type = "CODEPIPELINE"|"S3"|"NO_ARTIFACTS",
              location = "string",
              path = "string",
              namespaceType = "NONE"|"BUILD_ID",
              name = "string",
              packaging = "NONE"|"ZIP",
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
          timeoutInMinutes = 123,
          queuedTimeoutInMinutes = 123,
          encryptionKey = "string",
          tags = list(
            list(
              key = "string",
              value = "string"
            )
          ),
          created = as.POSIXct(
            "2015-01-01"
          ),
          lastModified = as.POSIXct(
            "2015-01-01"
          ),
          webhook = list(
            url = "string",
            payloadUrl = "string",
            secret = "string",
            branchFilter = "string",
            filterGroups = list(
              list(
                list(
                  type = "EVENT"|"BASE_REF"|"HEAD_REF"|"ACTOR_ACCOUNT_ID"|"FILE_PATH"|"COMMIT_MESSAGE",
                  pattern = "string",
                  excludeMatchedPattern = TRUE|FALSE
                )
              )
            ),
            buildType = "BUILD"|"BUILD_BATCH",
            lastModifiedSecret = as.POSIXct(
              "2015-01-01"
            )
          ),
          vpcConfig = list(
            vpcId = "string",
            subnets = list(
              "string"
            ),
            securityGroupIds = list(
              "string"
            )
          ),
          badge = list(
            badgeEnabled = TRUE|FALSE,
            badgeRequestUrl = "string"
          ),
          logsConfig = list(
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
          fileSystemLocations = list(
            list(
              type = "EFS",
              location = "string",
              mountPoint = "string",
              identifier = "string",
              mountOptions = "string"
            )
          ),
          buildBatchConfig = list(
            serviceRole = "string",
            combineArtifacts = TRUE|FALSE,
            restrictions = list(
              maximumBuildsAllowed = 123,
              computeTypesAllowed = list(
                "string"
              )
            ),
            timeoutInMins = 123,
            batchReportMode = "REPORT_INDIVIDUAL_BUILDS"|"REPORT_AGGREGATED_BATCH"
          ),
          concurrentBuildLimit = 123,
          projectVisibility = "PUBLIC_READ"|"PRIVATE",
          publicProjectAlias = "string",
          resourceAccessRole = "string"
        )
      ),
      projectsNotFound = list(
        "string"
      )
    )

### Request syntax

    svc$batch_get_projects(
      names = list(
        "string"
      )
    )