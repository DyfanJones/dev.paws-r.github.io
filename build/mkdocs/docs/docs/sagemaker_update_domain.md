<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>sagemaker_update_domain</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Updates the default settings for new user profiles in the domain

### Description

Updates the default settings for new user profiles in the domain.

### Usage

    sagemaker_update_domain(DomainId, DefaultUserSettings,
      DomainSettingsForUpdate, DefaultSpaceSettings,
      AppSecurityGroupManagement)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="sagemaker_update_domain_:_DomainId">DomainId</code></td>
<td><p>[required] The ID of the domain to be updated.</p></td>
</tr>
<tr class="even">
<td><code
id="sagemaker_update_domain_:_DefaultUserSettings">DefaultUserSettings</code></td>
<td><p>A collection of settings.</p></td>
</tr>
<tr class="odd">
<td><code
id="sagemaker_update_domain_:_DomainSettingsForUpdate">DomainSettingsForUpdate</code></td>
<td><p>A collection of <code>DomainSettings</code> configuration values
to update.</p></td>
</tr>
<tr class="even">
<td><code
id="sagemaker_update_domain_:_DefaultSpaceSettings">DefaultSpaceSettings</code></td>
<td><p>The default settings used to create a space within the
Domain.</p></td>
</tr>
<tr class="odd">
<td><code
id="sagemaker_update_domain_:_AppSecurityGroupManagement">AppSecurityGroupManagement</code></td>
<td><p>The entity that creates and manages the required security groups
for inter-app communication in <code>VPCOnly</code> mode. Required when
<code>CreateDomain.AppNetworkAccessType</code> is <code>VPCOnly</code>
and
<code>DomainSettings.RStudioServerProDomainSettings.DomainExecutionRoleArn</code>
is provided. If setting up the domain for use with RStudio, this value
must be set to <code>Service</code>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      DomainArn = "string"
    )

### Request syntax

    svc$update_domain(
      DomainId = "string",
      DefaultUserSettings = list(
        ExecutionRole = "string",
        SecurityGroups = list(
          "string"
        ),
        SharingSettings = list(
          NotebookOutputOption = "Allowed"|"Disabled",
          S3OutputPath = "string",
          S3KmsKeyId = "string"
        ),
        JupyterServerAppSettings = list(
          DefaultResourceSpec = list(
            SageMakerImageArn = "string",
            SageMakerImageVersionArn = "string",
            InstanceType = "system"|"ml.t3.micro"|"ml.t3.small"|"ml.t3.medium"|"ml.t3.large"|"ml.t3.xlarge"|"ml.t3.2xlarge"|"ml.m5.large"|"ml.m5.xlarge"|"ml.m5.2xlarge"|"ml.m5.4xlarge"|"ml.m5.8xlarge"|"ml.m5.12xlarge"|"ml.m5.16xlarge"|"ml.m5.24xlarge"|"ml.m5d.large"|"ml.m5d.xlarge"|"ml.m5d.2xlarge"|"ml.m5d.4xlarge"|"ml.m5d.8xlarge"|"ml.m5d.12xlarge"|"ml.m5d.16xlarge"|"ml.m5d.24xlarge"|"ml.c5.large"|"ml.c5.xlarge"|"ml.c5.2xlarge"|"ml.c5.4xlarge"|"ml.c5.9xlarge"|"ml.c5.12xlarge"|"ml.c5.18xlarge"|"ml.c5.24xlarge"|"ml.p3.2xlarge"|"ml.p3.8xlarge"|"ml.p3.16xlarge"|"ml.p3dn.24xlarge"|"ml.g4dn.xlarge"|"ml.g4dn.2xlarge"|"ml.g4dn.4xlarge"|"ml.g4dn.8xlarge"|"ml.g4dn.12xlarge"|"ml.g4dn.16xlarge"|"ml.r5.large"|"ml.r5.xlarge"|"ml.r5.2xlarge"|"ml.r5.4xlarge"|"ml.r5.8xlarge"|"ml.r5.12xlarge"|"ml.r5.16xlarge"|"ml.r5.24xlarge"|"ml.g5.xlarge"|"ml.g5.2xlarge"|"ml.g5.4xlarge"|"ml.g5.8xlarge"|"ml.g5.16xlarge"|"ml.g5.12xlarge"|"ml.g5.24xlarge"|"ml.g5.48xlarge"|"ml.geospatial.interactive"|"ml.p4d.24xlarge"|"ml.p4de.24xlarge",
            LifecycleConfigArn = "string"
          ),
          LifecycleConfigArns = list(
            "string"
          ),
          CodeRepositories = list(
            list(
              RepositoryUrl = "string"
            )
          )
        ),
        KernelGatewayAppSettings = list(
          DefaultResourceSpec = list(
            SageMakerImageArn = "string",
            SageMakerImageVersionArn = "string",
            InstanceType = "system"|"ml.t3.micro"|"ml.t3.small"|"ml.t3.medium"|"ml.t3.large"|"ml.t3.xlarge"|"ml.t3.2xlarge"|"ml.m5.large"|"ml.m5.xlarge"|"ml.m5.2xlarge"|"ml.m5.4xlarge"|"ml.m5.8xlarge"|"ml.m5.12xlarge"|"ml.m5.16xlarge"|"ml.m5.24xlarge"|"ml.m5d.large"|"ml.m5d.xlarge"|"ml.m5d.2xlarge"|"ml.m5d.4xlarge"|"ml.m5d.8xlarge"|"ml.m5d.12xlarge"|"ml.m5d.16xlarge"|"ml.m5d.24xlarge"|"ml.c5.large"|"ml.c5.xlarge"|"ml.c5.2xlarge"|"ml.c5.4xlarge"|"ml.c5.9xlarge"|"ml.c5.12xlarge"|"ml.c5.18xlarge"|"ml.c5.24xlarge"|"ml.p3.2xlarge"|"ml.p3.8xlarge"|"ml.p3.16xlarge"|"ml.p3dn.24xlarge"|"ml.g4dn.xlarge"|"ml.g4dn.2xlarge"|"ml.g4dn.4xlarge"|"ml.g4dn.8xlarge"|"ml.g4dn.12xlarge"|"ml.g4dn.16xlarge"|"ml.r5.large"|"ml.r5.xlarge"|"ml.r5.2xlarge"|"ml.r5.4xlarge"|"ml.r5.8xlarge"|"ml.r5.12xlarge"|"ml.r5.16xlarge"|"ml.r5.24xlarge"|"ml.g5.xlarge"|"ml.g5.2xlarge"|"ml.g5.4xlarge"|"ml.g5.8xlarge"|"ml.g5.16xlarge"|"ml.g5.12xlarge"|"ml.g5.24xlarge"|"ml.g5.48xlarge"|"ml.geospatial.interactive"|"ml.p4d.24xlarge"|"ml.p4de.24xlarge",
            LifecycleConfigArn = "string"
          ),
          CustomImages = list(
            list(
              ImageName = "string",
              ImageVersionNumber = 123,
              AppImageConfigName = "string"
            )
          ),
          LifecycleConfigArns = list(
            "string"
          )
        ),
        TensorBoardAppSettings = list(
          DefaultResourceSpec = list(
            SageMakerImageArn = "string",
            SageMakerImageVersionArn = "string",
            InstanceType = "system"|"ml.t3.micro"|"ml.t3.small"|"ml.t3.medium"|"ml.t3.large"|"ml.t3.xlarge"|"ml.t3.2xlarge"|"ml.m5.large"|"ml.m5.xlarge"|"ml.m5.2xlarge"|"ml.m5.4xlarge"|"ml.m5.8xlarge"|"ml.m5.12xlarge"|"ml.m5.16xlarge"|"ml.m5.24xlarge"|"ml.m5d.large"|"ml.m5d.xlarge"|"ml.m5d.2xlarge"|"ml.m5d.4xlarge"|"ml.m5d.8xlarge"|"ml.m5d.12xlarge"|"ml.m5d.16xlarge"|"ml.m5d.24xlarge"|"ml.c5.large"|"ml.c5.xlarge"|"ml.c5.2xlarge"|"ml.c5.4xlarge"|"ml.c5.9xlarge"|"ml.c5.12xlarge"|"ml.c5.18xlarge"|"ml.c5.24xlarge"|"ml.p3.2xlarge"|"ml.p3.8xlarge"|"ml.p3.16xlarge"|"ml.p3dn.24xlarge"|"ml.g4dn.xlarge"|"ml.g4dn.2xlarge"|"ml.g4dn.4xlarge"|"ml.g4dn.8xlarge"|"ml.g4dn.12xlarge"|"ml.g4dn.16xlarge"|"ml.r5.large"|"ml.r5.xlarge"|"ml.r5.2xlarge"|"ml.r5.4xlarge"|"ml.r5.8xlarge"|"ml.r5.12xlarge"|"ml.r5.16xlarge"|"ml.r5.24xlarge"|"ml.g5.xlarge"|"ml.g5.2xlarge"|"ml.g5.4xlarge"|"ml.g5.8xlarge"|"ml.g5.16xlarge"|"ml.g5.12xlarge"|"ml.g5.24xlarge"|"ml.g5.48xlarge"|"ml.geospatial.interactive"|"ml.p4d.24xlarge"|"ml.p4de.24xlarge",
            LifecycleConfigArn = "string"
          )
        ),
        RStudioServerProAppSettings = list(
          AccessStatus = "ENABLED"|"DISABLED",
          UserGroup = "R_STUDIO_ADMIN"|"R_STUDIO_USER"
        ),
        RSessionAppSettings = list(
          DefaultResourceSpec = list(
            SageMakerImageArn = "string",
            SageMakerImageVersionArn = "string",
            InstanceType = "system"|"ml.t3.micro"|"ml.t3.small"|"ml.t3.medium"|"ml.t3.large"|"ml.t3.xlarge"|"ml.t3.2xlarge"|"ml.m5.large"|"ml.m5.xlarge"|"ml.m5.2xlarge"|"ml.m5.4xlarge"|"ml.m5.8xlarge"|"ml.m5.12xlarge"|"ml.m5.16xlarge"|"ml.m5.24xlarge"|"ml.m5d.large"|"ml.m5d.xlarge"|"ml.m5d.2xlarge"|"ml.m5d.4xlarge"|"ml.m5d.8xlarge"|"ml.m5d.12xlarge"|"ml.m5d.16xlarge"|"ml.m5d.24xlarge"|"ml.c5.large"|"ml.c5.xlarge"|"ml.c5.2xlarge"|"ml.c5.4xlarge"|"ml.c5.9xlarge"|"ml.c5.12xlarge"|"ml.c5.18xlarge"|"ml.c5.24xlarge"|"ml.p3.2xlarge"|"ml.p3.8xlarge"|"ml.p3.16xlarge"|"ml.p3dn.24xlarge"|"ml.g4dn.xlarge"|"ml.g4dn.2xlarge"|"ml.g4dn.4xlarge"|"ml.g4dn.8xlarge"|"ml.g4dn.12xlarge"|"ml.g4dn.16xlarge"|"ml.r5.large"|"ml.r5.xlarge"|"ml.r5.2xlarge"|"ml.r5.4xlarge"|"ml.r5.8xlarge"|"ml.r5.12xlarge"|"ml.r5.16xlarge"|"ml.r5.24xlarge"|"ml.g5.xlarge"|"ml.g5.2xlarge"|"ml.g5.4xlarge"|"ml.g5.8xlarge"|"ml.g5.16xlarge"|"ml.g5.12xlarge"|"ml.g5.24xlarge"|"ml.g5.48xlarge"|"ml.geospatial.interactive"|"ml.p4d.24xlarge"|"ml.p4de.24xlarge",
            LifecycleConfigArn = "string"
          ),
          CustomImages = list(
            list(
              ImageName = "string",
              ImageVersionNumber = 123,
              AppImageConfigName = "string"
            )
          )
        ),
        CanvasAppSettings = list(
          TimeSeriesForecastingSettings = list(
            Status = "ENABLED"|"DISABLED",
            AmazonForecastRoleArn = "string"
          ),
          ModelRegisterSettings = list(
            Status = "ENABLED"|"DISABLED",
            CrossAccountModelRegisterRoleArn = "string"
          )
        )
      ),
      DomainSettingsForUpdate = list(
        RStudioServerProDomainSettingsForUpdate = list(
          DomainExecutionRoleArn = "string",
          DefaultResourceSpec = list(
            SageMakerImageArn = "string",
            SageMakerImageVersionArn = "string",
            InstanceType = "system"|"ml.t3.micro"|"ml.t3.small"|"ml.t3.medium"|"ml.t3.large"|"ml.t3.xlarge"|"ml.t3.2xlarge"|"ml.m5.large"|"ml.m5.xlarge"|"ml.m5.2xlarge"|"ml.m5.4xlarge"|"ml.m5.8xlarge"|"ml.m5.12xlarge"|"ml.m5.16xlarge"|"ml.m5.24xlarge"|"ml.m5d.large"|"ml.m5d.xlarge"|"ml.m5d.2xlarge"|"ml.m5d.4xlarge"|"ml.m5d.8xlarge"|"ml.m5d.12xlarge"|"ml.m5d.16xlarge"|"ml.m5d.24xlarge"|"ml.c5.large"|"ml.c5.xlarge"|"ml.c5.2xlarge"|"ml.c5.4xlarge"|"ml.c5.9xlarge"|"ml.c5.12xlarge"|"ml.c5.18xlarge"|"ml.c5.24xlarge"|"ml.p3.2xlarge"|"ml.p3.8xlarge"|"ml.p3.16xlarge"|"ml.p3dn.24xlarge"|"ml.g4dn.xlarge"|"ml.g4dn.2xlarge"|"ml.g4dn.4xlarge"|"ml.g4dn.8xlarge"|"ml.g4dn.12xlarge"|"ml.g4dn.16xlarge"|"ml.r5.large"|"ml.r5.xlarge"|"ml.r5.2xlarge"|"ml.r5.4xlarge"|"ml.r5.8xlarge"|"ml.r5.12xlarge"|"ml.r5.16xlarge"|"ml.r5.24xlarge"|"ml.g5.xlarge"|"ml.g5.2xlarge"|"ml.g5.4xlarge"|"ml.g5.8xlarge"|"ml.g5.16xlarge"|"ml.g5.12xlarge"|"ml.g5.24xlarge"|"ml.g5.48xlarge"|"ml.geospatial.interactive"|"ml.p4d.24xlarge"|"ml.p4de.24xlarge",
            LifecycleConfigArn = "string"
          ),
          RStudioConnectUrl = "string",
          RStudioPackageManagerUrl = "string"
        ),
        ExecutionRoleIdentityConfig = "USER_PROFILE_NAME"|"DISABLED",
        SecurityGroupIds = list(
          "string"
        )
      ),
      DefaultSpaceSettings = list(
        ExecutionRole = "string",
        SecurityGroups = list(
          "string"
        ),
        JupyterServerAppSettings = list(
          DefaultResourceSpec = list(
            SageMakerImageArn = "string",
            SageMakerImageVersionArn = "string",
            InstanceType = "system"|"ml.t3.micro"|"ml.t3.small"|"ml.t3.medium"|"ml.t3.large"|"ml.t3.xlarge"|"ml.t3.2xlarge"|"ml.m5.large"|"ml.m5.xlarge"|"ml.m5.2xlarge"|"ml.m5.4xlarge"|"ml.m5.8xlarge"|"ml.m5.12xlarge"|"ml.m5.16xlarge"|"ml.m5.24xlarge"|"ml.m5d.large"|"ml.m5d.xlarge"|"ml.m5d.2xlarge"|"ml.m5d.4xlarge"|"ml.m5d.8xlarge"|"ml.m5d.12xlarge"|"ml.m5d.16xlarge"|"ml.m5d.24xlarge"|"ml.c5.large"|"ml.c5.xlarge"|"ml.c5.2xlarge"|"ml.c5.4xlarge"|"ml.c5.9xlarge"|"ml.c5.12xlarge"|"ml.c5.18xlarge"|"ml.c5.24xlarge"|"ml.p3.2xlarge"|"ml.p3.8xlarge"|"ml.p3.16xlarge"|"ml.p3dn.24xlarge"|"ml.g4dn.xlarge"|"ml.g4dn.2xlarge"|"ml.g4dn.4xlarge"|"ml.g4dn.8xlarge"|"ml.g4dn.12xlarge"|"ml.g4dn.16xlarge"|"ml.r5.large"|"ml.r5.xlarge"|"ml.r5.2xlarge"|"ml.r5.4xlarge"|"ml.r5.8xlarge"|"ml.r5.12xlarge"|"ml.r5.16xlarge"|"ml.r5.24xlarge"|"ml.g5.xlarge"|"ml.g5.2xlarge"|"ml.g5.4xlarge"|"ml.g5.8xlarge"|"ml.g5.16xlarge"|"ml.g5.12xlarge"|"ml.g5.24xlarge"|"ml.g5.48xlarge"|"ml.geospatial.interactive"|"ml.p4d.24xlarge"|"ml.p4de.24xlarge",
            LifecycleConfigArn = "string"
          ),
          LifecycleConfigArns = list(
            "string"
          ),
          CodeRepositories = list(
            list(
              RepositoryUrl = "string"
            )
          )
        ),
        KernelGatewayAppSettings = list(
          DefaultResourceSpec = list(
            SageMakerImageArn = "string",
            SageMakerImageVersionArn = "string",
            InstanceType = "system"|"ml.t3.micro"|"ml.t3.small"|"ml.t3.medium"|"ml.t3.large"|"ml.t3.xlarge"|"ml.t3.2xlarge"|"ml.m5.large"|"ml.m5.xlarge"|"ml.m5.2xlarge"|"ml.m5.4xlarge"|"ml.m5.8xlarge"|"ml.m5.12xlarge"|"ml.m5.16xlarge"|"ml.m5.24xlarge"|"ml.m5d.large"|"ml.m5d.xlarge"|"ml.m5d.2xlarge"|"ml.m5d.4xlarge"|"ml.m5d.8xlarge"|"ml.m5d.12xlarge"|"ml.m5d.16xlarge"|"ml.m5d.24xlarge"|"ml.c5.large"|"ml.c5.xlarge"|"ml.c5.2xlarge"|"ml.c5.4xlarge"|"ml.c5.9xlarge"|"ml.c5.12xlarge"|"ml.c5.18xlarge"|"ml.c5.24xlarge"|"ml.p3.2xlarge"|"ml.p3.8xlarge"|"ml.p3.16xlarge"|"ml.p3dn.24xlarge"|"ml.g4dn.xlarge"|"ml.g4dn.2xlarge"|"ml.g4dn.4xlarge"|"ml.g4dn.8xlarge"|"ml.g4dn.12xlarge"|"ml.g4dn.16xlarge"|"ml.r5.large"|"ml.r5.xlarge"|"ml.r5.2xlarge"|"ml.r5.4xlarge"|"ml.r5.8xlarge"|"ml.r5.12xlarge"|"ml.r5.16xlarge"|"ml.r5.24xlarge"|"ml.g5.xlarge"|"ml.g5.2xlarge"|"ml.g5.4xlarge"|"ml.g5.8xlarge"|"ml.g5.16xlarge"|"ml.g5.12xlarge"|"ml.g5.24xlarge"|"ml.g5.48xlarge"|"ml.geospatial.interactive"|"ml.p4d.24xlarge"|"ml.p4de.24xlarge",
            LifecycleConfigArn = "string"
          ),
          CustomImages = list(
            list(
              ImageName = "string",
              ImageVersionNumber = 123,
              AppImageConfigName = "string"
            )
          ),
          LifecycleConfigArns = list(
            "string"
          )
        )
      ),
      AppSecurityGroupManagement = "Service"|"Customer"
    )