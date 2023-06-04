<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>imagebuilder_create_distribution_configuration</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates a new distribution configuration

### Description

Creates a new distribution configuration. Distribution configurations
define and configure the outputs of your pipeline.

### Usage

    imagebuilder_create_distribution_configuration(name, description,
      distributions, tags, clientToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="imagebuilder_create_distribution_configuration_:_name">name</code></td>
<td><p>[required] The name of the distribution configuration.</p></td>
</tr>
<tr class="even">
<td><code
id="imagebuilder_create_distribution_configuration_:_description">description</code></td>
<td><p>The description of the distribution configuration.</p></td>
</tr>
<tr class="odd">
<td><code
id="imagebuilder_create_distribution_configuration_:_distributions">distributions</code></td>
<td><p>[required] The distributions of the distribution
configuration.</p></td>
</tr>
<tr class="even">
<td><code
id="imagebuilder_create_distribution_configuration_:_tags">tags</code></td>
<td><p>The tags of the distribution configuration.</p></td>
</tr>
<tr class="odd">
<td><code
id="imagebuilder_create_distribution_configuration_:_clientToken">clientToken</code></td>
<td><p>[required] The idempotency token of the distribution
configuration.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      requestId = "string",
      clientToken = "string",
      distributionConfigurationArn = "string"
    )

### Request syntax

    svc$create_distribution_configuration(
      name = "string",
      description = "string",
      distributions = list(
        list(
          region = "string",
          amiDistributionConfiguration = list(
            name = "string",
            description = "string",
            targetAccountIds = list(
              "string"
            ),
            amiTags = list(
              "string"
            ),
            kmsKeyId = "string",
            launchPermission = list(
              userIds = list(
                "string"
              ),
              userGroups = list(
                "string"
              ),
              organizationArns = list(
                "string"
              ),
              organizationalUnitArns = list(
                "string"
              )
            )
          ),
          containerDistributionConfiguration = list(
            description = "string",
            containerTags = list(
              "string"
            ),
            targetRepository = list(
              service = "ECR",
              repositoryName = "string"
            )
          ),
          licenseConfigurationArns = list(
            "string"
          ),
          launchTemplateConfigurations = list(
            list(
              launchTemplateId = "string",
              accountId = "string",
              setDefaultVersion = TRUE|FALSE
            )
          ),
          s3ExportConfiguration = list(
            roleName = "string",
            diskImageFormat = "VMDK"|"RAW"|"VHD",
            s3Bucket = "string",
            s3Prefix = "string"
          ),
          fastLaunchConfigurations = list(
            list(
              enabled = TRUE|FALSE,
              snapshotConfiguration = list(
                targetResourceCount = 123
              ),
              maxParallelLaunches = 123,
              launchTemplate = list(
                launchTemplateId = "string",
                launchTemplateName = "string",
                launchTemplateVersion = "string"
              ),
              accountId = "string"
            )
          )
        )
      ),
      tags = list(
        "string"
      ),
      clientToken = "string"
    )