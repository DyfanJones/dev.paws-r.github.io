<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>sagemaker_update_device_fleet</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Updates a fleet of devices

### Description

Updates a fleet of devices.

### Usage

    sagemaker_update_device_fleet(DeviceFleetName, RoleArn, Description,
      OutputConfig, EnableIotRoleAlias)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="sagemaker_update_device_fleet_:_DeviceFleetName">DeviceFleetName</code></td>
<td><p>[required] The name of the fleet.</p></td>
</tr>
<tr class="even">
<td><code
id="sagemaker_update_device_fleet_:_RoleArn">RoleArn</code></td>
<td><p>The Amazon Resource Name (ARN) of the device.</p></td>
</tr>
<tr class="odd">
<td><code
id="sagemaker_update_device_fleet_:_Description">Description</code></td>
<td><p>Description of the fleet.</p></td>
</tr>
<tr class="even">
<td><code
id="sagemaker_update_device_fleet_:_OutputConfig">OutputConfig</code></td>
<td><p>[required] Output configuration for storing sample data collected
by the fleet.</p></td>
</tr>
<tr class="odd">
<td><code
id="sagemaker_update_device_fleet_:_EnableIotRoleAlias">EnableIotRoleAlias</code></td>
<td><p>Whether to create an Amazon Web Services IoT Role Alias during
device fleet creation. The name of the role alias generated will match
this pattern: "SageMakerEdge-{DeviceFleetName}".</p>
<p>For example, if your device fleet is called "demo-fleet", the name of
the role alias will be "SageMakerEdge-demo-fleet".</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$update_device_fleet(
      DeviceFleetName = "string",
      RoleArn = "string",
      Description = "string",
      OutputConfig = list(
        S3OutputLocation = "string",
        KmsKeyId = "string",
        PresetDeploymentType = "GreengrassV2Component",
        PresetDeploymentConfig = "string"
      ),
      EnableIotRoleAlias = TRUE|FALSE
    )