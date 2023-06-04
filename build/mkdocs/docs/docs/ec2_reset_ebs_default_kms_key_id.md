<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ec2_reset_ebs_default_kms_key_id</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Resets the default KMS key for EBS encryption for your account in this Region to the Amazon Web Services managed KMS key for EBS

### Description

Resets the default KMS key for EBS encryption for your account in this
Region to the Amazon Web Services managed KMS key for EBS.

After resetting the default KMS key to the Amazon Web Services managed
KMS key, you can continue to encrypt by a customer managed KMS key by
specifying it when you create the volume. For more information, see
[Amazon EBS
encryption](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html)
in the *Amazon Elastic Compute Cloud User Guide*.

### Usage

    ec2_reset_ebs_default_kms_key_id(DryRun)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ec2_reset_ebs_default_kms_key_id_:_DryRun">DryRun</code></td>
<td><p>Checks whether you have the required permissions for the action,
without actually making the request, and provides an error response. If
you have the required permissions, the error response is
<code>DryRunOperation</code>. Otherwise, it is
<code>UnauthorizedOperation</code>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      KmsKeyId = "string"
    )

### Request syntax

    svc$reset_ebs_default_kms_key_id(
      DryRun = TRUE|FALSE
    )