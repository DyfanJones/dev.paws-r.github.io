<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>glacier_create_vault</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## This operation creates a new vault with the specified name

### Description

This operation creates a new vault with the specified name. The name of
the vault must be unique within a region for an AWS account. You can
create up to 1,000 vaults per account. If you need to create more
vaults, contact Amazon S3 Glacier.

You must use the following guidelines when naming a vault.

-   Names can be between 1 and 255 characters long.

-   Allowed characters are a-z, A-Z, 0-9, '\_' (underscore), '-'
    (hyphen), and '.' (period).

This operation is idempotent.

An AWS account has full permission to perform all operations (actions).
However, AWS Identity and Access Management (IAM) users don't have any
permissions by default. You must grant them explicit permission to
perform specific actions. For more information, see [Access Control
Using AWS Identity and Access Management
(IAM)](https://docs.aws.amazon.com/amazonglacier/latest/dev/security-iam.html).

For conceptual information and underlying REST API, see [Creating a
Vault in Amazon
Glacier](https://docs.aws.amazon.com/amazonglacier/latest/dev/creating-vaults.html)
and [Create
Vault](https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-put.html)
in the *Amazon Glacier Developer Guide*.

### Usage

    glacier_create_vault(accountId, vaultName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="glacier_create_vault_:_accountId">accountId</code></td>
<td><p>[required] The <code>AccountId</code> value is the AWS account
ID. This value must match the AWS account ID associated with the
credentials used to sign the request. You can either specify an AWS
account ID or optionally a single '<code>-</code>' (hyphen), in which
case Amazon S3 Glacier uses the AWS account ID associated with the
credentials used to sign the request. If you specify your account ID, do
not include any hyphens ('-') in the ID.</p></td>
</tr>
<tr class="even">
<td><code id="glacier_create_vault_:_vaultName">vaultName</code></td>
<td><p>[required] The name of the vault.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      location = "string"
    )

### Request syntax

    svc$create_vault(
      accountId = "string",
      vaultName = "string"
    )

### Examples

    ## Not run: 
    # The following example creates a new vault named my-vault.
    svc$create_vault(
      accountId = "-",
      vaultName = "my-vault"
    )

    ## End(Not run)
