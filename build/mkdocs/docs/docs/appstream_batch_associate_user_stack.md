<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>appstream_batch_associate_user_stack</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Associates the specified users with the specified stacks

### Description

Associates the specified users with the specified stacks. Users in a
user pool cannot be assigned to stacks with fleets that are joined to an
Active Directory domain.

### Usage

    appstream_batch_associate_user_stack(UserStackAssociations)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="appstream_batch_associate_user_stack_:_UserStackAssociations">UserStackAssociations</code></td>
<td><p>[required] The list of UserStackAssociation objects.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      errors = list(
        list(
          UserStackAssociation = list(
            StackName = "string",
            UserName = "string",
            AuthenticationType = "API"|"SAML"|"USERPOOL"|"AWS_AD",
            SendEmailNotification = TRUE|FALSE
          ),
          ErrorCode = "STACK_NOT_FOUND"|"USER_NAME_NOT_FOUND"|"DIRECTORY_NOT_FOUND"|"INTERNAL_ERROR",
          ErrorMessage = "string"
        )
      )
    )

### Request syntax

    svc$batch_associate_user_stack(
      UserStackAssociations = list(
        list(
          StackName = "string",
          UserName = "string",
          AuthenticationType = "API"|"SAML"|"USERPOOL"|"AWS_AD",
          SendEmailNotification = TRUE|FALSE
        )
      )
    )