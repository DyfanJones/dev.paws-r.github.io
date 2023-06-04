<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>neptune_modify_db_parameter_group</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Modifies the parameters of a DB parameter group

### Description

Modifies the parameters of a DB parameter group. To modify more than one
parameter, submit a list of the following: `ParameterName`,
`ParameterValue`, and `ApplyMethod`. A maximum of 20 parameters can be
modified in a single request.

Changes to dynamic parameters are applied immediately. Changes to static
parameters require a reboot without failover to the DB instance
associated with the parameter group before the change can take effect.

After you modify a DB parameter group, you should wait at least 5
minutes before creating your first DB instance that uses that DB
parameter group as the default parameter group. This allows Amazon
Neptune to fully complete the modify action before the parameter group
is used as the default for a new DB instance. This is especially
important for parameters that are critical when creating the default
database for a DB instance, such as the character set for the default
database defined by the `character_set_database` parameter. You can use
the *Parameter Groups* option of the Amazon Neptune console or the
*DescribeDBParameters* command to verify that your DB parameter group
has been created or modified.

### Usage

    neptune_modify_db_parameter_group(DBParameterGroupName, Parameters)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="neptune_modify_db_parameter_group_:_DBParameterGroupName">DBParameterGroupName</code></td>
<td><p>[required] The name of the DB parameter group.</p>
<p>Constraints:</p>
<ul>
<li><p>If supplied, must match the name of an existing
DBParameterGroup.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code
id="neptune_modify_db_parameter_group_:_Parameters">Parameters</code></td>
<td><p>[required] An array of parameter names, values, and the apply
method for the parameter update. At least one parameter name, value, and
apply method must be supplied; subsequent arguments are optional. A
maximum of 20 parameters can be modified in a single request.</p>
<p>Valid Values (for the application method):
<code>immediate | pending-reboot</code></p>
<p>You can use the immediate value with dynamic parameters only. You can
use the pending-reboot value for both dynamic and static parameters, and
changes are applied when you reboot the DB instance without
failover.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      DBParameterGroupName = "string"
    )

### Request syntax

    svc$modify_db_parameter_group(
      DBParameterGroupName = "string",
      Parameters = list(
        list(
          ParameterName = "string",
          ParameterValue = "string",
          Description = "string",
          Source = "string",
          ApplyType = "string",
          DataType = "string",
          AllowedValues = "string",
          IsModifiable = TRUE|FALSE,
          MinimumEngineVersion = "string",
          ApplyMethod = "immediate"|"pending-reboot"
        )
      )
    )