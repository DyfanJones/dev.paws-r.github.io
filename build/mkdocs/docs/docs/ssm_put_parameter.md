<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ssm_put_parameter</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Add a parameter to the system

### Description

Add a parameter to the system.

### Usage

    ssm_put_parameter(Name, Description, Value, Type, KeyId, Overwrite,
      AllowedPattern, Tags, Tier, Policies, DataType)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="ssm_put_parameter_:_Name">Name</code></td>
<td><p>[required] The fully qualified name of the parameter that you
want to add to the system. The fully qualified name includes the
complete hierarchy of the parameter path and name. For parameters in a
hierarchy, you must include a leading forward slash character (/) when
you create or reference a parameter. For example: <code
style="white-space: pre;">⁠/Dev/DBServer/MySQL/db-string13⁠</code></p>
<p>Naming Constraints:</p>
<ul>
<li><p>Parameter names are case sensitive.</p></li>
<li><p>A parameter name must be unique within an Amazon Web Services
Region</p></li>
<li><p>A parameter name can't be prefixed with "<code>aws</code>" or
"<code>ssm</code>" (case-insensitive).</p></li>
<li><p>Parameter names can include only the following symbols and
letters: <code style="white-space: pre;">⁠a-zA-Z0-9_.-⁠</code></p>
<p>In addition, the slash character ( / ) is used to delineate
hierarchies in parameter names. For example: <code
style="white-space: pre;">⁠/Dev/Production/East/Project-ABC/MyParameter⁠</code></p></li>
<li><p>A parameter name can't include spaces.</p></li>
<li><p>Parameter hierarchies are limited to a maximum depth of fifteen
levels.</p></li>
</ul>
<p>For additional information about valid values for parameter names,
see <a
href="https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-su-create.html">Creating
Systems Manager parameters</a> in the <em>Amazon Web Services Systems
Manager User Guide</em>.</p>
<p>The maximum length constraint of 2048 characters listed below
includes 1037 characters reserved for internal use by Systems Manager.
The maximum length for a parameter name that you create is 1011
characters. This includes the characters in the ARN that precede the
name you specify, such as <code
style="white-space: pre;">⁠arn:aws:ssm:us-east-2:111122223333:parameter/⁠</code>.</p></td>
</tr>
<tr class="even">
<td><code id="ssm_put_parameter_:_Description">Description</code></td>
<td><p>Information about the parameter that you want to add to the
system. Optional but recommended.</p>
<p>Don't enter personally identifiable information in this
field.</p></td>
</tr>
<tr class="odd">
<td><code id="ssm_put_parameter_:_Value">Value</code></td>
<td><p>[required] The parameter value that you want to add to the
system. Standard parameters have a value limit of 4 KB. Advanced
parameters have a value limit of 8 KB.</p>
<p>Parameters can't be referenced or nested in the values of other
parameters. You can't include <code>{{}}</code> or
<code>{{ssm:parameter-name}}</code> in a parameter value.</p></td>
</tr>
<tr class="even">
<td><code id="ssm_put_parameter_:_Type">Type</code></td>
<td><p>The type of parameter that you want to add to the system.</p>
<p><code>SecureString</code> isn't currently supported for
CloudFormation templates.</p>
<p>Items in a <code>StringList</code> must be separated by a comma (,).
You can't use other punctuation or special character to escape items in
the list. If you have a parameter value that requires a comma, then use
the <code>String</code> data type.</p>
<p>Specifying a parameter type isn't required when updating a parameter.
You must specify a parameter type when creating a parameter.</p></td>
</tr>
<tr class="odd">
<td><code id="ssm_put_parameter_:_KeyId">KeyId</code></td>
<td><p>The Key Management Service (KMS) ID that you want to use to
encrypt a parameter. Use a custom key for better security. Required for
parameters that use the <code>SecureString</code> data type.</p>
<p>If you don't specify a key ID, the system uses the default key
associated with your Amazon Web Services account which is not as secure
as using a custom key.</p>
<ul>
<li><p>To use a custom KMS key, choose the <code>SecureString</code>
data type with the <code style="white-space: pre;">⁠Key ID⁠</code>
parameter.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code id="ssm_put_parameter_:_Overwrite">Overwrite</code></td>
<td><p>Overwrite an existing parameter. The default value is
<code>false</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="ssm_put_parameter_:_AllowedPattern">AllowedPattern</code></td>
<td><p>A regular expression used to validate the parameter value. For
example, for String types with values restricted to numbers, you can
specify the following: AllowedPattern=^\d+$</p></td>
</tr>
<tr class="even">
<td><code id="ssm_put_parameter_:_Tags">Tags</code></td>
<td><p>Optional metadata that you assign to a resource. Tags enable you
to categorize a resource in different ways, such as by purpose, owner,
or environment. For example, you might want to tag a Systems Manager
parameter to identify the type of resource to which it applies, the
environment, or the type of configuration data referenced by the
parameter. In this case, you could specify the following key-value
pairs:</p>
<ul>
<li><p><code
style="white-space: pre;">⁠Key=Resource,Value=S3bucket⁠</code></p></li>
<li><p><code
style="white-space: pre;">⁠Key=OS,Value=Windows⁠</code></p></li>
<li><p><code
style="white-space: pre;">⁠Key=ParameterType,Value=LicenseKey⁠</code></p></li>
</ul>
<p>To add tags to an existing Systems Manager parameter, use the
<code>add_tags_to_resource</code> operation.</p></td>
</tr>
<tr class="odd">
<td><code id="ssm_put_parameter_:_Tier">Tier</code></td>
<td><p>The parameter tier to assign to a parameter.</p>
<p>Parameter Store offers a standard tier and an advanced tier for
parameters. Standard parameters have a content size limit of 4 KB and
can't be configured to use parameter policies. You can create a maximum
of 10,000 standard parameters for each Region in an Amazon Web Services
account. Standard parameters are offered at no additional cost.</p>
<p>Advanced parameters have a content size limit of 8 KB and can be
configured to use parameter policies. You can create a maximum of
100,000 advanced parameters for each Region in an Amazon Web Services
account. Advanced parameters incur a charge. For more information, see
<a
href="https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-advanced-parameters.html">Standard
and advanced parameter tiers</a> in the <em>Amazon Web Services Systems
Manager User Guide</em>.</p>
<p>You can change a standard parameter to an advanced parameter any
time. But you can't revert an advanced parameter to a standard
parameter. Reverting an advanced parameter to a standard parameter would
result in data loss because the system would truncate the size of the
parameter from 8 KB to 4 KB. Reverting would also remove any policies
attached to the parameter. Lastly, advanced parameters use a different
form of encryption than standard parameters.</p>
<p>If you no longer need an advanced parameter, or if you no longer want
to incur charges for an advanced parameter, you must delete it and
recreate it as a new standard parameter.</p>
<p><strong>Using the Default Tier Configuration</strong></p>
<p>In <code>put_parameter</code> requests, you can specify the tier to
create the parameter in. Whenever you specify a tier in the request,
Parameter Store creates or updates the parameter according to that
request. However, if you don't specify a tier in a request, Parameter
Store assigns the tier based on the current Parameter Store default tier
configuration.</p>
<p>The default tier when you begin using Parameter Store is the
standard-parameter tier. If you use the advanced-parameter tier, you can
specify one of the following as the default:</p>
<ul>
<li><p><strong>Advanced</strong>: With this option, Parameter Store
evaluates all requests as advanced parameters.</p></li>
<li><p><strong>Intelligent-Tiering</strong>: With this option, Parameter
Store evaluates each request to determine if the parameter is standard
or advanced.</p>
<p>If the request doesn't include any options that require an advanced
parameter, the parameter is created in the standard-parameter tier. If
one or more options requiring an advanced parameter are included in the
request, Parameter Store create a parameter in the advanced-parameter
tier.</p>
<p>This approach helps control your parameter-related costs by always
creating standard parameters unless an advanced parameter is
necessary.</p></li>
</ul>
<p>Options that require an advanced parameter include the following:</p>
<ul>
<li><p>The content size of the parameter is more than 4 KB.</p></li>
<li><p>The parameter uses a parameter policy.</p></li>
<li><p>More than 10,000 parameters already exist in your Amazon Web
Services account in the current Amazon Web Services Region.</p></li>
</ul>
<p>For more information about configuring the default tier option, see
<a
href="https://docs.aws.amazon.com/systems-manager/latest/userguide/">Specifying
a default parameter tier</a> in the <em>Amazon Web Services Systems
Manager User Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code id="ssm_put_parameter_:_Policies">Policies</code></td>
<td><p>One or more policies to apply to a parameter. This operation
takes a JSON array. Parameter Store, a capability of Amazon Web Services
Systems Manager supports the following policy types:</p>
<p>Expiration: This policy deletes the parameter after it expires. When
you create the policy, you specify the expiration date. You can update
the expiration date and time by updating the policy. Updating the
<em>parameter</em> doesn't affect the expiration date and time. When the
expiration time is reached, Parameter Store deletes the parameter.</p>
<p>ExpirationNotification: This policy initiates an event in Amazon
CloudWatch Events that notifies you about the expiration. By using this
policy, you can receive notification before or after the expiration time
is reached, in units of days or hours.</p>
<p>NoChangeNotification: This policy initiates a CloudWatch Events event
if a parameter hasn't been modified for a specified period of time. This
policy type is useful when, for example, a secret needs to be changed
within a period of time, but it hasn't been changed.</p>
<p>All existing policies are preserved until you send new policies or an
empty policy. For more information about parameter policies, see <a
href="https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html">Assigning
parameter policies</a>.</p></td>
</tr>
<tr class="odd">
<td><code id="ssm_put_parameter_:_DataType">DataType</code></td>
<td><p>The data type for a <code>String</code> parameter. Supported data
types include plain text and Amazon Machine Image (AMI) IDs.</p>
<p><strong>The following data type values are supported.</strong></p>
<ul>
<li><p><code>text</code></p></li>
<li><p><code>aws:ec2:image</code></p></li>
<li><p><code>aws:ssm:integration</code></p></li>
</ul>
<p>When you create a <code>String</code> parameter and specify
<code>aws:ec2:image</code>, Amazon Web Services Systems Manager
validates the parameter value is in the required format, such as <code
style="white-space: pre;">⁠ami-12345abcdeEXAMPLE⁠</code>, and that the
specified AMI is available in your Amazon Web Services account.</p>
<p>If the action is successful, the service sends back an HTTP 200
response which indicates a successful <code>put_parameter</code> call
for all cases except for data type <code>aws:ec2:image</code>. If you
call <code>put_parameter</code> with <code>aws:ec2:image</code> data
type, a successful HTTP 200 response does not guarantee that your
parameter was successfully created or updated. The
<code>aws:ec2:image</code> value is validated asynchronously, and the
<code>put_parameter</code> call returns before the validation is
complete. If you submit an invalid AMI value, the PutParameter operation
will return success, but the asynchronous validation will fail and the
parameter will not be created or updated. To monitor whether your
<code>aws:ec2:image</code> parameters are created successfully, see <a
href="https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-cwe.html">Setting
up notifications or trigger actions based on Parameter Store events</a>.
For more information about AMI format validation , see <a
href="https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-ec2-aliases.html">Native
parameter support for Amazon Machine Image (AMI) IDs</a>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Version = 123,
      Tier = "Standard"|"Advanced"|"Intelligent-Tiering"
    )

### Request syntax

    svc$put_parameter(
      Name = "string",
      Description = "string",
      Value = "string",
      Type = "String"|"StringList"|"SecureString",
      KeyId = "string",
      Overwrite = TRUE|FALSE,
      AllowedPattern = "string",
      Tags = list(
        list(
          Key = "string",
          Value = "string"
        )
      ),
      Tier = "Standard"|"Advanced"|"Intelligent-Tiering",
      Policies = "string",
      DataType = "string"
    )