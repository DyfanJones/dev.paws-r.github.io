<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>sns_create_topic</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates a topic to which notifications can be published

### Description

Creates a topic to which notifications can be published. Users can
create at most 100,000 standard topics (at most 1,000 FIFO topics). For
more information, see [Creating an Amazon SNS
topic](https://docs.aws.amazon.com/sns/latest/dg/sns-create-topic.html)
in the *Amazon SNS Developer Guide*. This action is idempotent, so if
the requester already owns a topic with the specified name, that topic's
ARN is returned without creating a new topic.

### Usage

    sns_create_topic(Name, Attributes, Tags, DataProtectionPolicy)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="sns_create_topic_:_Name">Name</code></td>
<td><p>[required] The name of the topic you want to create.</p>
<p>Constraints: Topic names must be made up of only uppercase and
lowercase ASCII letters, numbers, underscores, and hyphens, and must be
between 1 and 256 characters long.</p>
<p>For a FIFO (first-in-first-out) topic, the name must end with the
<code>.fifo</code> suffix.</p></td>
</tr>
<tr class="even">
<td><code id="sns_create_topic_:_Attributes">Attributes</code></td>
<td><p>A map of attributes with their corresponding values.</p>
<p>The following lists the names, descriptions, and values of the
special request parameters that the <code>create_topic</code> action
uses:</p>
<ul>
<li><p><code>DeliveryPolicy</code> – The policy that defines how Amazon
SNS retries failed deliveries to HTTP/S endpoints.</p></li>
<li><p><code>DisplayName</code> – The display name to use for a topic
with SMS subscriptions.</p></li>
<li><p><code>FifoTopic</code> – Set to true to create a FIFO
topic.</p></li>
<li><p><code>Policy</code> – The policy that defines who can access your
topic. By default, only the topic owner can publish or subscribe to the
topic.</p></li>
<li><p><code>SignatureVersion</code> – The signature version corresponds
to the hashing algorithm used while creating the signature of the
notifications, subscription confirmations, or unsubscribe confirmation
messages sent by Amazon SNS. By default, <code>SignatureVersion</code>
is set to <code>1</code>.</p></li>
<li><p><code>TracingConfig</code> – Tracing mode of an Amazon SNS topic.
By default <code>TracingConfig</code> is set to
<code>PassThrough</code>, and the topic passes through the tracing
header it receives from an Amazon SNS publisher to its subscriptions. If
set to <code>Active</code>, Amazon SNS will vend X-Ray segment data to
topic owner account if the sampled flag in the tracing header is true.
This is only supported on standard topics.</p></li>
</ul>
<p>The following attribute applies only to <a
href="https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html">server-side
encryption</a>:</p>
<ul>
<li><p><code>KmsMasterKeyId</code> – The ID of an Amazon Web Services
managed customer master key (CMK) for Amazon SNS or a custom CMK. For
more information, see <a
href="https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms">Key
Terms</a>. For more examples, see <a
href="https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters">KeyId</a>
in the <em>Key Management Service API Reference</em>.</p></li>
</ul>
<p>The following attributes apply only to <a
href="https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html">FIFO
topics</a>:</p>
<ul>
<li><p><code>FifoTopic</code> – When this is set to <code>true</code>, a
FIFO topic is created.</p></li>
<li><p><code>ContentBasedDeduplication</code> – Enables content-based
deduplication for FIFO topics.</p>
<ul>
<li><p>By default, <code>ContentBasedDeduplication</code> is set to
<code>false</code>. If you create a FIFO topic and this attribute is
<code>false</code>, you must specify a value for the
<code>MessageDeduplicationId</code> parameter for the
<code>publish</code> action.</p></li>
<li><p>When you set <code>ContentBasedDeduplication</code> to
<code>true</code>, Amazon SNS uses a SHA-256 hash to generate the
<code>MessageDeduplicationId</code> using the body of the message (but
not the attributes of the message).</p>
<p>(Optional) To override the generated value, you can specify a value
for the <code>MessageDeduplicationId</code> parameter for the
<code>publish</code> action.</p></li>
</ul></li>
</ul></td>
</tr>
<tr class="odd">
<td><code id="sns_create_topic_:_Tags">Tags</code></td>
<td><p>The list of tags to add to a new topic.</p>
<p>To be able to tag a topic on creation, you must have the
<code>sns:CreateTopic</code> and <code>sns:TagResource</code>
permissions.</p></td>
</tr>
<tr class="even">
<td><code
id="sns_create_topic_:_DataProtectionPolicy">DataProtectionPolicy</code></td>
<td><p>The body of the policy document you want to use for this
topic.</p>
<p>You can only add one policy per topic.</p>
<p>The policy must be in JSON string format.</p>
<p>Length Constraints: Maximum length of 30,720.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      TopicArn = "string"
    )

### Request syntax

    svc$create_topic(
      Name = "string",
      Attributes = list(
        "string"
      ),
      Tags = list(
        list(
          Key = "string",
          Value = "string"
        )
      ),
      DataProtectionPolicy = "string"
    )