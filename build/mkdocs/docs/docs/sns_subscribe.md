<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>sns_subscribe</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Subscribes an endpoint to an Amazon SNS topic

### Description

Subscribes an endpoint to an Amazon SNS topic. If the endpoint type is
HTTP/S or email, or if the endpoint and the topic are not in the same
Amazon Web Services account, the endpoint owner must run the
`confirm_subscription` action to confirm the subscription.

You call the `confirm_subscription` action with the token from the
subscription response. Confirmation tokens are valid for three days.

This action is throttled at 100 transactions per second (TPS).

### Usage

    sns_subscribe(TopicArn, Protocol, Endpoint, Attributes,
      ReturnSubscriptionArn)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="sns_subscribe_:_TopicArn">TopicArn</code></td>
<td><p>[required] The ARN of the topic you want to subscribe
to.</p></td>
</tr>
<tr class="even">
<td><code id="sns_subscribe_:_Protocol">Protocol</code></td>
<td><p>[required] The protocol that you want to use. Supported protocols
include:</p>
<ul>
<li><p><code>http</code> – delivery of JSON-encoded message via HTTP
POST</p></li>
<li><p><code>https</code> – delivery of JSON-encoded message via HTTPS
POST</p></li>
<li><p><code>email</code> – delivery of message via SMTP</p></li>
<li><p><code>email-json</code> – delivery of JSON-encoded message via
SMTP</p></li>
<li><p><code>sms</code> – delivery of message via SMS</p></li>
<li><p><code>sqs</code> – delivery of JSON-encoded message to an Amazon
SQS queue</p></li>
<li><p><code>application</code> – delivery of JSON-encoded message to an
EndpointArn for a mobile app and device</p></li>
<li><p><code>lambda</code> – delivery of JSON-encoded message to an
Lambda function</p></li>
<li><p><code>firehose</code> – delivery of JSON-encoded message to an
Amazon Kinesis Data Firehose delivery stream.</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code id="sns_subscribe_:_Endpoint">Endpoint</code></td>
<td><p>The endpoint that you want to receive notifications. Endpoints
vary by protocol:</p>
<ul>
<li><p>For the <code>http</code> protocol, the (public) endpoint is a
URL beginning with <code
style="white-space: pre;">⁠http://⁠</code>.</p></li>
<li><p>For the <code>https</code> protocol, the (public) endpoint is a
URL beginning with <code
style="white-space: pre;">⁠https://⁠</code>.</p></li>
<li><p>For the <code>email</code> protocol, the endpoint is an email
address.</p></li>
<li><p>For the <code>email-json</code> protocol, the endpoint is an
email address.</p></li>
<li><p>For the <code>sms</code> protocol, the endpoint is a phone number
of an SMS-enabled device.</p></li>
<li><p>For the <code>sqs</code> protocol, the endpoint is the ARN of an
Amazon SQS queue.</p></li>
<li><p>For the <code>application</code> protocol, the endpoint is the
EndpointArn of a mobile app and device.</p></li>
<li><p>For the <code>lambda</code> protocol, the endpoint is the ARN of
an Lambda function.</p></li>
<li><p>For the <code>firehose</code> protocol, the endpoint is the ARN
of an Amazon Kinesis Data Firehose delivery stream.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code id="sns_subscribe_:_Attributes">Attributes</code></td>
<td><p>A map of attributes with their corresponding values.</p>
<p>The following lists the names, descriptions, and values of the
special request parameters that the <code>subscribe</code> action
uses:</p>
<ul>
<li><p><code>DeliveryPolicy</code> – The policy that defines how Amazon
SNS retries failed deliveries to HTTP/S endpoints.</p></li>
<li><p><code>FilterPolicy</code> – The simple JSON object that lets your
subscriber receive only a subset of messages, rather than receiving
every message published to the topic.</p></li>
<li><p><code>FilterPolicyScope</code> – This attribute lets you choose
the filtering scope by using one of the following string value
types:</p>
<ul>
<li><p><code>MessageAttributes</code> (default) – The filter is applied
on the message attributes.</p></li>
<li><p><code>MessageBody</code> – The filter is applied on the message
body.</p></li>
</ul></li>
<li><p><code>RawMessageDelivery</code> – When set to <code>true</code>,
enables raw message delivery to Amazon SQS or HTTP/S endpoints. This
eliminates the need for the endpoints to process JSON formatting, which
is otherwise created for Amazon SNS metadata.</p></li>
<li><p><code>RedrivePolicy</code> – When specified, sends undeliverable
messages to the specified Amazon SQS dead-letter queue. Messages that
can't be delivered due to client errors (for example, when the
subscribed endpoint is unreachable) or server errors (for example, when
the service that powers the subscribed endpoint becomes unavailable) are
held in the dead-letter queue for further analysis or
reprocessing.</p></li>
</ul>
<p>The following attribute applies only to Amazon Kinesis Data Firehose
delivery stream subscriptions:</p>
<ul>
<li><p><code>SubscriptionRoleArn</code> – The ARN of the IAM role that
has the following:</p>
<ul>
<li><p>Permission to write to the Kinesis Data Firehose delivery
stream</p></li>
<li><p>Amazon SNS listed as a trusted entity</p></li>
</ul>
<p>Specifying a valid ARN for this attribute is required for Kinesis
Data Firehose delivery stream subscriptions. For more information, see
<a
href="https://docs.aws.amazon.com/sns/latest/dg/sns-firehose-as-subscriber.html">Fanout
to Kinesis Data Firehose delivery streams</a> in the <em>Amazon SNS
Developer Guide</em>.</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code
id="sns_subscribe_:_ReturnSubscriptionArn">ReturnSubscriptionArn</code></td>
<td><p>Sets whether the response from the <code>subscribe</code> request
includes the subscription ARN, even if the subscription is not yet
confirmed.</p>
<p>If you set this parameter to <code>true</code>, the response includes
the ARN in all cases, even if the subscription is not yet confirmed. In
addition to the ARN for confirmed subscriptions, the response also
includes the <code style="white-space: pre;">⁠pending subscription⁠</code>
ARN value for subscriptions that aren't yet confirmed. A subscription
becomes confirmed when the subscriber calls the
<code>confirm_subscription</code> action with a confirmation token.</p>
<p>The default value is <code>false</code>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      SubscriptionArn = "string"
    )

### Request syntax

    svc$subscribe(
      TopicArn = "string",
      Protocol = "string",
      Endpoint = "string",
      Attributes = list(
        "string"
      ),
      ReturnSubscriptionArn = TRUE|FALSE
    )