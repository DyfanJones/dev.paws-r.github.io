<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>sns_set_sms_attributes</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Use this request to set the default settings for sending SMS messages and receiving daily SMS usage reports

### Description

Use this request to set the default settings for sending SMS messages
and receiving daily SMS usage reports.

You can override some of these settings for a single message when you
use the `publish` action with the `MessageAttributes.entry.N` parameter.
For more information, see [Publishing to a mobile
phone](https://docs.aws.amazon.com/sns/latest/dg/sms_publish-to-phone.html)
in the *Amazon SNS Developer Guide*.

To use this operation, you must grant the Amazon SNS service principal
(`sns.amazonaws.com`) permission to perform the `s3:ListBucket` action.

### Usage

    sns_set_sms_attributes(attributes)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="sns_set_sms_attributes_:_attributes">attributes</code></td>
<td><p>[required] The default settings for sending SMS messages from
your Amazon Web Services account. You can set values for the following
attribute names:</p>
<p><code>MonthlySpendLimit</code> – The maximum amount in USD that you
are willing to spend each month to send SMS messages. When Amazon SNS
determines that sending an SMS message would incur a cost that exceeds
this limit, it stops sending SMS messages within minutes.</p>
<p>Amazon SNS stops sending SMS messages within minutes of the limit
being crossed. During that interval, if you continue to send SMS
messages, you will incur costs that exceed your limit.</p>
<p>By default, the spend limit is set to the maximum allowed by Amazon
SNS. If you want to raise the limit, submit an SNS Limit Increase case.
For <strong>New limit value</strong>, enter your desired monthly spend
limit. In the <strong>Use Case Description</strong> field, explain that
you are requesting an SMS monthly spend limit increase.</p>
<p><code>DeliveryStatusIAMRole</code> – The ARN of the IAM role that
allows Amazon SNS to write logs about SMS deliveries in CloudWatch Logs.
For each SMS message that you send, Amazon SNS writes a log that
includes the message price, the success or failure status, the reason
for failure (if the message failed), the message dwell time, and other
information.</p>
<p><code>DeliveryStatusSuccessSamplingRate</code> – The percentage of
successful SMS deliveries for which Amazon SNS will write logs in
CloudWatch Logs. The value can be an integer from 0 - 100. For example,
to write logs only for failed deliveries, set this value to
<code>0</code>. To write logs for 10% of your successful deliveries, set
it to <code>10</code>.</p>
<p><code>DefaultSenderID</code> – A string, such as your business brand,
that is displayed as the sender on the receiving device. Support for
sender IDs varies by country. The sender ID can be 1 - 11 alphanumeric
characters, and it must contain at least one letter.</p>
<p><code>DefaultSMSType</code> – The type of SMS message that you will
send by default. You can assign the following values:</p>
<ul>
<li><p><code>Promotional</code> – (Default) Noncritical messages, such
as marketing messages. Amazon SNS optimizes the message delivery to
incur the lowest cost.</p></li>
<li><p><code>Transactional</code> – Critical messages that support
customer transactions, such as one-time passcodes for multi-factor
authentication. Amazon SNS optimizes the message delivery to achieve the
highest reliability.</p></li>
</ul>
<p><code>UsageReportS3Bucket</code> – The name of the Amazon S3 bucket
to receive daily SMS usage reports from Amazon SNS. Each day, Amazon SNS
will deliver a usage report as a CSV file to the bucket. The report
includes the following information for each SMS message that was
successfully delivered by your Amazon Web Services account:</p>
<ul>
<li><p>Time that the message was published (in UTC)</p></li>
<li><p>Message ID</p></li>
<li><p>Destination phone number</p></li>
<li><p>Message type</p></li>
<li><p>Delivery status</p></li>
<li><p>Message price (in USD)</p></li>
<li><p>Part number (a message is split into multiple parts if it is too
long for a single message)</p></li>
<li><p>Total number of parts</p></li>
</ul>
<p>To receive the report, the bucket must have a policy that allows the
Amazon SNS service principal to perform the <code>s3:PutObject</code>
and <code>s3:GetBucketLocation</code> actions.</p>
<p>For an example bucket policy and usage report, see <a
href="https://docs.aws.amazon.com/sns/latest/dg/sms_stats.html">Monitoring
SMS Activity</a> in the <em>Amazon SNS Developer Guide</em>.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$set_sms_attributes(
      attributes = list(
        "string"
      )
    )