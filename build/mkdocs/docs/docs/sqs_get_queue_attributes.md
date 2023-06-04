<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>sqs_get_queue_attributes</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets attributes for the specified queue

### Description

Gets attributes for the specified queue.

To determine whether a queue is
[FIFO](https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html),
you can check whether `QueueName` ends with the `.fifo` suffix.

### Usage

    sqs_get_queue_attributes(QueueUrl, AttributeNames)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="sqs_get_queue_attributes_:_QueueUrl">QueueUrl</code></td>
<td><p>[required] The URL of the Amazon SQS queue whose attribute
information is retrieved.</p>
<p>Queue URLs and names are case-sensitive.</p></td>
</tr>
<tr class="even">
<td><code
id="sqs_get_queue_attributes_:_AttributeNames">AttributeNames</code></td>
<td><p>A list of attributes for which to retrieve information.</p>
<p>The <code>AttributeName.N</code> parameter is optional, but if you
don't specify values for this parameter, the request returns empty
results.</p>
<p>In the future, new attributes might be added. If you write code that
calls this action, we recommend that you structure your code so that it
can handle new attributes gracefully.</p>
<p>The following attributes are supported:</p>
<p>The <code>ApproximateNumberOfMessagesDelayed</code>,
<code>ApproximateNumberOfMessagesNotVisible</code>, and
<code>ApproximateNumberOfMessagesVisible</code> metrics may not achieve
consistency until at least 1 minute after the producers stop sending
messages. This period is required for the queue metadata to reach
eventual consistency.</p>
<ul>
<li><p><code>All</code> – Returns all values.</p></li>
<li><p><code>ApproximateNumberOfMessages</code> – Returns the
approximate number of messages available for retrieval from the
queue.</p></li>
<li><p><code>ApproximateNumberOfMessagesDelayed</code> – Returns the
approximate number of messages in the queue that are delayed and not
available for reading immediately. This can happen when the queue is
configured as a delay queue or when a message has been sent with a delay
parameter.</p></li>
<li><p><code>ApproximateNumberOfMessagesNotVisible</code> – Returns the
approximate number of messages that are in flight. Messages are
considered to be <em>in flight</em> if they have been sent to a client
but have not yet been deleted or have not yet reached the end of their
visibility window.</p></li>
<li><p><code>CreatedTimestamp</code> – Returns the time when the queue
was created in seconds (<a
href="https://en.wikipedia.org/wiki/Unix_time">epoch time</a>).</p></li>
<li><p><code>DelaySeconds</code> – Returns the default delay on the
queue in seconds.</p></li>
<li><p><code>LastModifiedTimestamp</code> – Returns the time when the
queue was last changed in seconds (<a
href="https://en.wikipedia.org/wiki/Unix_time">epoch time</a>).</p></li>
<li><p><code>MaximumMessageSize</code> – Returns the limit of how many
bytes a message can contain before Amazon SQS rejects it.</p></li>
<li><p><code>MessageRetentionPeriod</code> – Returns the length of time,
in seconds, for which Amazon SQS retains a message.</p></li>
<li><p><code>Policy</code> – Returns the policy of the queue.</p></li>
<li><p><code>QueueArn</code> – Returns the Amazon resource name (ARN) of
the queue.</p></li>
<li><p><code>ReceiveMessageWaitTimeSeconds</code> – Returns the length
of time, in seconds, for which the <code>receive_message</code> action
waits for a message to arrive.</p></li>
<li><p><code>RedrivePolicy</code> – The string that includes the
parameters for the dead-letter queue functionality of the source queue
as a JSON object. For more information about the redrive policy and
dead-letter queues, see <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html">Using
Amazon SQS Dead-Letter Queues</a> in the <em>Amazon SQS Developer
Guide</em>.</p>
<ul>
<li><p><code>deadLetterTargetArn</code> – The Amazon Resource Name (ARN)
of the dead-letter queue to which Amazon SQS moves messages after the
value of <code>maxReceiveCount</code> is exceeded.</p></li>
<li><p><code>maxReceiveCount</code> – The number of times a message is
delivered to the source queue before being moved to the dead-letter
queue. When the <code>ReceiveCount</code> for a message exceeds the
<code>maxReceiveCount</code> for a queue, Amazon SQS moves the message
to the dead-letter-queue.</p></li>
</ul></li>
<li><p><code>VisibilityTimeout</code> – Returns the visibility timeout
for the queue. For more information about the visibility timeout, see <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html">Visibility
Timeout</a> in the <em>Amazon SQS Developer Guide</em>.</p></li>
</ul>
<p>The following attributes apply only to <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html">server-side-encryption</a>:</p>
<ul>
<li><p><code>KmsMasterKeyId</code> – Returns the ID of an Amazon Web
Services managed customer master key (CMK) for Amazon SQS or a custom
CMK. For more information, see <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms">Key
Terms</a>.</p></li>
<li><p><code>KmsDataKeyReusePeriodSeconds</code> – Returns the length of
time, in seconds, for which Amazon SQS can reuse a data key to encrypt
or decrypt messages before calling KMS again. For more information, see
<a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work">How
Does the Data Key Reuse Period Work?</a>.</p></li>
<li><p><code>SqsManagedSseEnabled</code> – Returns information about
whether the queue is using SSE-SQS encryption using SQS owned encryption
keys. Only one server-side encryption option is supported per queue
(e.g. <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sse-existing-queue.html">SSE-KMS</a>
or <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sqs-sse-queue.html">SSE-SQS</a>).</p></li>
</ul>
<p>The following attributes apply only to <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html">FIFO
(first-in-first-out) queues</a>:</p>
<ul>
<li><p><code>FifoQueue</code> – Returns information about whether the
queue is FIFO. For more information, see <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-understanding-logic.html">FIFO
queue logic</a> in the <em>Amazon SQS Developer Guide</em>.</p>
<p>To determine whether a queue is <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html">FIFO</a>,
you can check whether <code>QueueName</code> ends with the
<code>.fifo</code> suffix.</p></li>
<li><p><code>ContentBasedDeduplication</code> – Returns whether
content-based deduplication is enabled for the queue. For more
information, see <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-exactly-once-processing.html">Exactly-once
processing</a> in the <em>Amazon SQS Developer Guide</em>.</p></li>
</ul>
<p>The following attributes apply only to <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/high-throughput-fifo.html">high
throughput for FIFO queues</a>:</p>
<ul>
<li><p><code>DeduplicationScope</code> – Specifies whether message
deduplication occurs at the message group or queue level. Valid values
are <code>messageGroup</code> and <code>queue</code>.</p></li>
<li><p><code>FifoThroughputLimit</code> – Specifies whether the FIFO
queue throughput quota applies to the entire queue or per message group.
Valid values are <code>perQueue</code> and
<code>perMessageGroupId</code>. The <code>perMessageGroupId</code> value
is allowed only when the value for <code>DeduplicationScope</code> is
<code>messageGroup</code>.</p></li>
</ul>
<p>To enable high throughput for FIFO queues, do the following:</p>
<ul>
<li><p>Set <code>DeduplicationScope</code> to
<code>messageGroup</code>.</p></li>
<li><p>Set <code>FifoThroughputLimit</code> to
<code>perMessageGroupId</code>.</p></li>
</ul>
<p>If you set these attributes to anything other than the values shown
for enabling high throughput, normal throughput is in effect and
deduplication occurs as specified.</p>
<p>For information on throughput quotas, see <a
href="https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/quotas-messages.html">Quotas
related to messages</a> in the <em>Amazon SQS Developer
Guide</em>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Attributes = list(
        "string"
      )
    )

### Request syntax

    svc$get_queue_attributes(
      QueueUrl = "string",
      AttributeNames = list(
        "All"|"Policy"|"VisibilityTimeout"|"MaximumMessageSize"|"MessageRetentionPeriod"|"ApproximateNumberOfMessages"|"ApproximateNumberOfMessagesNotVisible"|"CreatedTimestamp"|"LastModifiedTimestamp"|"QueueArn"|"ApproximateNumberOfMessagesDelayed"|"DelaySeconds"|"ReceiveMessageWaitTimeSeconds"|"RedrivePolicy"|"FifoQueue"|"ContentBasedDeduplication"|"KmsMasterKeyId"|"KmsDataKeyReusePeriodSeconds"|"DeduplicationScope"|"FifoThroughputLimit"|"RedriveAllowPolicy"|"SqsManagedSseEnabled"
      )
    )