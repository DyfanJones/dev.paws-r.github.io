<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>health</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## AWS Health APIs and Notifications

### Description

Health

The Health API provides access to the Health information that appears in
the [Health Dashboard](https://health.aws.amazon.com/health/home). You
can use the API operations to get information about events that might
affect your Amazon Web Services and resources.

You must have a Business, Enterprise On-Ramp, or Enterprise Support plan
from [Amazon Web Services
Support](https://aws.amazon.com/premiumsupport/) to use the Health API.
If you call the Health API from an Amazon Web Services account that
doesn't have a Business, Enterprise On-Ramp, or Enterprise Support plan,
you receive a `SubscriptionRequiredException` error.

For API access, you need an access key ID and a secret access key. Use
temporary credentials instead of long-term access keys when possible.
Temporary credentials include an access key ID, a secret access key, and
a security token that indicates when the credentials expire. For more
information, see [Best practices for managing Amazon Web Services access
keys](https://docs.aws.amazon.com/accounts/latest/reference/best-practices.html)
in the *Amazon Web Services General Reference*.

You can use the Health endpoint health.us-east-1.amazonaws.com (HTTPS)
to call the Health API operations. Health supports a multi-Region
application architecture and has two regional endpoints in an
active-passive configuration. You can use the high availability endpoint
example to determine which Amazon Web Services Region is active, so that
you can get the latest information from the API. For more information,
see [Accessing the Health
API](https://docs.aws.amazon.com/health/latest/ug/health-api.html) in
the *Health User Guide*.

For authentication of requests, Health uses the [Signature Version 4
Signing
Process](https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-signing.html).

If your Amazon Web Services account is part of Organizations, you can
use the Health organizational view feature. This feature provides a
centralized view of Health events across all accounts in your
organization. You can aggregate Health events in real time to identify
accounts in your organization that are affected by an operational event
or get notified of security vulnerabilities. Use the organizational view
API operations to enable this feature and return event information. For
more information, see [Aggregating Health
events](https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html)
in the *Health User Guide*.

When you use the Health API operations to return Health events, see the
following recommendations:

-   Use the
    [eventScopeCode](https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html#AWSHealth-Type-Event-eventScopeCode)
    parameter to specify whether to return Health events that are public
    or account-specific.

-   Use pagination to view all events from the response. For example, if
    you call the `describe_events_for_organization` operation to get all
    events in your organization, you might receive several page results.
    Specify the `nextToken` in the next request to return more results.

### Usage

    health(config = list())

### Arguments

<table>
<colgroup>
<col style="width: 15%" />
<col style="width: 85%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="health_:_config">config</code></td>
<td><p>Optional configuration of credentials, endpoint, and/or
region.</p>
<ul>
<li><p><strong>access_key_id</strong>: AWS access key ID</p></li>
<li><p><strong>secret_access_key</strong>: AWS secret access
key</p></li>
<li><p><strong>session_token</strong>: AWS temporary session
token</p></li>
<li><p><strong>profile</strong>: The name of a profile to use. If not
given, then the default profile is used.</p></li>
<li><p><strong>anonymous</strong>: Set anonymous credentials.</p></li>
<li><p><strong>endpoint</strong>: The complete URL to use for the
constructed client.</p></li>
<li><p><strong>region</strong>: The AWS Region used in instantiating the
client.</p></li>
<li><p><strong>close_connection</strong>: Immediately close all HTTP
connections.</p></li>
<li><p><strong>timeout</strong>: The time in seconds till a timeout
exception is thrown when attempting to make a connection. The default is
60 seconds.</p></li>
<li><p><strong>s3_force_path_style</strong>: Set this to
<code>true</code> to force the request to use path-style addressing,
i.e., <code
style="white-space: pre;">⁠http://s3.amazonaws.com/BUCKET/KEY⁠</code>.</p></li>
</ul></td>
</tr>
</tbody>
</table>

### Value

A client for the service. You can call the service's operations using
syntax like `svc$operation(...)`, where `svc` is the name you've
assigned to the client. The available operations are listed in the
Operations section.

### Service syntax

    svc <- health(
      config = list(
        credentials = list(
          creds = list(
            access_key_id = "string",
            secret_access_key = "string",
            session_token = "string"
          ),
          profile = "string",
          anonymous = "logical"
        ),
        endpoint = "string",
        region = "string",
        close_connection = "logical",
        timeout = "numeric",
        s3_force_path_style = "logical"
      )
    )

### Operations

<table>
<tbody>
<tr class="odd">
<td
style="text-align: left;"><a href="../health_describe_affected_accounts_for_organization/"> describe_affected_accounts_for_organization </a></td>
<td style="text-align: left;">Returns a list of accounts in the
organization from Organizations that are affected by the provided
event</td>
</tr>
<tr class="even">
<td style="text-align: left;"><a href="../health_describe_affected_entities/"> describe_affected_entities </a></td>
<td style="text-align: left;">Returns a list of entities that have been
affected by the specified events, based on the specified filter
criteria</td>
</tr>
<tr class="odd">
<td
style="text-align: left;"><a href="../health_describe_affected_entities_for_organization/"> describe_affected_entities_for_organization </a></td>
<td style="text-align: left;">Returns a list of entities that have been
affected by one or more events for one or more accounts in your
organization in Organizations, based on the filter criteria</td>
</tr>
<tr class="even">
<td style="text-align: left;"><a href="../health_describe_entity_aggregates/"> describe_entity_aggregates </a></td>
<td style="text-align: left;">Returns the number of entities that are
affected by each of the specified events</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><a href="../health_describe_event_aggregates/"> describe_event_aggregates </a></td>
<td style="text-align: left;">Returns the number of events of each event
type (issue, scheduled change, and account notification)</td>
</tr>
<tr class="even">
<td style="text-align: left;"><a href="../health_describe_event_details/"> describe_event_details </a></td>
<td style="text-align: left;">Returns detailed information about one or
more specified events</td>
</tr>
<tr class="odd">
<td
style="text-align: left;"><a href="../health_describe_event_details_for_organization/"> describe_event_details_for_organization </a></td>
<td style="text-align: left;">Returns detailed information about one or
more specified events for one or more Amazon Web Services accounts in
your organization</td>
</tr>
<tr class="even">
<td style="text-align: left;"><a href="../health_describe_events/"> describe_events </a></td>
<td style="text-align: left;">Returns information about events that meet
the specified filter criteria</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><a href="../health_describe_events_for_organization/"> describe_events_for_organization </a></td>
<td style="text-align: left;">Returns information about events across
your organization in Organizations</td>
</tr>
<tr class="even">
<td style="text-align: left;"><a href="../health_describe_event_types/"> describe_event_types </a></td>
<td style="text-align: left;">Returns the event types that meet the
specified filter criteria</td>
</tr>
<tr class="odd">
<td
style="text-align: left;"><a href="../health_describe_health_service_status_for_organization/"> describe_health_service_status_for_organization </a></td>
<td style="text-align: left;">This operation provides status information
on enabling or disabling Health to work with your organization</td>
</tr>
<tr class="even">
<td
style="text-align: left;"><a href="../health_disable_health_service_access_for_organization/"> disable_health_service_access_for_organization </a></td>
<td style="text-align: left;">Disables Health from working with
Organizations</td>
</tr>
<tr class="odd">
<td
style="text-align: left;"><a href="../health_enable_health_service_access_for_organization/"> enable_health_service_access_for_organization </a></td>
<td style="text-align: left;">Enables Health to work with
Organizations</td>
</tr>
</tbody>
</table>

### Examples

    ## Not run: 
    svc <- health()
    svc$describe_affected_accounts_for_organization(
      Foo = 123
    )

    ## End(Not run)