<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>elb_create_lb_cookie_stickiness_policy</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Generates a stickiness policy with sticky session lifetimes controlled by the lifetime of the browser (user-agent) or a specified expiration period

### Description

Generates a stickiness policy with sticky session lifetimes controlled
by the lifetime of the browser (user-agent) or a specified expiration
period. This policy can be associated only with HTTP/HTTPS listeners.

When a load balancer implements this policy, the load balancer uses a
special cookie to track the instance for each request. When the load
balancer receives a request, it first checks to see if this cookie is
present in the request. If so, the load balancer sends the request to
the application server specified in the cookie. If not, the load
balancer sends the request to a server that is chosen based on the
existing load-balancing algorithm.

A cookie is inserted into the response for binding subsequent requests
from the same user to that server. The validity of the cookie is based
on the cookie expiration time, which is specified in the policy
configuration.

For more information, see [Duration-Based Session
Stickiness](https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration)
in the *Classic Load Balancers Guide*.

### Usage

    elb_create_lb_cookie_stickiness_policy(LoadBalancerName, PolicyName,
      CookieExpirationPeriod)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="elb_create_lb_cookie_stickiness_policy_:_LoadBalancerName">LoadBalancerName</code></td>
<td><p>[required] The name of the load balancer.</p></td>
</tr>
<tr class="even">
<td><code
id="elb_create_lb_cookie_stickiness_policy_:_PolicyName">PolicyName</code></td>
<td><p>[required] The name of the policy being created. Policy names
must consist of alphanumeric characters and dashes (-). This name must
be unique within the set of policies for this load balancer.</p></td>
</tr>
<tr class="odd">
<td><code
id="elb_create_lb_cookie_stickiness_policy_:_CookieExpirationPeriod">CookieExpirationPeriod</code></td>
<td><p>The time period, in seconds, after which the cookie should be
considered stale. If you do not specify this parameter, the default
value is 0, which indicates that the sticky session should last for the
duration of the browser session.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$create_lb_cookie_stickiness_policy(
      LoadBalancerName = "string",
      PolicyName = "string",
      CookieExpirationPeriod = 123
    )

### Examples

    ## Not run: 
    # This example generates a stickiness policy with sticky session lifetimes
    # controlled by the specified expiration period.
    svc$create_lb_cookie_stickiness_policy(
      CookieExpirationPeriod = 60L,
      LoadBalancerName = "my-load-balancer",
      PolicyName = "my-duration-cookie-policy"
    )

    ## End(Not run)