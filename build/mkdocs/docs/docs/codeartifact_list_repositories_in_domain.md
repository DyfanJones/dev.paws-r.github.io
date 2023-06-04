<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>codeartifact_list_repositories_in_domain</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns a list of RepositorySummary objects

### Description

Returns a list of
[RepositorySummary](https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_RepositorySummary.html)
objects. Each `RepositorySummary` contains information about a
repository in the specified domain and that matches the input
parameters.

### Usage

    codeartifact_list_repositories_in_domain(domain, domainOwner,
      administratorAccount, repositoryPrefix, maxResults, nextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="codeartifact_list_repositories_in_domain_:_domain">domain</code></td>
<td><p>[required] The name of the domain that contains the returned list
of repositories.</p></td>
</tr>
<tr class="even">
<td><code
id="codeartifact_list_repositories_in_domain_:_domainOwner">domainOwner</code></td>
<td><p>The 12-digit account number of the Amazon Web Services account
that owns the domain. It does not include dashes or spaces.</p></td>
</tr>
<tr class="odd">
<td><code
id="codeartifact_list_repositories_in_domain_:_administratorAccount">administratorAccount</code></td>
<td><p>Filter the list of repositories to only include those that are
managed by the Amazon Web Services account ID.</p></td>
</tr>
<tr class="even">
<td><code
id="codeartifact_list_repositories_in_domain_:_repositoryPrefix">repositoryPrefix</code></td>
<td><p>A prefix used to filter returned repositories. Only repositories
with names that start with <code>repositoryPrefix</code> are
returned.</p></td>
</tr>
<tr class="odd">
<td><code
id="codeartifact_list_repositories_in_domain_:_maxResults">maxResults</code></td>
<td><p>The maximum number of results to return per page.</p></td>
</tr>
<tr class="even">
<td><code
id="codeartifact_list_repositories_in_domain_:_nextToken">nextToken</code></td>
<td><p>The token for the next set of results. Use the value returned in
the previous response in the next request to retrieve the next set of
results.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      repositories = list(
        list(
          name = "string",
          administratorAccount = "string",
          domainName = "string",
          domainOwner = "string",
          arn = "string",
          description = "string",
          createdTime = as.POSIXct(
            "2015-01-01"
          )
        )
      ),
      nextToken = "string"
    )

### Request syntax

    svc$list_repositories_in_domain(
      domain = "string",
      domainOwner = "string",
      administratorAccount = "string",
      repositoryPrefix = "string",
      maxResults = 123,
      nextToken = "string"
    )