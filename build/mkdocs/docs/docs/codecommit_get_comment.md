<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>codecommit_get_comment</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns the content of a comment made on a change, file, or commit in a repository

### Description

Returns the content of a comment made on a change, file, or commit in a
repository.

Reaction counts might include numbers from user identities who were
deleted after the reaction was made. For a count of reactions from
active identities, use GetCommentReactions.

### Usage

    codecommit_get_comment(commentId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="codecommit_get_comment_:_commentId">commentId</code></td>
<td><p>[required] The unique, system-generated ID of the comment. To get
this ID, use <code>get_comments_for_compared_commit</code> or
<code>get_comments_for_pull_request</code>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      comment = list(
        commentId = "string",
        content = "string",
        inReplyTo = "string",
        creationDate = as.POSIXct(
          "2015-01-01"
        ),
        lastModifiedDate = as.POSIXct(
          "2015-01-01"
        ),
        authorArn = "string",
        deleted = TRUE|FALSE,
        clientRequestToken = "string",
        callerReactions = list(
          "string"
        ),
        reactionCounts = list(
          123
        )
      )
    )

### Request syntax

    svc$get_comment(
      commentId = "string"
    )