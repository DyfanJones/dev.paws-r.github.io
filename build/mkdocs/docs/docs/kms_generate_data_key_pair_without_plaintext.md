<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>kms_generate_data_key_pair_without_plaintext</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns a unique asymmetric data key pair for use outside of KMS

### Description

Returns a unique asymmetric data key pair for use outside of KMS. This
operation returns a plaintext public key and a copy of the private key
that is encrypted under the symmetric encryption KMS key you specify.
Unlike `generate_data_key_pair`, this operation does not return a
plaintext private key. The bytes in the keys are random; they are not
related to the caller or to the KMS key that is used to encrypt the
private key.

You can use the public key that
`generate_data_key_pair_without_plaintext` returns to encrypt data or
verify a signature outside of KMS. Then, store the encrypted private key
with the data. When you are ready to decrypt data or sign a message, you
can use the `decrypt` operation to decrypt the encrypted private key.

To generate a data key pair, you must specify a symmetric encryption KMS
key to encrypt the private key in a data key pair. You cannot use an
asymmetric KMS key or a KMS key in a custom key store. To get the type
and origin of your KMS key, use the `describe_key` operation.

Use the `KeyPairSpec` parameter to choose an RSA or Elliptic Curve (ECC)
data key pair. In China Regions, you can also choose an SM2 data key
pair. KMS recommends that you use ECC key pairs for signing, and use RSA
and SM2 key pairs for either encryption or signing, but not both.
However, KMS cannot enforce any restrictions on the use of data key
pairs outside of KMS.

`generate_data_key_pair_without_plaintext` returns a unique data key
pair for each request. The bytes in the key are not related to the
caller or KMS key that is used to encrypt the private key. The public
key is a DER-encoded X.509 SubjectPublicKeyInfo, as specified in [RFC
5280](https://datatracker.ietf.org/doc/html/rfc5280).

You can use an optional encryption context to add additional security to
the encryption operation. If you specify an `EncryptionContext`, you
must specify the same encryption context (a case-sensitive exact match)
when decrypting the encrypted data key. Otherwise, the request to
decrypt fails with an `InvalidCiphertextException`. For more
information, see [Encryption
Context](https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context)
in the *Key Management Service Developer Guide*.

The KMS key that you use for this operation must be in a compatible key
state. For details, see [Key states of KMS
keys](https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html)
in the *Key Management Service Developer Guide*.

**Cross-account use**: Yes. To perform this operation with a KMS key in
a different Amazon Web Services account, specify the key ARN or alias
ARN in the value of the `KeyId` parameter.

**Required permissions**:
[kms:GenerateDataKeyPairWithoutPlaintext](https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html)
(key policy)

**Related operations:**

-   `decrypt`

-   `encrypt`

-   `generate_data_key`

-   `generate_data_key_pair`

-   `generate_data_key_without_plaintext`

### Usage

    kms_generate_data_key_pair_without_plaintext(EncryptionContext, KeyId,
      KeyPairSpec, GrantTokens)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="kms_generate_data_key_pair_without_plaintext_:_EncryptionContext">EncryptionContext</code></td>
<td><p>Specifies the encryption context that will be used when
encrypting the private key in the data key pair.</p>
<p>Do not include confidential or sensitive information in this field.
This field may be displayed in plaintext in CloudTrail logs and other
output.</p>
<p>An <em>encryption context</em> is a collection of non-secret
key-value pairs that represent additional authenticated data. When you
use an encryption context to encrypt data, you must specify the same (an
exact case-sensitive match) encryption context to decrypt the data. An
encryption context is supported only on operations with symmetric
encryption KMS keys. On operations with symmetric encryption KMS keys,
an encryption context is optional, but it is strongly recommended.</p>
<p>For more information, see <a
href="https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context">Encryption
context</a> in the <em>Key Management Service Developer
Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code
id="kms_generate_data_key_pair_without_plaintext_:_KeyId">KeyId</code></td>
<td><p>[required] Specifies the symmetric encryption KMS key that
encrypts the private key in the data key pair. You cannot specify an
asymmetric KMS key or a KMS key in a custom key store. To get the type
and origin of your KMS key, use the <code>describe_key</code>
operation.</p>
<p>To specify a KMS key, use its key ID, key ARN, alias name, or alias
ARN. When using an alias name, prefix it with <code>"alias/"</code>. To
specify a KMS key in a different Amazon Web Services account, you must
use the key ARN or alias ARN.</p>
<p>For example:</p>
<ul>
<li><p>Key ID: <code
style="white-space: pre;">⁠1234abcd-12ab-34cd-56ef-1234567890ab⁠</code></p></li>
<li><p>Key ARN: <code
style="white-space: pre;">⁠arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab⁠</code></p></li>
<li><p>Alias name: <code>alias/ExampleAlias</code></p></li>
<li><p>Alias ARN:
<code>arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias</code></p></li>
</ul>
<p>To get the key ID and key ARN for a KMS key, use
<code>list_keys</code> or <code>describe_key</code>. To get the alias
name and alias ARN, use <code>list_aliases</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="kms_generate_data_key_pair_without_plaintext_:_KeyPairSpec">KeyPairSpec</code></td>
<td><p>[required] Determines the type of data key pair that is
generated.</p>
<p>The KMS rule that restricts the use of asymmetric RSA and SM2 KMS
keys to encrypt and decrypt or to sign and verify (but not both), and
the rule that permits you to use ECC KMS keys only to sign and verify,
are not effective on data key pairs, which are used outside of KMS. The
SM2 key spec is only available in China Regions.</p></td>
</tr>
<tr class="even">
<td><code
id="kms_generate_data_key_pair_without_plaintext_:_GrantTokens">GrantTokens</code></td>
<td><p>A list of grant tokens.</p>
<p>Use a grant token when your permission to call this operation comes
from a new grant that has not yet achieved <em>eventual
consistency</em>. For more information, see <a
href="https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token">Grant
token</a> and <a
href="https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token">Using
a grant token</a> in the <em>Key Management Service Developer
Guide</em>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      PrivateKeyCiphertextBlob = raw,
      PublicKey = raw,
      KeyId = "string",
      KeyPairSpec = "RSA_2048"|"RSA_3072"|"RSA_4096"|"ECC_NIST_P256"|"ECC_NIST_P384"|"ECC_NIST_P521"|"ECC_SECG_P256K1"|"SM2"
    )

### Request syntax

    svc$generate_data_key_pair_without_plaintext(
      EncryptionContext = list(
        "string"
      ),
      KeyId = "string",
      KeyPairSpec = "RSA_2048"|"RSA_3072"|"RSA_4096"|"ECC_NIST_P256"|"ECC_NIST_P384"|"ECC_NIST_P521"|"ECC_SECG_P256K1"|"SM2",
      GrantTokens = list(
        "string"
      )
    )

### Examples

    ## Not run: 
    # This example returns an asymmetric elliptic curve (ECC) data key pair.
    # The private key is encrypted under the symmetric encryption KMS key that
    # you specify. This operation doesn't return a plaintext (unencrypted)
    # private key.
    svc$generate_data_key_pair_without_plaintext(
      KeyId = "arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab",
      KeyPairSpec = "ECC_NIST_P521"
    )

    ## End(Not run)
