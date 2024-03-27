# OpenSSl and Cardano
In this document, we describe how to generate keys via the CLI and issue certificates using OpenSSL to create a basic X509 CA and X509 children. In short, X509 binds an identity to a public key using a digital signature.

# Generating keys and converting them to a PEM file.
OpenSSL does allow the use of the Ed25519 signature scheme for its certificates, it just reads this in a different format than the text-envelopes that the CLI uses. To generate a private/public key pair, you can use
```bash
cardano-cli address key-gen --signing-key-file ca.skey --verification-key-file ca.vkey
```
To get the public key hash from these keys, you can use
```bash
cardano-cli address key-hash --payment-verification-key-file ca.vkey > ca.keyhash
```
then you can convert these keys to their equivalent PEM files via
```bash
cat ca.skey | jq -r ".cborHex" | cut -c 5- | (echo -n "302e020100300506032b657004220420" && cat) | xxd -r -p | base64 | (echo "-----BEGIN PRIVATE KEY-----" && cat) | (cat && echo "-----END PRIVATE KEY-----") > ca-priv.pem
```
and
```bash
openssl pkey -in ca-priv.pem -pubout -out ca-pub.pem
```
to get the public key. It is good to know that with OpenSSL you can parse and check this via
```bash
openssl asn1parse < ca-priv.pem
```
Which dumps the data in stdout, check that the byte dump lines up with the last 32 characters of `ca.skey` (the `cborhex` minus the `5820`).

# Create a self-signed certificate (CA)
To start, create a `CA` folder, and generate keys for the CA via the above. Then, to create a self-signed CA, we use 
```bash
openssl req -x509 -new -key ca-priv.pem -days 3650 -out ca-cert.pem
```
This will prompt you for some basic attribute information of the CA. This interactive command can also be done via
```bash
openssl req -x509 -new -key ca-priv.pem -days 3650 -out ca-cert.pem -subj "/C=NL/ST=YourState/L=YourCity/O=YourOrganization/OU=YourOrganizationalUnit/CN=YourCommonName"
```
You can view the certificate via
```bash
openssl x509 -in ca-cert.pem -text -noout
```
# how to issue a subject
To generate a private/public key pair, you can again use
```bash
cardano-cli address key-gen --signing-key-file child-1.skey --verification-key-file child-1.vkey
```
To get the public key hash from these keys, you can use
```bash
cardano-cli address key-hash --payment-verification-key-file child-1.vkey > child-1.keyhasash
```
then you can convert these keys to their equivalent PEM files via
```bash
cat child-1.skey | jq -r ".cborHex" | cut -c 5- | (echo -n "302e020100300506032b657004220420" && cat) | xxd -r -p | base64 | (echo "-----BEGIN PRIVATE KEY-----" && cat) | (cat && echo "-----END PRIVATE KEY-----") > child-1-priv.pem
```
and
```bash
openssl pkey -in child-1-priv.pem -pubout -out child-1-pub.pem
```
to get the public key. Again, you can check your generated pem files via
```bash
openssl asn1parse < child-1-priv.pem
```
To create a child certificate, we need to first create a CSR (certificate signing request) via
```bash
openssl req -new -key child-1-priv.pem -out child-1.csr
```
and leave the extra attributes empty. Then as a CA we can view this request via
```bash
openssl req -in child-1.csr -text -noout
```
to verify the attributes of the subject. Then the CA can sign/issue it via
```bash
openssl x509 -days 365 -req -in child-1.csr -CA ../../ca-cert.pem -CAkey ../../ca-priv.pem -out child-1-cert.pem
```
You can view the certificate again via
```bash
openssl x509 -in child-1-cert.pem -text -noout
```
Which shows who the issuer is. To check a certificate against a CA, you can use
```bash
openssl verify -CAfile ../../ca-cert.pem child-1-cert.pem
```
You can calculate the sha256 hash of the certificate via
```bash
sha256 child-1-cert.pem | awk '{print $4}' > child-1-cert.hash
```
In the folder CA in this repo, you will find an example of a basic CA, as above. Note that an important part of X509 is the revocation of certificates, the basic example above does not implement this for the sake of simplicity. Also note that for a more elaborate CA tree with, for example, intermediate CA, you will need to use extra extensions. Both functionalities are explained [here](https://openssl-ca.readthedocs.io/en/latest/introduction.html).