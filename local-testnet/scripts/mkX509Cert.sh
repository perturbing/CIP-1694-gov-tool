#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -ne 4 ]; then
    echo "Usage: $0 <parent_cert_location> <parent_priv_key_location> <output_location> <child_certificate_name>"
    exit 1
fi

PARENT_CERT_LOCATION=$1
PARENT_PRIV_KEY_LOCATION=$2
OUTPUT_LOCATION=$3
CERTIFICATE_NAME=$4

# Create the required directories
mkdir -p "${OUTPUT_LOCATION}/cardano"
mkdir -p "${OUTPUT_LOCATION}/openssl"

# Navigate to the cardano directory
cd "${OUTPUT_LOCATION}/cardano"

# Generate the verification and signing key files
cardano-cli address key-gen --verification-key-file ${CERTIFICATE_NAME}.vkey --signing-key-file ${CERTIFICATE_NAME}.skey

# Generate the key hash
cardano-cli address key-hash --payment-verification-key-file ${CERTIFICATE_NAME}.vkey > ${CERTIFICATE_NAME}.keyHash

# Convert the signing key to an OpenSSL private key and store it
cat ${CERTIFICATE_NAME}.skey | jq -r ".cborHex" | cut -c 5- | (echo -n "302e020100300506032b657004220420" && cat) | xxd -r -p | base64 | (echo "-----BEGIN PRIVATE KEY-----" && cat) | (cat && echo "-----END PRIVATE KEY-----") > ../openssl/${CERTIFICATE_NAME}-priv.pem

# Navigate to the openssl directory
cd ../openssl

# Generate the public key from the private key
openssl pkey -in ${CERTIFICATE_NAME}-priv.pem -pubout -out ${CERTIFICATE_NAME}-pub.pem

# Create a CSR using the private key
openssl req -new -key ${CERTIFICATE_NAME}-priv.pem -out ${CERTIFICATE_NAME}.csr

echo $(pwd)
# Create an x509 certificate signed by the parent CA
openssl x509 -days 365 -req -in ${CERTIFICATE_NAME}.csr -CA ../../../${PARENT_CERT_LOCATION} -CAkey ../../../${PARENT_PRIV_KEY_LOCATION} -out ${CERTIFICATE_NAME}.pem

echo "Process completed successfully."
