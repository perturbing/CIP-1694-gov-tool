import sys
from cryptography import x509
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives import serialization

# Check if the required command-line arguments are provided
if len(sys.argv) != 3:
    print("Usage: script.py <CA_certificate_file> <Alice_certificate_file>")
    sys.exit(1)

# File paths from command-line arguments
ca_cert_file = sys.argv[1]
alice_cert_file = sys.argv[2]

# Load CA certificate
with open(ca_cert_file, 'rb') as f:
    ca_cert = x509.load_pem_x509_certificate(f.read(), default_backend())

# Load Alice's certificate
with open(alice_cert_file, 'rb') as f:
    alice_cert = x509.load_pem_x509_certificate(f.read(), default_backend())

tbs_bytes = alice_cert.tbs_certificate_bytes
print(tbs_bytes.hex())
print(" ")
print(alice_cert.signature.hex())

try:
    # Verify Alice's certificate
    ca_public_key = ca_cert.public_key()
    ca_public_key.verify(
        alice_cert.signature,
        alice_cert.tbs_certificate_bytes,
    )
    print("Verification successful: certificate is valid.")
except Exception as e:
    print("Verification failed:", e)
