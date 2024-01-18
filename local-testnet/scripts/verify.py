import sys
from cryptography import x509
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives import serialization

# Check if the required command-line arguments are provided
if len(sys.argv) != 3:
    print("Usage: script.py <Parent_certificate_file> <Child_certificate_file>")
    sys.exit(1)

# File paths from command-line arguments
parent_cert_file = sys.argv[1]
child_cert_file = sys.argv[2]

# Load parent certificate
with open(parent_cert_file, 'rb') as f:
    parent_cert = x509.load_pem_x509_certificate(f.read(), default_backend())

# Load child's certificate
with open(child_cert_file, 'rb') as f:
    child_cert = x509.load_pem_x509_certificate(f.read(), default_backend())

tbs_bytes = child_cert.tbs_certificate_bytes
print(tbs_bytes.hex())
print(" ")
print(child_cert.signature.hex())

try:
    # Verify child's certificate against parent
    parent_public_key = parent_cert.public_key()
    print(" ")
    print(parent_public_key.public_bytes(encoding=serialization.Encoding.Raw, format=serialization.PublicFormat.Raw).hex())
    print(" ")
    parent_public_key.verify(
        child_cert.signature,
        child_cert.tbs_certificate_bytes,
    )
    print("Verification successful: certificate is valid.")
except Exception as e:
    print("Verification failed:", e)
