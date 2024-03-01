import React from 'react';
import { fromHex, fromText, sha256, toHex } from "lucid-cardano";
import { callOpenSSL } from '@/utils/utils';

export default function VerifyX509({certs, setCerts}) {
    const openOpenSSLCerts = async () => {
        const fileInput = document.createElement('input');
        fileInput.type = 'file';
        fileInput.style.display = 'none';

        fileInput.onchange = async (e) => {
            const file = e.target.files[0];
            if (file) {
                const reader = new FileReader();
                reader.onload = async (e) => {
                    const text = e.target.result;
                    callOpenSSL('viewX509', text as string, "error viewing certificate").then(data => {
                        setCerts('caCert', text as string);
                        setCerts('caCertHash', toHex(sha256(fromHex(fromText(text as string)))));
                    }).catch(err => {
                        window.alert("that was not a valid x509 certificate: " +err);
                    });
                };
                reader.readAsText(file);
            }
        };
        fileInput.click();
    };

    const handleCertInputChange = (e) => {
        setCerts('toCheckCert', e.target.value);
        callOpenSSL('viewX509', e.target.value, "error viewing certificate").then(data => {
            callOpenSSL('verifyX509', { caCert: certs.caCert, toCheckCert: e.target.value }, "error verifying certificate").then(data2 => {
                setCerts('isValidCert', true);
            }).catch(err => {
                setCerts('isValidCert', false);
            });
        }).catch(err2 => {
            setCerts('isValidCert', false);
        });
    };

    // Dynamically set the background color based on isValidCert
    const bgColorClass = certs.isValidCert ? "bg-green-500" : "bg-white";

    return (
        <div className={`p-4 ${bgColorClass} bg-opacity-90 rounded shadow`}>
            <div className="mb-4">
                <button
                    className="bg-blue-500 text-white px-4 py-2 rounded flex items-center justify-center gap-2"
                    onClick={openOpenSSLCerts}
                >
                    Open CA certificate
                </button>
            </div>
            <div>
                <p className="text-center text-lg text-gray-600">CA certificate Hash: {certs.caCertHash}</p>
                <textarea
                    className="w-full p-2 border text-gray-600 border-gray-300 rounded mt-4"
                    rows="8"
                    placeholder="-----BEGIN CERTIFICATE-----"
                    onChange={handleCertInputChange}
                    value={certs.toCheckCert || ''}
                />
            </div>
        </div>
    );
}
