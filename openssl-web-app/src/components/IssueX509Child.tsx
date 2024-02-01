import { callOpenSSL, copyToClipboard } from "@/utils/utils";

export default function IssueX509Child({ csr, setCSR, privKey }) {

    // Handler to update CSR state when the user pastes the CSR
    const handleCSRChange = async (e) => {
        setCSR('csr', e.target.value)
        callOpenSSL('viewCSR', e.target.value , 'Error viewing CSR')
            .then(data => {
                setCSR('decodedData', data.result);
            })
            .catch(error => {
                setCSR('decodedData', 'Error decoding CSR')
            });
    };

    // Example action function for the button
    const handleButtonClick = () => {
        console.log('CSR: ' + csr.csr);

    };

    return (
        <div className="flex flex-col w-full">
            <div className="p-4 bg-gray-100 rounded shadow m-2">
                <p className="text-center text-lg text-gray-600">Paste your own X.509 certificate</p>
                <textarea
                    value={csr.ownCert}
                    onChange={(e) => setCSR('ownCert', e.target.value)}
                    className="w-full p-2 border text-gray-600 border-gray-300 rounded"
                    rows="8"
                    placeholder="-----BEGIN CERTIFICATE-----"
                />
            </div>
            <div className="p-4 bg-gray-100 rounded shadow m-2">
                <p className="text-center text-lg text-gray-600">Paste the CSR</p>
                <textarea
                    value={csr.csr}
                    onChange={handleCSRChange}
                    className="w-full p-2 border text-gray-600 border-gray-300 rounded"
                    rows="8"
                    placeholder="-----BEGIN CERTIFICATE REQUEST-----"
                />
            </div>
            <div className="p-4 bg-gray-100 rounded shadow m-2">
                <p className="text-center text-lg text-gray-600">Decoded CSR Data</p>
                <pre className="w-full p-2 border text-gray-600 border-gray-300 rounded whitespace-pre-wrap">{csr.decodedData}</pre>
            </div>
            {/* Centered Button */}
            <div className="flex justify-center p-4">
                <button
                    onClick={handleButtonClick}
                    className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                >
                    Copy signed certificate to clipboard
                </button>
            </div>
        </div>
    );
}
