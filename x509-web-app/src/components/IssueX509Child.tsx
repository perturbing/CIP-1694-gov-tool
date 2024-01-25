import React, { useContext } from 'react';
import { AppStateContext } from '@/pages/_app';

export default function IssueX509Child() {
    const { appState } = useContext(AppStateContext);

// Check if the wallet is connected
    if (!appState.pubKeyHash) {
        return (
            <div className="p-4 bg-gray-100 rounded shadow">
                <p className="text-center text-lg text-gray-600">Please connect your lace wallet via the blue button in the top right corner.</p>
            </div>
        );
    }

    // Check if the public key is available
    if (!appState.pubKey) {
        return (
            <div className="p-4 bg-gray-100 rounded shadow">
                <p className="text-center text-lg text-gray-600">
                    To manage a certificate, the public key of you wallets needs to be verified. 
                    This requires the wallet to sign a message that contains the current date and time.
                    Please press the green button in the top right corner to sign this message.
                </p>
            </div>
        );
    }

    // If both pubKeyHash and pubKey are available
    return (
        <div className="p-4 bg-gray-100 rounded shadow">
            <p className="text-center text-lg text-gray-600">Hello World</p>
        </div>
    );
} 