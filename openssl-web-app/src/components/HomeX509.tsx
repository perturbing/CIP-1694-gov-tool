import React, { useContext } from 'react';
import { AppStateContext } from '@/pages/_app';

export default function HomeX509() {
  const { _appState } = useContext(AppStateContext);

  return (
    <div className="p-4">
      <h1 className="text-lg font-bold">Welcome to this X.509 Certificate Management Application</h1>
      <p>This application integrates OpenSSL in the browser for managing X.509 certificates over ed25519. </p>
      <p>Here's what you can do:</p>
      <ul className="list-disc ml-8 mt-4">
        <li>Create Self-Signed X.509 Certificates (CA).</li>
        <li>Create Certificate Signing Requests (CSRs).</li>
        <li>Issue Child Certificates.</li>
        <li>Verify X.509 Certificates</li>
      </ul>
    </div>
  );
}
