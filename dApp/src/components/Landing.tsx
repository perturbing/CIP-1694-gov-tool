import React, { useContext } from 'react';
import { AppStateContext } from '@/pages/_app';

export default function Landing() {
  const { _appState } = useContext(AppStateContext);

  return (
    <div className="p-4">
      <h1 className="text-lg font-bold">Welcome to this Constitutional Committee Management Application</h1>
      <p>This application integrates OpenSSL in the browser for managing your organisation in the Committee. </p>
      <p>Here's what you can do:</p>
      <ul className="list-disc ml-8 mt-4">
        <li>To write later :)</li>
      </ul>
    </div>
  );
}
