import React, { useContext } from 'react';
import { AppStateContext } from '@/pages/_app';

export default function Deploy({deployedState}) {
    return (
        <div className="p-4 bg-gray-100 rounded shadow">
            <p className="text-center text-lg text-gray-600">Deployed: {deployedState.toString()}</p>
        </div>
    );
}