import React from 'react';

export default function CreateX509CA({ formData, onFormDataChange }) {
    const handleSubmit = async (e) => {
        e.preventDefault();
        // Handle the form submission using formData
        // Add logic here to use formData for creating an X.509 certificate
        console.log(formData);
    };

    return (
        <div className="p-4 bg-white bg-opacity-90 rounded shadow">
            <form onSubmit={handleSubmit} className="space-y-4">
                <div>
                    <label htmlFor="country" className="block text-sm font-medium text-gray-700">Country</label>
                    <input type="text" id="country" value={formData.country} onChange={(e) => onFormDataChange('country', e.target.value)} className="mt-1 block w-full px-3 py-2 bg-white border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 text-gray-700" />
                </div>
                <div>
                    <label htmlFor="state" className="block text-sm font-medium text-gray-700">State</label>
                    <input type="text" id="state" value={formData.state} onChange={(e) => onFormDataChange('state', e.target.value)} className="mt-1 block w-full px-3 py-2 bg-white border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 text-gray-700" />
                </div>
                <div>
                    <label htmlFor="city" className="block text-sm font-medium text-gray-700">City</label>
                    <input type="text" id="city" value={formData.city} onChange={(e) => onFormDataChange('city', e.target.value)} className="mt-1 block w-full px-3 py-2 bg-white border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 text-gray-700" />
                </div>
                <div>
                    <label htmlFor="organization" className="block text-sm font-medium text-gray-700">Organization</label>
                    <input type="text" id="organization" value={formData.organization} onChange={(e) => onFormDataChange('organization', e.target.value)} className="mt-1 block w-full px-3 py-2 bg-white border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 text-gray-700" />
                </div>
                <div>
                    <label htmlFor="commonName" className="block text-sm font-medium text-gray-700">Common Name</label>
                    <input type="text" id="commonName" value={formData.commonName} onChange={(e) => onFormDataChange('commonName', e.target.value)} className="mt-1 block w-full px-3 py-2 bg-white border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 text-gray-700" />
                </div>
                <div>
                    <label htmlFor="email" className="block text-sm font-medium text-gray-700">Email</label>
                    <input type="email" id="email" value={formData.email} onChange={(e) => onFormDataChange('email', e.target.value)} className="mt-1 block w-full px-3 py-2 bg-white border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 text-gray-700" />
                </div>
                <button type="submit" className="w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500">Sign X.509 certificate</button>
            </form>
        </div>
    );
}
