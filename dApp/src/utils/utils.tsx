export const processPemKey = (pemKey: string, prefix: string): string => {
    // Remove the first and last lines
    const base64Key = pemKey.split('\n').slice(1, -2).join('');
    // Base64-decode the key
    const rawKey = Buffer.from(base64Key, 'base64');
    // Convert to hex string
    let hexKey = rawKey.toString('hex');
    // Remove the specified prefix
    // const private-prefix = "302e020100300506032b657004220420";
    // const public-prefix = "302a300506032b6570032100";
    if (hexKey.startsWith(prefix)) {
        hexKey = hexKey.substring(prefix.length);
        return hexKey;
    } else {
        throw new Error('Invalid key format');
    }
};

export const callOpenSSL = async (requestType, inputData, errorMsg) => {
    try {
      const response = await fetch('/api/openssl', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ type: requestType, data: inputData}),
      });
      const data = await response.json();
      if (response.ok) {
        return data;
      } else {
        throw new Error( "Error calling API: " + errorMsg );
      }
    } catch (error) {
      // You could also rethrow the error to handle it outside, or handle it here
      throw error;
    }
};

export const getShortenedKey = (key:string) => {
    return `${key.substring(0, 4)}...${key.substring(key.length - 4)}`;
};

export const copyToClipboard = (data:string, msg:string) => {
  navigator.clipboard.writeText(data).then(() => {
    window.alert(msg);
  }, (err) => {
    console.error('Could not copy text: ', err);
  });
};