import "@/styles/globals.css";
import {
    Address,
    Lucid,
    PublicKey,
} from "lucid-cardano";
import type { AppProps } from "next/app";
import {
    Dispatch,
    SetStateAction,
    createContext,
    useEffect,
    useState,
} from "react";

export type AppState = {
    openSSLPrivKey?: string;
    openSSLPubKey?: string;
    publicKey?: string;
    publicKeyHash?: string;
};

export const initialAppState: AppState = {};

export const AppStateContext = createContext<{
    appState: AppState;
    setAppState: Dispatch<SetStateAction<AppState>>;
}>({ appState: initialAppState, setAppState: () => {} });

export default function App({ Component, pageProps }: AppProps) {
    const [appState, setAppState] = useState<AppState>(initialAppState);
    const [isLoading, setIsLoading] = useState(true);

    useEffect(() => {
        const initializeLucid = async () => {
            const lucid = await Lucid.new();
            setAppState({ ...appState, lucid });
            setIsLoading(false); // Set loading to false once Lucid is initialized
        };
        initializeLucid();
    }, []);

    if (isLoading) {
        return <div>Loading...</div>;
    }

    return (
        <AppStateContext.Provider value={{ appState, setAppState }}>
            <Component {...pageProps} />
        </AppStateContext.Provider>
    );
}
