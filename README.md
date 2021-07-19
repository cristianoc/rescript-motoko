# mareo

Based on [https://github.com/cristianoc/mareo](), with a frontend written in [ReScript](https://rescript-lang.org) and backend written in [Motoko](https://sdk.dfinity.org/docs/language-guide/motoko.html).
The backend is used to load and save game state.


## Prerequisites

- `dfx` command line from [Internet Computer SDK](https://sdk.dfinity.org/docs/quickstart/local-quickstart.html#download-and-install)
- `node` installed

## Quick Start

```bash
cd rescript-motoko

# start the replica and build the backend
dfx start --background
dfx deploy backend

# start the frontend
npm install
npm run start
```

To iterate on the frontend code in `src/frontend` either use an editor extension with ReScript support such as [rescript-vscode](https://marketplace.visualstudio.com/items?itemName=chenglou92.rescript-vscode), or build directly with `npx rescript`.

To iterate on the backend code, edit `.mo` files in `src/backend`.
The interface between frontend and backend is in [src/frontend/src/Candid.res](src/frontend/src/Candid.res).

## Deploy both backend and frontend caisters

```bash
dfx deploy
npm run open
```
