import init, { main } from './pkg/wock_web.js';

const initPromise = init()

// window.document.c

const src = new URL(import.meta.url).searchParams.get("src")

if (src === null) {
  console.error("src query parameter not specified.")
}

await initPromise
await main(src)

